#!/usr/bin/env node

var Elm = require("./elm.js");
var Promise = require("bluebird");
var fs = require("fs-extra");
var klaw = require("klaw");
var through2 = require("through2");

var app = Elm.Main.worker();

app.ports.toJS.subscribe(console.dir);
app.ports.allUnused.subscribe(printUnused);

function printUnused(unusedItems) {
    console.log("Unused functions:");
    unusedItems.map(unused =>
        console.log(" - " + unused.modul.join(".") + "." + unused.fun)
    );
}

fs
    .readFile("elm-package.json")
    .then(data => JSON.parse(data))
    .then(info =>
        Promise.map(Object.keys(info.dependencies), parsePackage).then(
            () => info
        )
    )
    .then(info => Promise.map(info["source-directories"], parseSources))
    .then(() => app.ports.fetch.send(null));

function parsePackage(packageName) {
    return parsePackageName(packageName)
        .then(findVersion)
        .then(findExposedModules)
        .then(pkg => Promise.map(pkg.modules, parsePackageModule(pkg)));
}

function parseSources(sourceDirectory) {
    return findElmFiles(sourceDirectory).then(sources =>
        Promise.map(sources, parseSource)
    );
}

function parseSource(modulePath) {
    return fs.readFile(modulePath).then(data =>
        app.ports.toElm.send({
            fileName: modulePath,
            content: data.toString(),
            package: null
        })
    );
}

var excludeDirFilter = through2.obj(function(item, enc, next) {
    if (!item.stats.isDirectory()) {
        this.push(item);
    }
    next();
});

var excludeElmStuffFilter = through2.obj(function(item, enc, next) {
    if (!item.path.includes("elm-stuff")) {
        this.push(item);
    }
    next();
});

var excludeTestsFilter = through2.obj(function(item, enc, next) {
    if (!item.path.includes("tests")) {
        this.push(item);
    }
    next();
});

var onlyElmFilesFilter = through2.obj(function(item, enc, next) {
    if (item.path.endsWith(".elm")) {
        this.push(item);
    }
    next();
});

function findElmFiles(sourceDirectory) {
    return new Promise(function(resolve, reject) {
        var files = [];

        klaw(sourceDirectory)
            .pipe(excludeDirFilter)
            .pipe(excludeElmStuffFilter)
            .pipe(excludeTestsFilter)
            .pipe(onlyElmFilesFilter)
            .on("data", fileName => files.push(fileName.path))
            .on("end", () => resolve(files));
    });
}

function parsePackageModule(pkg) {
    var pkgName = packageToName(pkg);
    var pkgPath = basePath(pkg);

    return function(moduleName) {
        var fileNames = [
            pkgPath + "src/" + modulePath(moduleName),
            pkgPath + modulePath(moduleName)
        ];

        return Promise.any(
            fileNames.map(name =>
                fs.readFile(name).then(content =>
                    app.ports.toElm.send({
                        package: {
                            name: packageToName(pkg),
                            version: pkg.version
                        },
                        fileName: name,
                        content: content.toString()
                    })
                )
            )
        );
    };
}

function modulePath(name) {
    return name.split(".").join("/") + ".elm";
}

function parsePackageName(name) {
    var parts = name.split("/");
    if (parts.length == 2) {
        return Promise.resolve({ author: parts[0], name: parts[1] });
    } else {
        return Promise.reject("failed to parse " + name);
    }
}

function findVersion(pkg) {
    var versionsPath = "elm-stuff/packages/" + pkg.author + "/" + pkg.name;

    return fs
        .readdir(versionsPath)
        .then(data => data.filter(v => !(v == "." || v == "..")))
        .then(versions => {
            if (versions.length !== 1) {
                return Promise.reject(
                    "Found multiple candidate versions for " +
                        packageToName(pkg)
                );
            } else {
                return { ...pkg, version: versions[0] };
            }
        });
}
function basePath(pkg) {
    return (
        "elm-stuff/packages/" +
        pkg.author +
        "/" +
        pkg.name +
        "/" +
        pkg.version +
        "/"
    );
}

function findExposedModules(pkg) {
    return fs.readFile(basePath(pkg) + "elm-package.json").then(data => {
        var info = JSON.parse(data);
        return { ...pkg, modules: info["exposed-modules"] };
    });
}

function packageToName(package) {
    return package.author + "/" + package.name;
}
