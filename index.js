#!/usr/bin/env node

var Elm = require("./elm.js");
var Promise = require("bluebird");
var os = require("os");
var fs = require("fs-extra");
var klaw = require("klaw");
var through2 = require("through2");
var crypto = require("crypto");

var app = Elm.Elm.Main.init({});

app.ports.toJS.subscribe(console.dir);
app.ports.allUnused.subscribe(printUnused);
app.ports.storeFile.subscribe(storeFile);
app.ports.showUsages.subscribe(showUsages);

fs
    .readFile("elm.json")
    .then(data => JSON.parse(data))
    .then(info =>
        Promise.map(
            Object.entries(info.dependencies.direct),
            parsePackage
        ).then(() => {
            console.log("done with deps");
            return info;
        })
    )
    .then(info => Promise.map(info["source-directories"], parseSources))
    .then(() => {
        if (process.argv.length > 2) {
            var mod = process.argv[2].split(".");
            var fun = mod.pop();

            app.ports.check.send([mod, fun]);
        } else {
            app.ports.fetch.send(null);
        }
    })
    .catch(console.error);

function printUnused(unusedItems) {
    console.log("");
    console.log("Unused functions:");
    unusedItems.map(unused =>
        console.log(" - " + unused[0].join(".") + "." + unused[1])
    );
    console.log("");
}

function showUsages(usages) {
    console.log("");
    console.log("Usages:");
    usages.map(usage =>
        console.log(
            " - " +
                usage[0][0].join(".") +
                "." +
                usage[0][1] +
                " (" +
                usage[1].join(", ") +
                ")"
        )
    );
    console.log("");
}

function parsePackage(package) {
    console.log("Parsing dependency", package);
    return parsePackageName(package)
        .then(findExposedModules)
        .then(pkg => Promise.map(pkg.modules, parsePackageModule(pkg)));
}

function parseSources(sourceDirectory) {
    console.log("Parsing sources in", sourceDirectory);
    return findElmFiles(sourceDirectory).then(sources =>
        Promise.map(sources, parseSource)
    );
}

function parseSource(modulePath) {
    return fs.readFile(modulePath).then(data =>
        readCache(hash(data))
            .then(cachedData =>
                app.ports.restore.send({
                    fileName: modulePath,
                    package: null,
                    data: cachedData
                })
            )
            .catch(() =>
                app.ports.toElm.send({
                    fileName: modulePath,
                    content: data.toString(),
                    package: null
                })
            )
    );
}

function readCache(hash) {
    return fs.readJson("elm-stuff/xref/cache/" + hash + ".json");
}

function hash(content) {
    return crypto
        .createHash("md5")
        .update(content)
        .digest("hex");
}

function storeFile(fileInfo) {
    return fs
        .ensureDir("elm-stuff/xref/cache")
        .then(() =>
            fs.writeJson(
                "elm-stuff/xref/cache/" + hash(fileInfo.content) + ".json",
                fileInfo.data
            )
        );
}

function findElmFiles(sourceDirectory) {
    return new Promise(function(resolve, reject) {
        var files = [];

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

        klaw(sourceDirectory)
            .pipe(excludeDirFilter)
            .pipe(excludeElmStuffFilter)
            .pipe(excludeTestsFilter)
            .pipe(onlyElmFilesFilter)
            .on("data", fileName => {
                if (files.indexOf(fileName.path) == -1) {
                    files.push(fileName.path);
                }
            })
            .on("end", () => resolve(files));
    });
}

function parsePackageModule(pkg) {
    var pkgName = packageToName(pkg);
    var pkgPath = basePath(pkg);

    return function(moduleName) {
        var fileName = pkgPath + "src/" + modulePath(moduleName);

        return fs.readFile(fileName).then(content =>
            readCache(hash(content))
                .then(data =>
                    app.ports.restore.send({
                        fileName: name,
                        package: {
                            name: packageToName(pkg),
                            version: pkg.version
                        },
                        data
                    })
                )
                .catch(() =>
                    app.ports.toElm.send({
                        package: {
                            name: packageToName(pkg),
                            version: pkg.version
                        },
                        fileName: fileName,
                        content: content.toString()
                    })
                )
        );
    };
}

function modulePath(name) {
    return name.split(".").join("/") + ".elm";
}

function parsePackageName(package) {
    var name = package[0];
    var parts = name.split("/");
    if (parts.length == 2) {
        return Promise.resolve({
            author: parts[0],
            name: parts[1],
            version: package[1]
        });
    } else {
        return Promise.reject("failed to parse " + name);
    }
}

function basePath(pkg) {
    return (
        os.homedir() +
        "/.elm/0.19.0/package/" +
        pkg.author +
        "/" +
        pkg.name +
        "/" +
        pkg.version +
        "/"
    );
}

function findExposedModules(pkg) {
    console.log("Finding exposed modules", pkg);
    return fs.readFile(basePath(pkg) + "elm.json").then(data => {
        var info = JSON.parse(data);
        var modules;
        if (Array.isArray(info["exposed-modules"])) {
            modules = info["exposed-modules"];
        } else {
            modules = Object.values(info["exposed-modules"]).reduce(
                (acc, ms) => acc.concat(ms),
                []
            );
        }

        console.log("Found modules", modules);
        return Object.assign({}, pkg, {
            modules: modules
        });
    });
}

function packageToName(package) {
    return package.author + "/" + package.name;
}
