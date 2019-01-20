#!/usr/bin/env node

var Elm = require("./elm.js"),
    Promise = require("bluebird"),
    os = require("os"),
    fs = require("fs-extra"),
    path = require("path"),
    klaw = require("klaw"),
    through2 = require("through2"),
    crypto = require("crypto");

var app = Elm.Elm.Main.init({});

app.ports.toJS.subscribe(console.dir);
app.ports.allUnused.subscribe(printUnused);
app.ports.storeFile.subscribe(storeFile);
app.ports.showUsages.subscribe(showUsages);

if (process.argv.length > 2) {
    var arg = process.argv[2];
    if (arg == "--help" || arg == "-h") {
        showUsage();
    } else {
        findUsages(arg);
    }
} else {
    findUnused();
}

function showUsage() {
    console.log("Usage:");
    console.log("");
    console.log("    elm-xref");
    console.log("        Find unused functions");
    console.log("");
    console.log("    elm-xref Some.Module.function");
    console.log(
        "        Find where a function (or custom type constructor) is used"
    );
    console.log("");
}

function findUnused() {
    parseProject()
        .then(() => app.ports.fetch.send(null))
        .catch(console.error);
}

function findUsages(qualifiedFun) {
    var mod = qualifiedFun.split(".");
    var fun = mod.pop();

    parseProject()
        .then(() => app.ports.check.send([mod, fun]))
        .catch(console.error);
}

function parseProject() {
    return fs
        .readFile("elm.json")
        .then(data => JSON.parse(data))
        .then(info =>
            Promise.map(
                Object.entries(info.dependencies.direct),
                parsePackage
            ).then(() => info)
        )
        .then(info => Promise.map(info["source-directories"], parseSources));
}

function printUnused(unusedItems) {
    if (unusedItems.length == 0) {
        console.log("No unused functions or custom type constructors found!");
        process.exit(0);
    } else {
        console.log("Unused functions:");
        unusedItems.map(unused =>
            console.log(
                " - " +
                    unused[1][0] +
                    ":" +
                    unused[1][1].row +
                    ":" +
                    unused[1][1].column +
                    "\t" +
                    unused[0][0].join(".") +
                    "." +
                    unused[0][1]
            )
        );
        process.exit(1);
    }
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
    return parsePackageName(package)
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
        var fileName = path.join(pkgPath, "src", modulePath(moduleName));

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

function elmHomeDir() {
    if (process.env.ELM_HOME) {
        return path.normalize(process.env.ELM_HOME);
    } else {
        return path.join(os.homedir(), ".elm");
    }
}

function packagesRoot() {
    return path.join(elmHomeDir(), "0.19.0", "package");
}

function basePath(pkg) {
    return path.join(packagesRoot(), pkg.author, pkg.name, pkg.version);
}

function findExposedModules(pkg) {
    var elmJson = path.join(basePath(pkg), "elm.json");
    return fs.readFile(elmJson).then(data => {
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

        return Object.assign({}, pkg, {
            modules: modules
        });
    });
}

function packageToName(package) {
    return package.author + "/" + package.name;
}
