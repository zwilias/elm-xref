var Elm = require("./elm.js"),
    Promise = require("bluebird"),
    os = require("os"),
    fs = require("fs-extra"),
    path = require("path"),
    klaw = require("klaw"),
    semverSort = require("semver-sort"),
    through2 = require("through2"),
    crypto = require("crypto"),
    semver = require("semver"),
    compareVersions = require("compare-versions");

module.exports = {
    parseProject,
    findUnused,
    findUsages,
    initElm,
    parseAppPackage
};

function initElm(args = []) {
    var app = Elm.Elm.Main.init({ flags: args });

    app.ports.toJS.subscribe(console.error);
    app.ports.storeFile.subscribe(storeFile);

    return app;
}

function findUnused(app) {
    return new Promise(function(resolve, reject) {
        app.ports.allUnused.subscribe(resolve);
        app.ports.fetch.send(null);
    });
}

function findUsages(app, qualifiedFun) {
    return new Promise(function(resolve, reject) {
        var mod = qualifiedFun.split(".");
        var fun = mod.pop();

        app.ports.showUsages.subscribe(resolve);
        app.ports.check.send([mod, fun]);
    });
}

function parseProject() {
    return fs
        .readFile("elm.json")
        .then(data => JSON.parse(data))
        .then(info => {
            switch (info.type) {
                case "application":
                    return parseApplicationProject(info);
                case "package":
                    return parsePackageProject(info);
                default:
                    return Promise.reject("Unknown package type: " + info.type);
            }
        });
}

function parseApplicationProject(info) {
    var app = initElm();
    return Promise.map(
        Object.entries(info.dependencies.direct),
        parseAppPackage(info["elm-version"], app)
    )
        .then(() => Promise.map(info["source-directories"], parseSources(app)))
        .then(() => app);
}

function parsePackageProject(info) {
    var app = initElm(extractExposedModules(info));
    return Promise.map(Object.keys(info.dependencies), parsePackagePackage(app))
        .then(() => parseSources(app)("src"))
        .then(() => app);
}

function parseAppPackage(elmVersion, app) {
    return function(pkg) {
        return parsePackageName(elmVersion, pkg)
            .then(findExposedModules)
            .then(pkg =>
                Promise.map(pkg.modules, parsePackageModule(app, pkg))
            );
    };
}

function parsePackagePackage(app) {
    return function(packageName) {
        return resolvePackageVersion(packageName)
            .then(findExposedModules)
            .then(pkg =>
                Promise.map(pkg.modules, parsePackageModule(app, pkg))
            );
    };
}

function parseSources(app) {
    return function(sourceDirectory) {
        return findElmFiles(sourceDirectory).then(sources =>
            Promise.map(sources, parseSource(app))
        );
    };
}

function parseSource(app) {
    return function(modulePath) {
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
    };
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

function parsePackageModule(app, pkg) {
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

function resolvePackageVersion(packageName) {
    var parts = packageName.split("/");
    if (parts.length == 2) {
        return findLatestVersion(parts[0], parts[1]).then(
            ({ elmVersion, version }) => ({
                author: parts[0],
                name: parts[1],
                version,
                elmVersion
            })
        );
    } else {
        return Promise.reject("Invalid dependency name: " + packageName);
    }
}

function findLatestVersion(author, name) {
    return elmVersions()
        .then(elmVersions =>
            Promise.all(
                elmVersions.map(elmVersion =>
                    findLatestVersionsWithElmVersion(elmVersion)
                )
            )
        )
        .then(versions => {
            var vs = versions
                .sort((a, b) => compareVersions(a.version, b.version))
                .reverse();
            var version = vs.shift();
            if (version == undefined) {
                return Promise.reject(
                    "Found no candidate versions for package" +
                        author +
                        "/" +
                        name
                );
            } else {
                return version;
            }
        });
}

function findLatestVersionsWithElmVersion(elmVersion, author, name) {
    return Promise.resolve(path.join(packagesRoot(elmVersion), author, name))
        .then(fs.readdir)
        .then(data => data.filter(v => !(v == "." || v == "..")))
        .then(versions => semverSort.desc(versions))
        .then(versions => {
            version = versions.shift();
            if (version == undefined) {
                return [];
            }
            return [{ elmVersion, version }];
        })
        .catch(_ => []);
}

function parsePackageName(elmVersion, package) {
    var name = package[0];
    var parts = name.split("/");
    if (parts.length == 2) {
        return Promise.resolve({
            elmVersion: elmVersion,
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
    } else if (process.platform === "win32") {
        return path.resolve(os.homedir(), "AppData", "Roaming", "elm");
    } else {
        return path.join(os.homedir(), ".elm");
    }
}

function elmVersions() {
    return Promise.resolve(elmHomeDir())
        .then(fs.readdir)
        .then(data => data.filter(v => !(v == "." || v == "..")))
        .then(versions =>
            versions.filter(v => semver.satisfies(v, ">= 0.19.0"))
        )
        .then(versions => semverSort.desc(versions));
}

function packagesRoot(elmVersion) {
    var packageFolder = "packages";
    if (elmVersion == "0.19.0") {
        packageFolder = "package";
    }
    return path.join(elmHomeDir(), elmVersion, packageFolder);
}

function basePath(pkg) {
    return path.join(
        packagesRoot(pkg.elmVersion),
        pkg.author,
        pkg.name,
        pkg.version
    );
}

function findExposedModules(pkg) {
    var elmJson = path.join(basePath(pkg), "elm.json");
    return fs.readFile(elmJson).then(data => {
        var info = JSON.parse(data);
        return Object.assign({}, pkg, { modules: extractExposedModules(info) });
    });
}

function extractExposedModules(pkgInfo) {
    if (Array.isArray(pkgInfo["exposed-modules"])) {
        return pkgInfo["exposed-modules"];
    } else {
        return Object.values(pkgInfo["exposed-modules"]).reduce(
            (acc, ms) => acc.concat(ms),
            []
        );
    }
}

function packageToName(package) {
    return package.author + "/" + package.name;
}
