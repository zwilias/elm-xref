var t = require("tap"),
    runner = require("../lib/runner.js");

t.test("Entry points are used", function(t) {
    return initParser()
        .then(
            parseContent(`
module Main exposing (main)
main = "hi there!"
`)
        )
        .then(runner.findUnused)
        .then(function(unused) {
            t.same(unused, []);
        });
});

t.test("Unused function is unused", function(t) {
    return initParser()
        .then(
            parseContent(`
module Main exposing (main)
unused = "not used!"
main = "this is used"
`)
        )
        .then(runner.findUnused)
        .then(function(unused) {
            t.notSame(unused, []);
        });
});

t.test("Constructor only used in self match is unused", function(t) {
    return initParser()
        .then(
            parseContent(`
module Main exposing (main)

type Foo = Foo | Bar

map : Foo -> Foo
map f =
  case f of
    Foo -> Foo
    Bar -> Bar

main = map Foo
`)
        )
        .then(runner.findUnused)
        .then(function(unused) {
            t.notSame(unused, []);
        });
});

t.test("Let declaration shadows imported module", function(t) {
    return initParser()
        .then(
            parseContent(
                `
module ToImport exposing (importedFn)

importedFn : String
importedFn = "hello"
`,
                "ToImport.elm"
            )
        )
        .then(
            parseContent(`
module Importer exposing (main)

import ToImport exposing (importedFn)

main =
  let importedFn = "shadow" in importedFn
`)
        )
        .then(runner.findUnused)
        .then(function(unused) {
            t.notSame(unused, []);
        });
});

function initParser() {
    var app = runner.initElm();

    return runner
        .parseAppPackage("0.19.0", app)(["elm/core", "1.0.0"])
        .then(() => app);
}

function parseContent(content, fileName = "Foo.elm") {
    return function(app) {
        app.ports.toElm.send({
            fileName,
            content,
            package: null
        });

        return app;
    };
}
