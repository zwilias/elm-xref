var t = require("tap"),
    runner = require("../lib/runner.js");

t.test("Entry points are used", function(t) {
    return parseContent(`
module Main exposing (main)
main = "hi there!"
`).then(function(unused) {
        t.same(unused, []);
    });
});

t.test("Unused function is unused", function(t) {
    return parseContent(`
module Main exposing (main)
unused = "not used!"
main = "this is used"
`).then(function(unused) {
        t.notSame(unused, []);
    });
});

t.test("Constructor only used in self match is unused", function(t) {
    return parseContent(`
module Main exposing (main)

type Foo = Foo | Bar

map : Foo -> Foo
map f =
  case f of
    Foo -> Foo
    Bar -> Bar

main = map Foo
`).then(function(unused) {
        t.notSame(unused, []);
    });
});

function parseContent(content) {
    var app = runner.initElm(["Main"]);

    return runner
        .parseAppPackage(app)(["elm/core", "1.0.0"])
        .then(function() {
            app.ports.toElm.send({
                fileName: "Foo.elm",
                content: content,
                package: null
            });
            return app;
        })
        .then(runner.findUnused);
}
