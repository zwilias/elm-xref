# elm-xref

> Cross-referencing tools for Elm. Find unused code, and figure out where
> specific functions are used.

## Installation

```shell
npm i -g elm-xref
```

## Usage

To index a project and find unused functions, execute `elm-xref` in the same
directory as your `elm-package.json`.

To find usages of a function, specify the fully qualified function name as an
argument.

Examples on [rtfeldman/elm-spa-example@c8c3201](https://github.com/rtfeldman/elm-spa-example/tree/c8c3201ec0488f17c1245e1fd2293ba5bc0748d5):

```shell
# To find unused functions:
$ elm-xref
Unused functions:
 - /Users/ilias/Src/elm/elm-spa-example/src/Api.elm:294:1       Api.cacheStorageKey
 - /Users/ilias/Src/elm/elm-spa-example/src/Api.elm:299:1       Api.credStorageKey
 - /Users/ilias/Src/elm/elm-spa-example/src/Api.elm:73:1        Api.decode
 - /Users/ilias/Src/elm/elm-spa-example/src/Author.elm:120:1    Author.follow
 - /Users/ilias/Src/elm/elm-spa-example/src/Author.elm:125:1    Author.unfollow
 - /Users/ilias/Src/elm/elm-spa-example/src/Avatar.elm:55:1     Avatar.toMaybeString
 - /Users/ilias/Src/elm/elm-spa-example/src/Email.elm:30:7      Email.Email
 - /Users/ilias/Src/elm/elm-spa-example/src/Email.elm:44:1      Email.decoder
 - /Users/ilias/Src/elm/elm-spa-example/src/Email.elm:39:1      Email.encode
 - /Users/ilias/Src/elm/elm-spa-example/src/Email.elm:34:1      Email.toString
 - /Users/ilias/Src/elm/elm-spa-example/src/Main.elm:112:7      Main.ChangedRoute
 - /Users/ilias/Src/elm/elm-spa-example/src/Page.elm:26:7       Page.Login
 - /Users/ilias/Src/elm/elm-spa-example/src/Page.elm:27:7       Page.Register
 - /Users/ilias/Src/elm/elm-spa-example/src/Page.elm:28:7       Page.Settings
 - /Users/ilias/Src/elm/elm-spa-example/src/Page/Settings.elm:90:7      Page.Settings.Valid
 - /Users/ilias/Src/elm/elm-spa-example/src/Page/Settings.elm:456:1     Page.Settings.nothingIfEmpty
 - /Users/ilias/Src/elm/elm-spa-example/src/Route.elm:19:7      Route.Root

# To find usages of `Maybe.Just`:
$ elm-xref Maybe.Just
Usages:
 - Api.storeCredWith (113)
 - Article.faveDecoder (231)
 - Article.favorite (221)
 - Article.Comment.post (80, 81)
 - Article.Slug.urlParser (22)
 - Author.followDecoder (145)
 - Author.requestFollow (132)
 - Main.changeRouteTo (179)
 - Page.Article.Editor.create (535, 536)
 - Page.Article.Editor.edit (560)
 - Page.Article.Editor.getSlug (583, 586, 589, 592, 595)
 - Page.Settings.nothingIfEmpty (462)
 - Session.cred (42)
 - Session.viewer (32)
 - Username.urlParser (43)
```

Value constructors of types are also tracked. Note that only constructing values
is tracked as usage - pattern matches and destructuring are _not_ tracked as
usage.

Usage tracking isn't terribly clever. This tool only does static analysis and
does not attempt to evaluate any expressions. As a result, functions that only
appear in "obviously unreachable code" (i.e. conditionals that always evaluate
to `False`) do very much count as usage.

## Roadmap

- [x] Show unused functions
- [x] Find usages
- [x] Support union type constructors
- [x] Show more detail about usages (line/context)
- [ ] Support packages (count exposed declarations from exposed modules as used)
- [ ] Support whitelisting
- [ ] Show unused parameters
- [ ] Support type aliases (as function and in type signatures)
- [ ] Show file/line(/col?) links for usages and definitions


## License

See the `LICENSE` file.
