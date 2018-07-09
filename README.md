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

Example:

```shell
# To find unused functions:
$ elm-xref

# To find usages of `Maybe.map`:
$ elm-xref Maybe.map
```

## License

See the `LICENSE` file.
