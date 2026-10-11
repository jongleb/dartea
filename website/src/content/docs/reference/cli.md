---
title: Command line
description: Every dartea command and option.
---

Run any command with `--help` for the same text in your terminal. With the npm package, prefix commands with `npx`.

## `dartea init`

```sh
dartea init [FOLDER]
```

Writes `dartea.json` and `src/Main.elm` with a small counter application, ready for `dartea make`. `FOLDER` defaults to the current one.

Refuses to run if the folder already has a `dartea.json` or an `elm.json`.

## `dartea make`

```sh
dartea make [TARGET] [--output=FILE]
```

Compiles Elm into JavaScript.

| Argument | Meaning |
| --- | --- |
| `TARGET` | A single `.elm` file to start from, or a source folder. Defaults to the current folder. |
| `--output=FILE` | Where to write the result. Defaults to `index.html`. |

The extension of `--output` decides what is produced:

| `--output` | Result |
| --- | --- |
| `index.html` | A page with the script inside |
| `app.js` | A script that defines the global `Dartea` |
| `/dev/null` | Nothing. Only type checks. |
| anything else | A folder with one `.mjs` file per module |

## `dartea install`

```sh
dartea install [PACKAGE]
```

Without `PACKAGE`: reads the dependencies from `dartea.json`, works out one version of every package that everyone agrees on, downloads them, and writes the versions it settled on into `dartea.lock`. If `dartea.lock` already satisfies `dartea.json`, nothing is resolved again.

With `PACKAGE`, like `@someone/charts`: finds its newest version and adds it to `dartea.json` as `^version`, but only if it fits with the rest of your dependencies.

## `dartea publish`

```sh
dartea publish [FOLDER] [--dry-run]
```

Publishes a package to npm. Checks that the package compiles and that every module in `exposed-modules` exists, then hands `dartea.json`, `src` and a generated `package.json` to `npm publish`. Log in with `npm login` first.

| Option | Meaning |
| --- | --- |
| `--dry-run` | Do everything except sending the package to npm. |

## Common options

| Option | Meaning |
| --- | --- |
| `--help` | Show help for a command. |
| `--version` | Show the version of dartea. |

Set `NO_COLOR` to turn off colours in error messages.
