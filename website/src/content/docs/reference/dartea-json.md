---
title: dartea.json
description: The project file, for applications and for packages.
---

`dartea.json` sits at the root of a project. It says whether the project is an application or a package, where the code is, and what it depends on. If a folder has both `dartea.json` and `elm.json`, `dartea.json` wins.

## Application

```json
{
    "source-directories": ["src"],
    "dependencies": {
        "@someone/charts": "^1.2.0"
    }
}
```

| Field | Required | Meaning |
| --- | --- | --- |
| `type` | no | `"application"`, the default. |
| `source-directories` | no | Folders with your `.elm` files. Defaults to `["src"]`. Must not be empty. |
| `dependencies` | no | Packages and the versions you accept. |

## Package

```json
{
    "type": "package",
    "name": "@you/greeting",
    "version": "1.0.0",
    "summary": "Say hello",
    "license": "BSD-3-Clause",
    "exposed-modules": ["Greeting"],
    "dependencies": {}
}
```

| Field | Required | Meaning |
| --- | --- | --- |
| `type` | yes | `"package"`. |
| `name` | yes | The npm name, preferably under your own scope. |
| `version` | yes | `MAJOR.MINOR.PATCH`. |
| `summary` | yes | One line, shown on npm. |
| `license` | yes | An SPDX identifier, like `BSD-3-Clause` or `MIT`. |
| `exposed-modules` | yes | The modules other projects may import. Must not be empty. |
| `dependencies` | no | Packages and the versions you accept. |

The code of a package always lives in `src`.

## Versions

| Written | Accepts |
| --- | --- |
| `"1.2.0"` | exactly `1.2.0` |
| `"^1.2.0"` | `1.2.0` and newer, below `2.0.0` |

Other npm ranges, such as `~1.2.0` or `>=1.0.0`, are reported as an error instead of being guessed.

## dartea.lock

Written by `dartea install`. It holds the exact version of every package in the project, direct or not:

```json
{ "packages": { "@someone/charts": "1.2.3", "@someone/scales": "2.0.1" } }
```

Commit it. `dartea make` reads packages only through the lock, so every machine builds with the same code.
