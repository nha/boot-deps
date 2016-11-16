[![CircleCI](https://circleci.com/gh/nha/boot-deps.svg?style=svg)](https://circleci.com/gh/nha/boot-deps)

# boot-deps

boot tasks for dealing with dependencies

## Install

[![Clojars Project](https://img.shields.io/clojars/v/nha/boot-deps.svg)](https://clojars.org/nha/boot-deps)

# Usage

## Check-conflicts

Task that throws an exception if your dependencies have conflicts (ie. different packages require different versions of another package):

In a task:
```
(deftask dev [] (comp (nha.boot-deps/check-conflicts)) ... )

```

From code:
```
(boot.core/boot (nha.boot-deps/check-conflicts))

```

## Write-resolved

Writes a conflict-free dependency edn file if your project has conflicts.
The strategy used to resolve conflicts is to always keep the most recent project.

From code:

```
  (boot.core/boot (nha.boot-deps/write-resolved :filepath "conflict-free-deps.edn"))

```
