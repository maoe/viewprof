# viewprof
[![Hackage](https://img.shields.io/hackage/v/viewprof.svg)](https://hackage.haskell.org/package/viewprof)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/viewprof.svg)](http://packdeps.haskellers.com/feed?needle=viewprof)
[![Stackage LTS](http://stackage.org/package/viewprof/badge/lts)](http://stackage.org/lts/package/viewprof)
[![Stackage Nightly](http://stackage.org/package/viewprof/badge/nightly)](http://stackage.org/nightly/package/viewprof)
[![Build Status](https://travis-ci.org/maoe/viewprof.svg?branch=master)](https://travis-ci.org/maoe/viewprof)

viewprof is a text-based interactive GHC .prof viewer.

![screenshot](img/screenshot.png)

It has three display modes:

* __Aggregate cost centers view__: This is the default view. It groups cost centers by their name and module name, like the middle section of .prof files.
* __Call sites view__: If you press enter on a cost center, viewprof displays call sites of the cost center you selected. This view tells how much the cost center spent for each call site.
* __Modules view__: If you press M, viewprof displays the module level breakdown. This view tells coarse overview of cost attribution.

## Installation

Note: Currently viewprof doesn't support Windows because the underlying library (vty) doesn't support it yet. See [#1](https://github.com/maoe/viewprof/issues/1).

```
stack install viewprof
```

## Usage

| keys              | action                                      |
|-------------------|---------------------------------------------|
| `q` or `escape`   | quit the current view                       |
| `j` or `↓`        | move focus down     　 　　　　　　           |
| `k` or `↑`        | move focus up         　　　　　　　          |
| `gg`              | move focus to the top                       |
| `G`               | move focus to the bottom                    |
| `C`               | display aggregate cost center view          |
| `M`               | switch to module breakdown                  |
| `enter`           | select a cost center and display call sites |
| `t`               | sort by time                                |
| `a`               | sort by allocation                          |
| `e`               | sort by # of entries                        |
| `h` or `?`        | show key bindings                           |
| `i`               | show profile information                    |

## Acknowledgement

`viewprof` was originally meant to be a Haskell port of [mkotha/viewprof](https://github.com/mkotha/viewprof), which is a text-based .prof viewer written in Common Lisp.
