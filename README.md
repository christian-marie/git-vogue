git-vogue - A framework for pre-commit checks 
=========================================================

[![Travis Status](http://travis-ci.org/anchor/git-vogue.png)](https://travis-ci.org/anchor/git-vogue)

Intended to be used as a git pre-commit hook, *git-vogue* encourages developers
to keep their Haskell code ["en vogue"][1] by providing a framework for code
quality checking plugins and some supporting plugins.

Currently, *git-vogue* ships with the following plugins:

* [cabal][6]
* [hlint][2]
* [Stylish Haskell][4] + automatic fixing
* [ghc-mod][5]


[1]: https://www.youtube.com/watch?v=GuJQSAiODqI
[2]: http://hackage.haskell.org/package/hlint
[4]: https://hackage.haskell.org/package/stylish-haskell
[5]: https://hackage.haskell.org/package/ghc-mod
[6]: https://hackage.haskell.org/package/Cabal

Quickstart
------------

```bash
cabal install git-vogue
```

If you wish to set up pre-commit hooks (recommended):

```bash
git vogue init
```

With pre-commit hooks set up, `git vogue check` will be run before every
commit. If you wish to check the whole repository, run `git vogue check --all`.

You can attempt to automatically rectify any problems discovered via `git vogue
fix` and `git vogue fix --all`. The only plugin that currently supports this
auto-fixing is stylish haskell.

# Plugin discovery/disabling

Running `git-vogue plugins` will show you the libexec directory in which
git-vogue will discover plugins.

Should one or more plugins annoy you, you may disable it by setting it
non-executable:

```bash
chmod -x .cabal/libexec/git-vogue/git-vogue-stylish
```

A more sophisticated interface to plugin manipulation is planned.

# Plugins

## cabal

Checks your .cabal file for packaging problems. Can not fix problems
automatically.

## hlint

Checks .hs files for linting issues, respects HLint.hs in the top level of the
repository. Can not fix problems automatically.

## ghc-mod

Checks .hs files (excluding HLint.hs and Setup.hs) as per ghc-mod check.
ghc-mod can be tempremental, so if this fails to run the plugin will allow the
commit to pass. Can not fix problems automatically.

## stylish haskell

Checks if .hs files would have been modified by stylish-haskell. Respects
.stylish-haskell.yaml. Fixes problems automatically.

Uninstalling 
------------

To remove a `git-vogue init` configured pre-commit hook, run:

```bash
rm .git/hooks/pre-commit
```

[Here are instructions](https://www.youtube.com/watch?v=4qXD5l-ZlfA) for
uninstalling a cabal package.



Philosophy
---------

At Anchor Engineering, we're pretty un-dogmatic about using one editor or even
one OS for development. As such, we've found ourselves in need of a common
benchmark for linting, formatting and code quality checks.

We wanted a tool that would:

* Install in one command
* Require nothing to be learned for a new developer
* Tell that developer what they need to fix in the code that they modify and
  that code only.

git-vogue aims to satisfy these needs, whilst saving developers from the
drudgery of installing, configuring and running each of these tools
independently.

Plugin specification
-------------------

**The interface** for an executable (to be called by git-vogue) is a single
command line argument, one of:

* check
* fix
* name

The plugin can assume that the CWD will be set to the top-level directory of
the package.

The plugin will receive a list of all files in the current repository that may
be looked at via STDIN when running in "check" or "fix" mode. These file paths
will be absolute and newline separated. The plugin is expected to filter them
appropriate to its needs.

**Invariants for well-behaved plugin commands**

* `name` will return a human-readable name one line
* `check` will not modify any files
* `check` will exit with a return code of:
    * No errors - 0
    * Errors need fixing - 1
    * Catastrophic failure to check - 2
* `fix` is idempotent
* `fix` will exit with a return code of:
    * The code is now good (changes may or may not have been made) - 0
    * Some errors remain - 1
    * Catastrophic failure to check - 2
* If `fix` returns "success" (return code 0), `check` must no longer fail
