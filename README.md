git-vogue
=========

[![Travis Status](http://travis-ci.org/anchor/git-vogue.png)](https://travis-ci.org/anchor/git-vogue)

*git-vogue* assists developers to keep their Haskell code ["in vogue"][1].

This package defines a list of commands to check code quality, comments, and formatting continually. *git-vogue* will set up your repository to run all command pairs on commit, giving instant feedback on the quality of code, and helping fix issues more quickly.

Currently, *git-vogue* supports [hlint][2], [hspec][3] and [Stylish Haskell][4].

[1]: https://www.youtube.com/watch?v=GuJQSAiODqI
[2]: http://hackage.haskell.org/package/hlint
[3]: https://hackage.haskell.org/package/hspec
[4]: https://hackage.haskell.org/package/stylish-haskell

Installation
------------

*git-vogue* can be found on Hackage (link TBA) and can be installed with Cabal.

```bash
cabal install git-vogue
```

To apply it to your repository, invoke:

```bash
git vogue init
```

This one time command will setup the git hooks and other requirements to latch itself onto your repository. Once installed, *git-vogue*'s functionality is seamlessly integrated into your git workflow, making it an intrinsic part of your development cycle.

Rationale
---------

At Anchor Engineering, we've been working with Haskell for some time, and we've encountered some interesting problems along the way. A lot of these problems have to do with *code readiness*: code that is not only ready to run and deploy, but also ready to pass on to other developers so they can maintain and update it.

### Formatting

Nobody formats their code quite the same way, making it difficult for developers to pick up each other's code and read or change it. Differences in spaces verses tabs, or how methods are delimited can cause issues with a lack of standardization.

To make things easier for everyone (including yourself), we've set up *git-vogue* to use *Stylish Haskell* to check if your code conforms to a given format. If it doesn't, your commit will not be allowed to be pushed upstream, and you will instead receive a number of recommeded changes.

Design Philosophy
-----------------

**The interface** is a single named subcommand, one of:

* check
* fix
* name

The plugin can assume that it will be running in the top-level directory of the package.

**Rules for each subcommand**

* `check` shall not modify any files
* `check` may have various return values:
    * No errors - return code 0
    * Errors need fixing - return code 1
    * Catastrophic failure to check - return code 2
* `fix` is idempotent
* `fix` may have various return values:
    * The code is now good (changes may or may not have been made) - return code 0
    * Some errors remain - return code 1
    * Catastrophic failure to check - return code 2
* If `fix` returns "success" (return code 0), `check` must no longer fail
