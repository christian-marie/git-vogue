git-vogue
=========

*git-vogue* helps developers to add git hooks to their Haskell repository, to ensure quality of their code.

It defines a list of commands to check code quality, paired with another command to fix problems that arise from that check. *git-vogue* will set up your repository to run all command pairs on commit, giving instant feedback on the quality of code, and helping fix issues more quickly.

Currently, *git-vogue* supports [hlint][1] and [Stylish Haskell][2].

[1]: http://hackage.haskell.org/package/hlint
[2]: https://hackage.haskell.org/package/stylish-haskell

Installation
------------

(The usage details below are subject to change until *git-vogue* is stabilised and packaged.)

*git-vogue* can be found on Hackage (link TBA) and can be installed with Cabal.

```bash
cabal install git-vogue
```

Usage
-----

Once installed, invoking is as simple as:

```bash
git vogue
```

By being integrated as a git command option, *git-vogue* is now seamlessly integrated into your git workflow.
