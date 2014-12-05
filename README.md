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

Rationale
---------

At Anchor Systems, we've been working with Haskell for some time, and we've encountered some interesting problems along the way. A lot of these problems have to do with *code readiness* –- code that is not only ready to run and deploy, but also ready to pass on to other developers so they can maintain and update it, and also give to end users so they can use it.

### Formatting

Nobody formats their code quite the same way, making it difficult for other developers to pick up other people's code and read or change it.

It's kind of hard to work with other people's code when you're using tabs and they're not, or when your tabbing is different, or you delimit method type definitions differently from everyone else. Therefore, to make things easier for everyone (including yourself), we've set up *git-vogue* to check if your code conforms to a given format. If it doesn't, it will stop the commit and recommend changes.

### Comments

Nobody comments their code enough to generate decent documentation for it. In fact, sometimes it's difficult to tell what a given method does.  

For example, what we have below doesn't tell us enough about what it does.

```haskell
doAThing :: Int -> Int -> Int -> Int -> Int -> Int -> IO Frob
doAThing a b c d e f = Frob <$> (sequence . map foop $ [a b c d e f])
```

Something like this, on the other hand, gives us enough information to be able to tell what this method actually does and how to work with it.

```haskell
-- | Performs a foop transformation on all identifiers of the Royal Family,
-- and packs it into a Frob for use in IO contexts.
doAThing :: Int -- ^ Index of the Empress
         -> Int -- ^ Index of the High Consort
         -> Int -- ^ Index of the First Daughter
         -> Int -- ^ Index of the First Son
         -> Int -- ^ Index of the Distant Relative
         -> Int -- ^ Index of the Fool
         -> IO Frob
doAThing a b c d e f = Frob <$> (sequence . map foop $ [a b c d e f])
```

Using *git-vogue*, we can determine if given methods are commented in sufficient detail, and if any aren't, it alerts the user and stops the code from being committed.

### Testing

Tests are an indispensable part of writing good software, but they're not very useful unless you remember to run them.

*git-vogue* uses Cabal to run your program's test suite before it's committed to make sure that your code is tested, not just before it can be pushed to the repository, but that it's tested regularly as you work.

### "Niceness"

In any given Haskell program, it's safe to say that there is probably a more concise way to do particular things, if only you knew about them.  `hlint` is a good tool for helping to find ways to optimise your syntax, and *git-vogue* makes use of it.

For example, you might have a line somewhere like:

```haskell
putStrLn . show $ foo
```

This can be condensed down into the following:

```haskell
print foo
```

*git-vogue* will help find these optimisations before you commit your code, enabling you to not just make your code more concise, but also teach you new ways of working so you can become a better developer.

Design Philosophy
-----------------



