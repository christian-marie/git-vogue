# git-vogue GitHub Pages

## Installing

```shell
cabal install
```

## Importing the git-vogue README

```shell
cabal exec readme-import
```

## Building & Running

```shell
cabal exec site clean && cabal exec site build && cabal exec site serve
```
