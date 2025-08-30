# The Craft of Functional Programming

This folder contains my solutions to all exercises from *The Craft of Functional Programming* by Simon Thompson.

It is not necessary to install the code provided for the book to work through the exercises, but should you want to do so, the instructions are provided below.

## Setup

First, make sure you have [GHCup](https://www.haskell.org/ghcup/install/) installed.

Then, to install the code provided for the book manually (file included in this repository), run:

```bash
tar -xvf Craft3e-2.0.0.tar.gz
cd Craft3e-2.0.0
cabal install
```

Alternatively, you can use:

```bash
cabal unpack Craft3e
cd Craft3e
cabal install
```

## Resources

- [Hackage page](https://hackage.haskell.org/package/Craft3e): installation instructions and package details.

- [Official website](https://www.haskellcraft.com/craft3e/Home.html): more information about the book and related resources.
