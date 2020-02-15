# ZuriHac 2020 Haskell install instructions

This is a guide that aims to be the simplest way to get a modern Haskell toolchain up and running on your computer.

This guide was created for ZuriHac 2020. We describe only _"one way to do it"_, in order to simplify the process for beginners and help them start writing Haskell as fast as possible.

Of course there are other ways you can do set up a Haskell dev environment, so if you already have some experience setting up your favourite build system and editor plugins then this guide might not be for you.

At the end we will have the following:

- **[Visual Studio Code](https://code.visualstudio.com/)** with a plugin that gives you all the modern conveniences like:
  - type signatures and documentation on hover.
  - autocomplete.
  - jump to definition, rename variable and function names, etc.
- **ghc** - the Glasgow Haskell Compiler.
- **ghci** for REPL to interactively type in code and explore.
- **ghcid** for monitoring your code for changes and automatically recompiling, giving you blazing fast feedback and error messages.
- **ghcide** for the IDE features in VSCode mentioned above.
- **cabal** to manage dependencies and build your project.
- (tbd: ghcup?)

If you already have some programming experience, then developing Haskell will feel slightly different, because the better compiler guarantees give us a different approach to getting fast feedback on what our code does.

For example it's not common to litter our code with `print` statements, or set breakpoints, like you might do in other languages.

But rest assured, the modern Haskell IDE experience has gotten extremly nice!


## pre requisites

## Linux

## MacOS

## Windows

## Alternatives

- vim
- intero (?)
- repl.it
- stack instead of cabal
