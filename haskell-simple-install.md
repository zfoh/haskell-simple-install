# ZuriHac 2020 Haskell install instructions

This guide that aims to be a simple way to get a modern Haskell toolchain up and running on your computer.

We describe only _"one way to do it"_, in order to simplify the process for beginners. The goal is to get started **fast** and make it **pleasant** to learn the language we all love.

Of course there are other ways you can do set up a Haskell dev environment, so if you already have some experience setting up your favourite build system and editor plugins then this guide might not be for you.

At the end we will have the following:

- **[Visual Studio Code](https://code.visualstudio.com/)** with a plugin that gives you all the modern conveniences, like:
  - type signatures and documentation on hover.
  - autocomplete.
  - jump to definition, rename variable and function names, etc.
- **ghc** - the Glasgow Haskell Compiler.
- **ghci**, a REPL to interactively type in code and explore.
- **ghcid** for monitoring your code for changes and automatically recompiling, giving you blazing fast feedback and error messages.
- **ghcide** for the IDE features in VSCode mentioned above.
- **cabal** to manage dependencies and build your project.
- (tbd: ghcup?)

If you already have some programming experience, then developing Haskell will feel slightly different, because the better compiler guarantees give us a different approach to getting fast feedback on what our code does.

For example it's not common to litter our code with `print` statements, or set breakpoints, like you might do in other languages.

But rest assured, the modern Haskell IDE experience has gotten extremly nice!


## prerequisites

## Linux

## MacOS

## Windows

### Minimal setup

This was tested on Windows 10.

- Install the Haskell Platform from https://www.haskell.org/platform/#windows
- See if your installation worked by either launching GHCi. That's your interactive prompt.
- To run single Haskell Source (.hs) files you can use `runghc` from the commandline.

Try it out with the following hello world program:

```
module Hello where

main = putStrLn "sup"
```

Save it as `Hello.hs` somewhere, and execute `runghc Hello.hs` in the commandline.

> We recommend you use **Powershell** when you need to execute stuff in the commandline. If that's not included in your version of Windows, then you can use the normal **Command Prompt** (`cmd.exe`) as well.

### More features setup

This allows you to set up projects with multiple source code files, external library dependencies, editor integration, etc.

- Install Visual Studio Code from https://code.visualstudio.com/
- Launch VScode, and install the **Haskell Syntax Highlighting** plugin. Open `Hello.hs` from before and admire the shiny colours.

Now we'll get the latest version of Cabal. Right now the Haskell Platform uses 2.4.1.0. That's fine, but Cabal 3 has some major improvements, so let's upgrade.

- Download the binary for your type of machine (32 or 64 bit) https://www.haskell.org/cabal/download.html
- There are at least two places that binaries can go into.
- Copy the cabal executable inside the .zip to `C:\Program Files\Haskell Platform\8.6.5\lib\extralibs\bin`.
- `C:\Users\your-username-here\AppData\Roaming\cabal\bin`
- `C:\Program Files\Haskell Platform\8.6.5\lib\extralibs\bin`
- `cabal user-config update`

- Update **Cabal** (your tool for managing dependencies and building projects) to the latest version. Execute these in the commandline:
    - `cabal v2-update`.
    - **Before** you run the next step, you currently need to fix something in the Cabal configuration. Please follow the instructions [below](#Cabal-on-Windows-config-adjustment).
    - `cabal v2-install Cabal cabal-install`. Get a coffee while it compiles, or go make a new friend.
    - verify that you have version 3: `cabal --version`.
    - you can now create a new project folder with `cabal init` 🎉
- Install 

#### Cabal on Windows config adjustment

Right now it's good to make some small adjustments to the Cabal config file.

Find out where it lives with `cabal user-config init` and open it in your editor of choice.

## Alternatives

- vim
- intero (?)
- repl.it
- stack instead of cabal
