# Haskell install instructions - ZuriHac 2020

### Table of contents

- [Prerequisites](#Prerequisites)
- [Linux / MacOS](#Linux-and-MacOS)
    - [Minimal setup](#minimal-setup)
    - [Fancy features](#Fancy-features)
- [Windows](#Windows)
    - [Minimal setup](#minimal-setup-1)
    - [Fancy features](#Fancy-features-1)

This guide that aims to be a simple way to get a modern Haskell toolchain up and running on your computer.

We describe only _one way to do it_, in order to simplify the process for beginners. The goal is to get started quickly and make it pleasant to learn the language we all love.

Of course there are other ways you can do set up a Haskell dev environment, so if you already have some experience setting up your favourite build system and editor plugins then do it your way.

At the end we will have the following:

- **[Visual Studio Code](https://code.visualstudio.com/)** with plugins that gives you features like:
  - type signatures and documentation on hover, autocomplete.
  - jump to definition, rename variable and function names.
  - error messages right in your editor.
- **ghc** - the Glasgow Haskell Compiler.
- **ghci**, a REPL to interactively type in code and explore.
- **[ghcid](https://github.com/ndmitchell/ghcid)** (optional) for monitoring your code for changes and automatically recompiling, giving you fast feedback and error messages.
- **[ghcide](https://github.com/digital-asset/ghcide)** for the IDE features in VS Code mentioned above.
- **cabal** to manage dependencies and build your project.
- On Linux and Mac: **[ghcup](https://www.haskell.org/ghcup/)** to manage GHC versions, and help install Cabal.
- On Windows: The **[Haskell Platform](https://www.haskell.org/platform/)**, which bundles GHC, Cabal and some tools.

If you just want to get started, then please read the [prerequisites](#Prerequisites), then just jump straight to the install instructions for your operating system:

- [Linux](#Linux)
- [MacOS](#MacOS)
- [Windows](#Windows)

### Note:

If you already have some programming experience, then developing Haskell might feel different. Generally we have a different approach to getting feedback on our code.

For example it's not common to litter our code with `print()` or `console.log()` statements, or set breakpoints, like you might do in other languages.

Instead:

- we use `ghcid` to watch our code for changes and recompile continuously, giving us success or error messages within a fraction of a second.
- types + purity give us more guarantees if the code compiles. So no more running the entire program to check if it works, just to find out that `calculate(ponies)` fails because `ponies` was `null`.
- we compose our programs out of smaller, often side-effect-free building blocks, which we can try out and test individually in the REPL.

So it might need some getting used to, but with the right tools the modern Haskell development experience is really nice!

## Prerequisites

Even in 2020 programming still requires some familiarity with the **command-line**. Haskell is no exception, so you should know how to use the **Terminal** on your computer. You don't need to be a command-line-ninja, but you'll be a lot more effective if you can change directories, copy/move files, list directory contents and a couple of other basic things.

If you're absolutely new to this, we recommend you take some time to familiarise yourself with the following:

- on MacOS: install and use [iTerm2](https://iterm2.com/).
- on Windows: use Powershell.
- on Linux: use whatever terminal your distro comes with.

The good news is that these are universal skills that help you be an effective programmer across _all languages_.


## Linux and MacOS

### Minimal setup

> Tested on Ubuntu 18.04 and MacOS Mojave

If the command below fails because you don't have `curl` installe, then consider using [Homebrew]([https://brew.sh/](<https://brew.sh/>)) on Mac, or `sudo apt install curl` on Ubuntu/Debian. If you're using another distro then I'm sure you know what to do.

On Ubuntu you might also need to install `libgmp` with `sudo apt install libgmp-dev`.

Install **ghcup** as per the [official instructions](https://www.haskell.org/ghcup/):

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

It will take a while to download GHC and Cabal, and you need to press Enter to confirm a couple of times. When it asks you to modify your `.bashrc`, enter YES and continue. If for some reason you missed that step, you can also just add the line `source ~/.ghcup/env` at the end of your `.bashrc` file in your home directory.

For the changes to take effect, either restart all your terminal windows, or type `source ~/.bashrc` in every one of them.

**That's it!** You now have Cabal 3 and a pretty recent version of GHC on your machine. You can see which one by typing `ghcup list`.

Play around in an interactive REPL with `cabal repl`.

Run a Hello World program by saving the following to a file called `Hi.hs`:

```haskell
module Hi where

main = putStrLn "hey"
```

Run it with `runghc Hi.hs`. You're all set!


### Fancy features

## Windows

### Minimal setup

> Tested on Windows 10.

- Install the Haskell Platform from https://www.haskell.org/platform/#windows
- See if your installation worked by either launching GHCi. That's your interactive prompt.
- To run single Haskell Source (.hs) files you can use `runghc` from the commandline.

Try it out with the following hello world program:

```haskell
module Hello where

main = putStrLn "sup"
```

Save it as `Hello.hs` somewhere, and execute `runghc Hello.hs` in the commandline.

> We recommend you use **Powershell** when you need to execute stuff in the commandline. If that's not included in your version of Windows, then you can use the normal **Command Prompt** (`cmd.exe`) as well.

### Fancy features

This allows you to set up projects with multiple source code files, external library dependencies, editor integration, etc.

#### 1

- Install Visual Studio Code from https://code.visualstudio.com/
- Launch VS Code, and install the **Haskell Syntax Highlighting** plugin. Open `Hello.hs` from before and admire the shiny colours.

#### 2

Next we install the latest version of Cabal. Right now the Haskell Platform uses 2.4.1.0. That's fine, but Cabal 3 has some major improvements, so let's upgrade:

- Download the binary for your type of machine (32 or 64 bit) https://www.haskell.org/cabal/download.html
- Copy the cabal executable inside the .zip to `C:\Program Files\Haskell Platform\8.6.5\lib\extralibs\bin`.
- Verify that you have the right version with `cabal --version`. Only  continue if this shows version 3.
- Execute `cabal update` to refresh its package index.
- Execute `cabal user-config update` to upgrade the version of Cabal's config file.

The last command also tells you _where_ the config is. For exampe mine is at `C:\Users\taylorswift\AppData\Roaming\cabal\config`. I strongly suggest you open it and add the following line:

```
write-ghc-environment-files: never
```

Later, for compiling some packages like `network`, this config file also needs to be fixed, in particular the lines `extra-lib-dirs`, `extra-prog-path`, etc. See the remarks below.

You can now create new projects with `cabal init` 🎉

#### 3

Final step: editor integration. The best way to do this in 2020 is to install **ghcide**:

```
cabal install ghcide --install-method=copy --overwrite-policy=always
```

This will compile for quite a while, so go get a coffee or make a new friend.

At the end you will have a new executable/command called `ghcid`.

For your information, the executables are installed in `C:\Users\your-username-here\AppData\Roaming\cabal\bin`, which Haskell Platform made sure to put in your Windows `%PATH%`.

Create a new project by doing the following in your commandline:

- `mkdir first-project`
- `cd first-project`
- `cabal init`

Open this folder in VS Code, and install the extension **ghcide** from the VS Code Marketplace. You're done.

How can you know this all worked? You will be able to see definitions when hovering with your mouse, you can introduce an error and see it highlighted red in the code, and you will get autocompletion.

You add dependencies in `first-project.cabal`, and you can run the code by just typing `cabal run`. The thing that will be executed is always your `main :: IO ()` function.

The rest you will learn in the beginner's course. Have fun!

### Some remarks about Cabal on Windows

**Skip this if you just want to set up your Haskell tooling**. This is more of an explanation why we did the setup the way described above.

The reason we are downloading a pre-compiled cabal 3 instead of doing the more canonical `cabal v2-install Cabal cabal-install` is because

- it works
- compiling from source takes a while
- the Cabal configuration on Windows currently needs some fixing.
- Cabal 2.4 doesn't have the flag `--install-method=copy`, so it can only try to symlink, which is not something Windows supports.

By default cabal currently produces the following config:

```
extra-include-dirs: C:\Program,
                    Files\Haskell,
                    Platform\8.6.5\mingw\include
extra-lib-dirs: C:\Program,Files\Haskell,Platform\8.6.5\mingw\lib
extra-prog-path: C:\Program Files\Haskell Platform\8.6.5\msys\usr\bin,
                 C:\Users\justinbieber\AppData\Roaming\cabal\bin
```

The line breaks and commas make no sense, and they prevent Cabal from seeing MinGW and MSYS, leading to an error when trying to compile packages with a `./configure` step like `network`.

It can be assumed that this will be fixed in the future, and moreover **Haskell Platform** will probably ship directly with Cabal 3 at some point.

## Other Editors

If you're eager to use something other than Visual Studio Code (vim, emacs, Sublime Text, Atom, etc), then the keyword to search for is "_Language Server Protocol_" (LSP). The [ghcide Readme](https://github.com/digital-asset/ghcide/blob/master/README.md) has instructions for a bunch of these. For emacs you might even consider [intero](https://github.com/chrisdone/intero).
