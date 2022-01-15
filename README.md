# DCC019-LP-Trabalho2
Repository containing the source code presented on the second task of the Programming Languages (Linguagens de Programação) course

## Nim game

The game is played by two players. In this program, the game is configured to be a board with 4 lines, each line has its own stickers and the player has to choose one line and take between 1 and all stickers from it each turn. The initial board starts with 1, 3, 5 & 7 stickers on its 4 lines. The player who takes the last sticker, wins.

There are two levels of difficulty: easy and hard. On the easy mode, the machine makes random moves. On the hard mode, the machine tries to keep making only perfect moves, if they are not possible, then it fallbacks to a random move to continue the dynamic.

### Demo

![](https://github.com/oliveiraleo/DCC019-LP-Trabalho2/blob/main/nim-quick-demo.GIF)

 For more information about my implementation, please refer to the option 9 of the game menu and/or my source code above. General information can be found at [Wikipedia](https://en.wikipedia.org/wiki/Nim).

## Requirements

- A GNU/Linux distribution
- The [GHC](https://www.haskell.org/ghc/) compiler
- Any text editor (for visualization or editing)

### Other requirements

- For running the program correctly, the packet that provides the `random` module is needed.

#### **=> Method 1** (longer but it's tested working)

1- Install the `cabal` packet from your distro repo, for e.g.*:

```
sudo pacman -S cabal
```

\* **Note:** In a [Arch](https://archlinux.org/)-based distro `pacman -S` is the equivalent of `apt install` from the Debian-based distros

2- Update cabal's package database:

```
cabal update
```

3- Install the `random` package using cabal:

```
cabal install random
```

4- If GHC was already running, please restart it (i.e. close and reopen)

#### **=> Method 2** (easier but use at your own risk)

1- Install the `monadrandom` (or an [equivalent](https://packages.ubuntu.com/search?suite=impish&section=all&arch=any&keywords=monadrandom&searchon=names)) packet from your distro repo, for e.g.:

```
sudo pacman -S haskell-monadrandom
```

\* **Note:** In a [Arch](https://archlinux.org/)-based distro `pacman -S` is the equivalent of `apt install` from the Debian-based distros

2- If GHC was already running, please restart it (i.e. close and reopen)

## Running the program

\- Clone the repo with:

```
git clone https://github.com/oliveiraleo/DCC019-LP-Trabalho2.git
```

\- Enter inside the project folder:

```
cd DCC019-LP-Trabalho2/
```

\- Load the source code inside the compiler, for eg. using:

```
ghci main.hs
```
\- Load the game:

```
ghci> main
```

\- Then follow the on screen instructions

## License

This program is licensed under [The MIT License](https://opensource.org/licenses/MIT)