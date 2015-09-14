This is still in "closed" development (that is, I don't think you can
make sense of this repo).

If you have interest in more information (like a real README), contact me.



# Installation (Linux)

## Prerequisites

### GHC and Cabal

Install GHC 7.10.2 (older version are untested, 7.6.* is known) and Cabal. For
example:

* Download http://downloads.haskell.org/~ghc/7.10.2/ghc-7.10.2-x86_64-unknown-linux-deb7.tar.xz from https://www.haskell.org/ghc/download_ghc_7_10_2#x86_64linux
* Unpack and go to unpacked directory
* `./configure`
* `sudo make install`

* Download https://www.haskell.org/cabal/release/cabal-install-1.22.6.0/cabal-install-1.22.6.0.tar.gz from https://www.haskell.org/cabal/download.html
* Unpack and go to unpacked directory
* `./bootstrap.sh`
* Add `.cabal/bin` to `$PATH`

* `cabal update`
* `cabal install happy`  # On my system, this dependency was not installed automatically

### Python3 and libraries

* `sudo apt-get install python3 python3-pip`
* `sudo pip3 install mypy-lang`

### PyQt5

* `sudo apt-get install python3-pyqt5 python3-pyqt5.qtsvg python3-pyqt5.qtwebkit`

# Running

* `make run` (in the root of the repository)


