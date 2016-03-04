This is still in "closed" development (that is, I don't think you can
make sense of this repo).

If you have interest in more information (like a real README), contact me.



# Installation (Linux)

## Prerequisites

### GHC and Cabal

Install GHC 7.10.2 (older version are untested, 7.6.* is known *not* to work) and Cabal. For example:

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
* `sudo pip3 install mypy-lang lxml`

### PyQt5

* `sudo apt-get install python3-pyqt5 python3-pyqt5.qtsvg python3-pyqt5.qtwebkit`


## Building and running from source directory

* `make run` (in the root of the repository)



# Installation (Windows)

## Prerequisites

### Cygwin environment TODO: not needed???

Cygwin is needed for executing the makefile. Probably other environments providing a make-command will work as well.

* Download https://cygwin.com/setup-x86.exe (32 bit) or https://cygwin.com/setup-x86_64.exe (64 bit) from https://cygwin.com/install.html. (It should not matter whether a 32- or 64-bit version is used.)
* Execute setup and select packages:

### Notes on matching versions

Python3, PyQt5, and GHC must have matching versions in the following senses:

* All must be 32-bit or all must be 64-bit.
* The Python3-version must match the Python version of the PyQt5 installer.
* At the time of this writing, installing the 64-bit version of PyQt5 did not succeed, and PyQt5 was only available for Python 3.4.

### Python3 and libraries

* Download Python 3 installer from https://www.python.org/downloads/windows/. E.g., https://www.python.org/ftp/python/3.4.3/python-3.4.3.msi.
* Run the installer. Select the installation option "Add python.exe to Path"

### PyQt5

* Download PyQt5 installer from https://www.riverbankcomputing.com/software/pyqt/download5. E.g., http://sourceforge.net/projects/pyqt/files/PyQt5/PyQt-5.5/PyQt5-5.5-gpl-Py3.4-Qt5.5.0-x32.exe.
* Run the installer
* `sudo pip3 install mypy-lang` (in `cmd.exe` or in Cygwin Terminal)

### GHC and Stack

* Download Stack from https://github.com/commercialhaskell/stack/wiki/Downloads#windows, e.g., https://github.com/commercialhaskell/stack/releases/download/v0.1.4.0/stack-0.1.4.0-i386-windows.zip
* Unpack the file stack.exe so that it is in the path (e.g., to c:\Windows)
* Visit https://www.haskell.org/ in Internet Explorer (this makes sure the certificate that stack needs is installed).
* `stack setup` (In `cmd.exe`. Do not use Cygwin!)

### MinGW and MSYS

* Download the MinGW installer from http://sourceforge.net/projects/mingw/files/latest/download



* Download MinGHC installer from https://www.haskell.org/downloads/windows. E.g., https://github.com/fpco/minghc/releases/download/2015-08-13/minghc-7.10.2-i386.exe.
* Run the installer
* Run `cabal update` (In `cmd.exe`. Do not use Cygwin!)

## Building and running from source directory

TODO: Doesn't work -- need to use stack, not cabal

* `make run` (in the Cygwin Terminal, in the root of the repository)
* Alternative: `make` and double click on `src\main.py` in the repository.
