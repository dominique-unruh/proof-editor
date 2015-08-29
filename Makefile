ifeq ($(OS),Windows_NT)
  EXE_SUFFIX=.exe
  BITS=32
  OSNAME=windows
else
  EXE_SUFFIX=
  BITS=64
  OSNAME=linux
endif

ifeq ($(BITS),32)
  ARCH=x86
else
  ARCH=x86_64
endif

CMATHML = cmathml$(EXE_SUFFIX)
MATHEDHASKELL = haskell/dist/build/MathEdHaskell/MathEdHaskell$(EXE_SUFFIX)

DEPENDENCIES=resources/mathjax src/testui/Ui_mainwin.py src/graph/Ui_controls.py resources/icons $(CMATHML) $(MATHEDHASKELL)

test : $(DEPENDENCIES)
	python3 src/main.py

all : $(DEPENDENCIES)
	echo $(wildcard haskell/*.hs haskell/src/*.hs haskell/src/*/*.hs haskell/src/*/*/*.hs)

Ui_%.py : %.ui
	pyuic5 -o $@ $<

$(MATHEDHASKELL) : haskell/MathEdHaskell.cabal \
		$(wildcard haskell/*.hs haskell/src/*.hs haskell/src/*/*.hs haskell/src/*/*/*.hs)
	cd haskell && cabal install --only-dependencies --enable-tests
	cd haskell && cabal configure
	cd haskell && cabal build

cmathml : cmathml.ml cmathml.mli
	ocamlfind ocamlopt -package batteries,xml-light -linkpkg cmathml.mli cmathml.ml -g -o cmathml 

cmathml.exe : cmathml.ml cmathml.mli Makefile
	ocamlfind ocamlopt xml-light.cmxa -package batteries -linkpkg cmathml.mli cmathml.ml -g -o cmathml.exe 

resources/mathjax : 
	rm -rf $@ /tmp/mathjax.zip
	wget https://github.com/mathjax/MathJax/archive/v2.5-latest.zip -O /tmp/mathjax.zip
	unzip /tmp/mathjax.zip
	mv MathJax-2.5-latest $@
	rm -rf $@/unpacked/ $@/fonts/HTML-CSS/TeX/png $@/fonts/HTML-CSS/{Asana-Math,Gyre-Pagella,Gyre-Termes,Latin-Modern,Neo-Euler,STIX-Web}

TANGO = tango-icon-theme-0.8.90
resources/icons :
	rm -rf resources/icons
	wget -c http://tango.freedesktop.org/releases/$(TANGO).tar.gz -O /tmp/$(TANGO).tar.gz
	cd /tmp && tar xfv $(TANGO).tar.gz
	rm -rf /tmp/icons
	mkdir /tmp/icons
	for i in 32x32 22x22 16x16 scalable index.theme.in; do cp -r /tmp/$(TANGO)/$$i /tmp/icons/; done
	mv /tmp/icons/index.theme.in /tmp/icons/index.theme
	mv /tmp/icons resources/

proof-editor-$(OSNAME)-$(BITS).zip : $(DEPENDENCIES)
	python3 setup.py build_exe
	cp -a build/exe.$(OSNAME)-x86_64-3.4 build/math-ed-linux-64
	cd build && zip ../$@ math-ed-linux-64

math-ed-windows-32: $(DEPENDENCIES_WIN)
	python setup.py build_exe
	cp -a build/exe.windows-x86-3.4 build/math-ed-windows-32

.mypy-stubs : mypy-stubs.py pyqt-mypy2.py
	rm -rf $@
	mypy pyqt-mypy2.py
	python3 pyqt-mypy2.py
	python3 mypy-stubs.py

export MYPYPATH=.mypy-stubs
export MYPY_CHECK=main.py # $(wildcard */*plugin.py) $(wildcard */tests.py)
mypy : .mypy-stubs
	python3 -c "import sys; [print('import '+f[:-3].replace('/','.')) for f in sys.argv[1:]]" $(MYPY_CHECK) >mypy-tmp.py
	mypy --verbose mypy-tmp.py 2>&1 | sed 's/, line /:/'
	rm -f mypy-tmp.py

mypy-nodep :
	python3 -c "import sys; [print('import '+f[:-3].replace('/','.')) for f in sys.argv[1:]]" $(MYPY_CHECK) >mypy-tmp.py
	mypy --verbose mypy-tmp.py 2>&1 | sed 's/, line /:/'

run_qtdesigner :
	LD_PRELOAD="/usr/lib/python3.4/config-3.4m-x86_64-linux-gnu/libpython3.4.so" \
	PYQTDESIGNERPATH="$(CURDIR)/src/mathview:$(CURDIR)/src/graph/" \
	designer --qt=5 src/testui/mainwin.ui
