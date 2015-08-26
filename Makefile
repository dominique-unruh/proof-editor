
# opam install batteries xml-light

DEPENDENCIES_ALL=resources/mathjax src/testui/Ui_mainwin.py src/graph/Ui_controls.py resources/icons
DEPENDENCIES_LINUX=$(DEPENDENCIES_ALL) cmathml
DEPENDENCIES_WIN=$(DEPENDENCIES_ALL) cmathml.exe

export OCAMLRUNPARAM=b
test_cmathml : cmathml
	@#OCAMLRUNPARAM=b ./cmathml auto2cmml 'x x = x all'
	./cmathml cmml2pmml '<bind><csymbol cd="quant1">forall</csymbol><bvar><ci>x</ci></bvar><apply><csymbol cd="relation1">eq</csymbol><ci>x</ci><ci>x</ci></apply></bind>'
	@#OCAMLRUNPARAM=b ./cmathml transformation commute "1 2 + 3 +" "1"
	@#OCAMLRUNPARAM=b  ./cmathml "transformation"  "modusponens" '<apply><csymbol cd="relation1">eq</csymbol><cn type="integer">1</cn><apply><csymbol cd="arith1">plus</csymbol><apply><csymbol cd="arith1">plus</csymbol><ci>d</ci><apply><csymbol cd="arith1">plus</csymbol><ci>a</ci><ci>b</ci></apply></apply><ci>c</ci></apply></apply>' "-" '<apply><csymbol cd="logic1">implies</csymbol><apply><csymbol cd="relation1">eq</csymbol><cn type="integer">1</cn><apply><csymbol cd="arith1">plus</csymbol><apply><csymbol cd="arith1">plus</csymbol><ci>d</ci><apply><csymbol cd="arith1">plus</csymbol><ci>a</ci><ci>b</ci></apply></apply><ci>c</ci></apply></apply><ci>done</ci></apply>' "-" 


Ui_%.py : %.ui
	pyuic5 -o $@ $<

test : all
	python3 src/main.py

all : $(DEPENDENCIES_LINUX)

cmathml : cmathml.ml cmathml.mli Makefile
	ocamlfind ocamlopt -package batteries,xml-light -linkpkg cmathml.mli cmathml.ml -g -o cmathml 

cmathml.exe : cmathml.ml cmathml.mli Makefile
	#cp /cygdrive/c/OCaml/lib/*.dll .
	ocamlfind ocamlopt xml-light.cmxa -package batteries -linkpkg cmathml.mli cmathml.ml -g -o cmathml.exe 

resources/mathjax : 
	rm -rf $@ /tmp/mathjax.zip
	wget https://github.com/mathjax/MathJax/archive/v2.5-latest.zip -O /tmp/mathjax.zip
	unzip /tmp/mathjax.zip
	mv MathJax-2.5-latest $@
	rm -rf $@/unpacked/ $@/fonts/HTML-CSS/TeX/png $@/fonts/HTML-CSS/{Asana-Math,Gyre-Pagella,Gyre-Termes,Latin-Modern,Neo-Euler,STIX-Web}

#mathjax.qrc : mathjax Makefile
#	qrcgen.py mathjax ""

#mathjax_rc.py : mathjax.qrc
#	pyrcc5 -o $@ $<


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

math-ed-linux-64.zip : $(DEPENDENCIES_LINUX)
	python3 setup.py build_exe
	cp -a build/exe.linux-x86_64-3.4 build/math-ed-linux-64
	cd build && zip ../$@ math-ed-linux-64

math-ed-windows-32: $(DEPENDENCIES_WIN)
	python setup.py build_exe
	cp -a build/exe.windows-x86-3.4 build/math-ed-windows-32


cxfreeze :
	rm -rf build
	python3 scripts/setup.py build_exe
	cd build/exe.linux-x86_64-3.4 && ./main

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

# mypy2 :
# 	mypy pyqt-mypy2.py
# 	rm -rf mypy-pyqt/PyQt5
# 	python3 pyqt-mypy2.py
# 	export MYPYPATH=mypy-pyqt; mypy -m main
