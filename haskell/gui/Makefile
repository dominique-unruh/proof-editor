
run : html/proof-editor/out.js html/mathquill/build/mathquill.js
	xdotool windowactivate `xdotool search --name "Mozilla Firefox" | head -1`
	sleep 0.1
	xdotool key F5


html/proof-editor/out.js : $(wildcard src/*.hs) Makefile
	bash -c 'stack build |& grep -v "^Ignoring that the GHCJS boot package"; test $${PIPESTATUS[0]} -eq 0'

html/mathquill/build/mathquill.js : html/mathquill/src/*.js html/mathquill/src/*/*.js html/mathquill/src/*/*/*.js
	make -C html/mathquill
