
run : html/proof-editor/out.js html/mathquill.js
	xdotool windowactivate `xdotool search --name "Mozilla Firefox" | head -1`
	sleep 0.1
	xdotool key F5

clean :
	rm -rf .stack-work
	make -C mathquill clean
	rm -rf mathquill/build mathquill/node_modules

html/proof-editor/out.js : $(wildcard src/*.hs) $(wildcard ../src/*/*.hs) Makefile
	bash -c 'stack build proof-editor-gui |& grep -v "^Ignoring that the GHCJS boot package"; test $${PIPESTATUS[0]} -eq 0'

html/mathquill.js : mathquill/src/*.js mathquill/src/*/*.js mathquill/src/*/*/*.js
	make -C mathquill js css font
	cp -ur mathquill/build/mathquill.js mathquill/build/mathquill.css mathquill/build/font html/
