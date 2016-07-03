
all_resources : resources/mathjax resources/jquery.js resources/mathquill

resources/mathjax :
	rm -rf $@ tmp/mathjax.zip
	mkdir -p resources
	wget https://github.com/mathjax/MathJax/archive/v2.5-latest.zip -O tmp/mathjax.zip
	unzip tmp/mathjax.zip
	mv MathJax-2.5-latest $@
	rm -rf $@/unpacked/ $@/fonts/HTML-CSS/TeX/png $@/fonts/HTML-CSS/{Asana-Math,Gyre-Pagella,Gyre-Termes,Latin-Modern,Neo-Euler,STIX-Web}

resources/jquery.js :
	rm -rf $@
	mkdir -p resources
	wget https://code.jquery.com/jquery-2.2.4.js -O $@

MATHQUILL_COMMIT=v0.10.1
resources/mathquill :
	rm -rf $@ tmp/mathquill.zip tmp/mathquill-extract
	mkdir -p tmp/mathquill-extract
	wget https://github.com/mathquill/mathquill/archive/$(MATHQUILL_COMMIT).zip -O tmp/mathquill.zip
	unzip tmp/mathquill.zip -d tmp/mathquill-extract
	make -C tmp/mathquill-extract/mathquill-*
	mv tmp/mathquill-extract/mathquill-*/build $@
