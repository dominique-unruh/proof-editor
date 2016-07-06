function onresize() {
//    console.log("resize detected");
    rect = span.getBoundingClientRect();
    window.controller.onResize(rect.right,rect.bottom);
}

function onLoad() {
    MQ = MathQuill.getInterface(2);
    span = document.getElementById("formula-span")
    tracksizeobj = document.getElementById("tracksize-obj")
    mathfield = MQ.StaticMath(span);
    tracksizeobj.contentDocument.defaultView.onresize = onresize;

    MQ.registerEmbed("error", function (id) {
	return { htmlString: '<span class=error>ERROR</span>',
		 text: function() { return "ERROR" },
		 latex: function() { return "\\embed{error}["+id+"]" }
       }
    });

//    test();
    
    window.controller.onLoad();
}

function enterHandler(m) {
    window.controller.onEnter(m.latex());
}

function setMath(math) {
//    console.log("setMath "+math);
    mathfield.latex(math);
    if (mathfield.innerFields.edit) {
        var m = mathfield.innerFields.edit;
        m.config({handlers:{enter:enterHandler}});
        m.focus();
    }
    window.controller.onMathRendered();
    onresize();
}

function getPath(element) {
    var cl = element.classList;
    if (!cl.contains("mq-class")) return null;
    for (var i=0; i<cl.length; i++) {
	console.log("class:",cl[i]);
	if (cl[i].startsWith("path-")) return cl[i];
    }
    return null;
}

function getSelection() {
    var sel = document.getElementsByClassName("mq-selection");
    if (sel.length==0) { console.log("nothing selected"); return null; };
    sel = sel[0];
    if (sel.childElementCount==1) {
	console.log("one child, checking if it has a path");
	var child = sel.children[0];
	var path = getPath(child);
	console.log("path",path);
	if (path!==null) return path;
    }
    console.log("TODO: should check upwards");
    return null;
}

function test() { // for interactive debugging
    //    mathfield.latex("x+\\frac{y}{\\MathQuillMathField[edit]{a+z}}+z");
    // mathfield.latex("\\class{path-}{{\\class{path-1}{{\\class{path-1-1}{\\class{variable}{x}}}+{\\class{path-1-2}{\\class{variable}{y}}}}}-{\\class{path-2}{\\embed{error}[]}}}");
    mathfield.latex("\\class{path-}{{\\class{path-1}{{\\class{path-1-1}{\\class{variable}{x}}}+{\\class{path-1-2}{\\MathQuillMathField[edit]{a+z}}}}}-{\\class{path-2}{\\embed{error}[]}}}");

    setInterval(function () { var sel = getSelection(); console.log("selection:",sel) }, 1000);
}
