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
