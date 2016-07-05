function onresize() {
//    console.log("resize detected");
    rect = span.getBoundingClientRect();
    window.controller.onResize(rect.right,rect.bottom+2);
}

function onLoad() {
    MQ = MathQuill.getInterface(2);
    span = document.getElementById("formula-span")
    tracksizeobj = document.getElementById("tracksize-obj")
    mathfield = MQ.StaticMath(span);
    tracksizeobj.contentDocument.defaultView.onresize = onresize
    editOptions = { handlers: { enter: function enter(m) { window.controller.onEnter(m.latex()) }}};
    window.controller.onLoad();
}

function setMath(math) {
//    console.log("setMath "+math);
    mathfield.latex(math);
    if (mathfield.innerFields.edit) { var m = mathfield.innerFields.edit; m.config(editOptions); m.focus(); }
    window.controller.onMathRendered();
    onresize();
}
