function onLoad() {
    MQ = MathQuill.getInterface(2);
    span = document.getElementById("formula-span")
    mathfield = MQ.StaticMath(span);
    window.controller.onLoad();
}

//function getSize() {
//    return span.getBoundingClientRect();
//}

function setMath(math) {
    console.log("setMath "+math);
    mathfield.latex(math);
    rect = span.getBoundingClientRect();
    window.controller.onMathRendered(rect.right,rect.bottom);
}