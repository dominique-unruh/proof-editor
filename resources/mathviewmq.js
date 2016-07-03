function onLoad() {
    MQ = MathQuill.getInterface(2);
    span = document.getElementById("formula-span")
    mathfield = MQ.StaticMath(span);
    rect = span.getBoundingClientRect();
    window.controller.onMathRendered(rect.right,rect.bottom);
}
