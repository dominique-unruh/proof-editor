window.MathJax = {
    jax: ["input/MathML", "output/HTML-CSS" ],
    extensions: ["mml2jax.js" ],
    showMathMenu: false,
    mml2jax: { preview: "Please wait…" },
};

var dragging = false;
var dragx;
var dragy;
var dragrect;

function onMouseUp(e) {
    console.log("mouseup");
    dragging = false;
    dragrect.style.visibility = 'hidden';

    var x = dragx;
    var y = dragy;
    var x2 = e.clientX;
    var y2 = e.clientY;
    var t;
    if (x2<x) { t=x2; x2=x; x=t; }
    if (y2<y) { t=y2; y2=y; y=t; }
    dragrect.style.left = x;
    dragrect.style.top = y;
    dragrect.style.width = x2-x;
    dragrect.style.height = y2-y;
    
    var selection_rect = {left:x,top:y,right:x2,bottom:y2}; // new DOMRect(x,y,x2-x,y2-y);
    console.log("selection_rect "+x+" "+y+" "+x2+" "+y2);
    var selected_elem = touchSelection(selection_rect);
    
    var highlit = document.getElementsByClassName("highlight")
    for (var i=0; i<highlit.length; i++) {
	highlit[i].classList.remove("highlight");
    }
    
    if (selected_elem==null) {
	window.controller.onMathDeselected();
	console.log("mouseup processed 1");
	return;
    }
    
    selected_elem.classList.add("highlight");

    var path = selected_elem.getAttriSbute("path");
    console.log("selected path",path);
    window.controller.onMathSelection(path);
    console.log("mouseup processed 2");
}

// Returns the smallest subelement (with path-attribute) of the formula-span that contains leaf elements that intersect the selection
// (or null if there is no intersection)
function touchSelection(selection) {
    var math = document.getElementById("formula-span");
    var math = touchSelection0(math,selection);
    if (math!=null) console.log("preselection:",math);
    while (math!=null) {
	if (math.hasAttribute("path")) return math;
	math = math.parentElement;
    }
    return null;
}


// Returns the smallest subelement of math that contains leaf elements that intersect the selection
// (or null if there is no intersection)
function touchSelection0(math,selection) {
    var mathrect = math.getBoundingClientRect();
    //var is_leaf = math.classList.contains("mi") || math.classList.contains("mn"); // TODO: other checks?
    var is_leaf = math.classList.contains("leaf")
    //var is_leaf = (math.childElementCount==0);
    if (is_leaf) {
	var overlap = !(mathrect.right < selection.left || 
			mathrect.left > selection.right || 
			mathrect.bottom < selection.top || 
			mathrect.top > selection.bottom);
	if (overlap) return math;
    } else {
	var found = null;
	for (var i=0; i<math.childElementCount; i++) {
	    var child = math.children[i];
	    var res = touchSelection0(child,selection);
	    if (res==null) continue;
	    if (found!=null) return math; // at least two subelements touch the selection, so math is the smallest containing all
	    found = res;
	}
	return found;
    }
}

function onMouseDown(e) {
    console.log("mousedown");
    //window.controller.mousedown();
    if (e.which != 1) return false;
    dragx = e.clientX;
    dragy = e.clientY;
    dragging = true;
    e.preventDefault();
    dragrect.style.left = dragx;
    dragrect.style.top = dragy;
    dragrect.style.width = 0;
    dragrect.style.height = 0;
    dragrect.style.visibility = 'visible';
    console.log("mousedown processed");
    return false;
}

function onMouseMove(e) {
    if (!dragging) return true;
    console.log("mousemove");
    e.preventDefault();
    var x = dragx;
    var y = dragy;
    var x2 = e.clientX;
    var y2 = e.clientY;
    var t;
    if (x2<x) { t=x2; x2=x; x=t; }
    if (y2<y) { t=y2; y2=y; y=t; }
    dragrect.style.left = x;
    dragrect.style.top = y;
    dragrect.style.width = x2-x;
    dragrect.style.height = y2-y;
    console.log("mousemove processed");
    return false;
}

function onLoad() {
    //document.addEventListener('click',onClick);
    document.addEventListener('mousedown',onMouseDown);
    document.addEventListener('mouseup',onMouseUp);
    document.addEventListener('mousemove',onMouseMove);
    dragrect = document.getElementById("selection-rect");
    MathJax.Hub.Register.StartupHook("End",function () {window.controller.onMathRendered()});
}
