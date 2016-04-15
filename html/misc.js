function m_to_pmml() {
    pmml = m.pmathml();
    //pmml2 = $("<tmp/>").append(pmml).html();
    //console.log("m_to_pmml",pmml,pmml2);
    var mathspan = $("#mathspan"); // $("<span>").attr("style","border:solid").appendTo("body");

    mathspan.empty().append(pmml);

    //    mathspan.html('<math xmlns="http://www.w3.org/1998/Math/MathML">'+pmml2+'</math>');
}


function pmml_to_m() {
  try {
    console.log("pmml_to_m");
    var math = $("#mmlex")[0];
    m.__controller.writePMML(math);
  } catch (e) {
    console.error(e);
  };
}

function copy_via_pmml() {
  try {
    m2.latex("");
    var mathspan = $("#mathspan");
    mathspan.empty();
    $("#mmlsrc").empty();
    var math = m.pmathml();
    mathspan.append(math);
    $("#mmlsrc").text(math.outerHTML);
    m2.__controller.writePMML(math);
  } catch (e) {
    console.error(e);
  }
};
