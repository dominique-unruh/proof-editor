function m_to_pmml() {
    pmml = m.pmathml();
    pmml2 = $("<tmp/>").append(pmml).html();
    //console.log("m_to_pmml",pmml,pmml2);
    var mathspan = $("#mathspan"); // $("<span>").attr("style","border:solid").appendTo("body");
    mathspan.html('<math xmlns="http://www.w3.org/1998/Math/MathML">'+pmml2+'</math>');
}
