function updatePreviewPane() {
    $("#previewpane").hide();
    var url = location.pathname.replace(/_edit\//,"_preview/");
    $.post(
      url,
      {"raw" : $("#editedText").attr("value")},
      function(data) {
        $('#previewpane').html(data);
        // Process any mathematics if we're using MathML
        if (typeof(convert) == 'function') { convert(); }
        // Process any mathematics if we're using jsMath
        if (typeof(jsMath) == 'object')    { jsMath.ProcessBeforeShowing(); }
      },
      "html");

    $('#previewpane').fadeIn(1000);

};
$(document).ready(function(){
    $("#previewButton").show();
  });

