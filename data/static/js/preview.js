function updatePreviewPane() {
    $("#previewpane").hide();
    var url = location.pathname.replace(/_edit\//,"_preview/");
    $("#previewpane").load(url, { "raw" : $("#editedText").attr("value") }, function() {convert();} );
    $("#previewpane").fadeIn(1000);
};
$(document).ready(function(){
    $("#previewButton").show();
  });

