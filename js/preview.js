function updatePreviewPane() {
    $("#previewpane").hide();
    $("#previewpane").load("/_preview", { "raw" : $("#editedText").attr("value") });
    $("#previewpane").fadeIn(1000);
};
$(document).ready(function(){
    $("#previewButton").show();
  });

