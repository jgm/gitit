function updatePreviewPane() {
    $("#previewpane").hide();
    $("#previewpane").load("___preview", { "raw" : $("#editedText").attr("value")
                                         , "format" : $("#format").attr("value")
                                         , "lhs": $("#lhs").attr("value")
                                         , "toc": $("#toc").attr("value")
                                         , "title": $("#title").attr("value")
                                         , "categories": $("#categories").attr("value")
                                         });
    $("#previewpane").fadeIn(1000);
};
$(document).ready(function(){
    $("#previewButton").show();
  });

