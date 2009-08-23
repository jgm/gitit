function updatePreviewPane() {
    $("#previewpane").hide();
    var url = location.pathname.replace(/_edit\//,"_preview/");
    $("#previewpane").load(url, { "raw" : $("#editedText").attr("value")
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

