function updatePreviewPane() {
    $("#previewpane").hide();
    var url = location.pathname.replace(/_edit\//,"_preview/");
    $.post(
      url,
      {"raw" : $("#editedText").attr("value")},
      function(data) {
        $('#previewpane').html(data);
        if (typeof(convert) == 'function') { convert(); }
      },
      "html");
    $('#previewpane').fadeIn(1000);
};
$(document).ready(function(){
    $("#previewButton").show();
  });

