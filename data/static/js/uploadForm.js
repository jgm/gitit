$(document).ready(function(){
    $("#file").change(function () {
      var fn = $(this).val().replace(/.*\\/,"");
      $("#wikiname").val(fn);
    });
  });
