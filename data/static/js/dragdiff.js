$(document).ready(function(){
    $("#content").prepend("<p>Drag one revision onto another to see differences.</p>");
    $(".difflink").draggable({helper: "clone"}); 
    $(".difflink").droppable({
         accept: ".difflink",
         drop: function(ev, ui) {
            var targetOrder = parseInt($(this).attr("order"));
            var sourceOrder = parseInt($(ui.draggable).attr("order"));
            var diffurl = $(this).attr("diffurl");
            if (targetOrder < sourceOrder) {
                var fromRev = $(this).attr("revision");
                var toRev   = $(ui.draggable).attr("revision");
            } else {
                var toRev   = $(this).attr("revision");
                var fromRev = $(ui.draggable).attr("revision");
            };
            location.href = diffurl + '?from=' + fromRev + '&to=' + toRev;
            }
        });
});

