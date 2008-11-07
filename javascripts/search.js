function toggleMatches(obj) {
  obj.next('.matches').slideToggle(300);
  if (obj.html() == '[show matches]') {
      obj.html('[hide matches]');
    } else {
      obj.html('[show matches]');
    };
  }
$(function() {
  $('a.showmatch').attr("onClick", "toggleMatches($(this));");
  $('pre.matches').hide();
  $('a.showmatch').show();
  });
