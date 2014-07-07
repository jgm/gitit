jQuery.fn.highlightPattern = function (patt, className)
{

    // escape regex metacharacters that may be in the patt
    var epatt = patt.replace(/([.*+?^=!:${}()|\[\]\/\\])/g, "\\$1");

    // patt is a space separated list of strings - we want to highlight
    // an occurrence of any of these strings as a separate word.
    var regex = new RegExp('\\b(' + epatt.replace(/ /g, '|') + ')\\b', 'gi');

    return this.each(function ()
    {
        this.innerHTML = this.innerHTML.replace(regex,
          '<span class=\'' + className + '\'>' + '$1' + '</span>');
    });
};
function toggleMatches(obj) {
  var pattern = $('#pattern').text();
  var matches = obj.next('.matches')
  matches.slideToggle(300);
  matches.highlightPattern(pattern, 'highlighted');
  if (obj.html() == '[show matches]') {
      obj.html('[hide matches]');
    } else {
      obj.html('[show matches]');
    };
  }
$(function() {
  $('a.showmatch').attr('onClick', 'toggleMatches($(this));');
  $('pre.matches').hide();
  $('a.showmatch').show();
  });
