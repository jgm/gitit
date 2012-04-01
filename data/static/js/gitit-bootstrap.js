$(document).ready(function() {
  //$('link[href="/css/custom.css"]').attr('href','/css/bootstrap.css');
  // layout
  $('#doc3').addClass('container-fluid');
  $('#yui-main').addClass('row-fluid');
  $('#maincol').addClass('span9').wrapInner('<div class="well" />');
  $('#sidebar').addClass('span3').wrapInner('<div class="well" />');
  $('#sidebar').insertBefore($('#maincol'));

  // change global navbar
  var $link = $('<a class="brand" href="/">Wiki Name</a>');
  var $con = $('<div class="container-fluid" />').append($link, $('<div class="nav-collapse">').append($('.tabs'), $('#userbox')));
  $con = $('<div class="navbar-inner" />').append($con);
  $con = $('<div class="navbar navbar-fixed-top" />').append($con);
  //$con = $con.wrap('<div class="navbar" />');
  $('body').css('padding-top', '60px').prepend($con);
  console.log($con);
  $('.tabs').addClass('nav');
  $('.selected').addClass('active');
  $('#userbox').addClass('navbar-text pull-right');

  // change sidebar
  $('#footer').addClass('container-fluid').prepend('<hr>').insertAfter($('#doc3'));

  // change form width
  $('#editedText').css({
    'width': '98%',
    'height': '400px'}
    );
  $('#logMsg').css({'width': '98%'});

  // change button styles
  $("input[type='submit']").addClass('btn btn-primary');
  $("input[type='button']").addClass('btn btn-inverse');
});

