<div id="userbox">
$if(loggedinuser)$
  <a id="logoutlink" class="login" href="$base$/_logout">Logout <span id="logged_in_user">$loggedinuser$</span></a>
$else$
  <a id="loginlink" class="login" href="$base$/_login">Login / Get an account</a>
$else$
</div>
