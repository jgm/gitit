<div id="userbox">
  $if(user)$
  <a href="$base$/_logout">Logout $user$</a>
  $else$
  <a href="$base$/_login">Login</a> &bull; <a href="/_register">Get an account</a>
  $endif$
</div>
