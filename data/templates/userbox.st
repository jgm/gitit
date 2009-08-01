<div id="userbox">
  $if(user)$
  <a href="$base$/_logout">Logout $user$</a>
  $else$
  <a href="$base$/_login">Login / create an account</a>
  $endif$
</div>