<div id="userbox">
  $if(showLogin)$
    $if(user)$
    <a href="$base$/_logout">Logout $user$</a>
    $else$
    <a href="$base$/_login">Login</a> &bull; <a href="/_register">Get an account</a>
    $endif$
  $else$
    <span class="login">Logged in as $user$</span>
  $endif$
</div>
