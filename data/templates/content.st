<div id="content">
  $if(nothead)$
    <h2 class="revision">Revision $revision$</h2>
  $endif$
  <h1 class="pageTitle"><a href="$base$/$pagename$">$pagetitle$</a></h1>
  $if(messages)$
    $messages()$
  $endif$
  $content$
</div>
