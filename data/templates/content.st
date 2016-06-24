<div id="content">
  $if(revision)$
    <h2 class="revision">Revision $revision$ (click the page title to view the current version)</h2>
  $endif$
  <h1 class="pageTitle"><a href="$base$$pageUrl$">$pagetitle$</a></h1>
  $if(messages)$
    $messages()$
  $endif$
  $content$
</div>
