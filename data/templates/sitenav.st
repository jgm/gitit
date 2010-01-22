<div class="sitenav">
  <fieldset>
    <legend>Site</legend>
    <ul>
      <li><a href="$base$/">Front page</a></li>
      <li><a href="$base$/_index">All pages</a></li>
      <li><a href="$base$/_categories">Categories</a></li>
      <li><a href="$base$/_random">Random page</a></li>
      <li><a href="$base$/_activity">Recent activity</a></li>
      $if(wikiupload)$
        <li><a href="$base$/_upload">Upload a file</a></li>
      $endif$
      $if(feed)$
      <li><a href="$base$/_feed/" type="application/atom+xml" rel="alternate" title="ATOM Feed">Atom feed</a> <img alt="feed icon" src="$base$/img/icons/feed.png"/></li>
      $endif$
      <li><a href="$base$/Help">Help</a></li>
    </ul>
    <form action="$base$/_search" method="post" id="searchform">
     <input type="text" name="patterns" id="patterns"/>
     <input type="submit" name="search" id="search" value="Search"/>
    </form>
    <form action="$base$/_go" method="post" id="goform">
      <input type="text" name="gotopage" id="gotopage"/>
      <input type="submit" name="go" id="go" value="Go"/>
    </form>
  </fieldset>
</div>
