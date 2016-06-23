<script src='/js/autolink.js'></script>
<script type="text/javascript">
/* <![CDATA[ */
\$(document).ready(function() {
    \$("#wikipage").each(function() {
      var that = \$(this);
      var text = that.html();
      that.html(text.autoLink());
    });
});
/* ]]> */
</script>
