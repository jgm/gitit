<script type="text/javascript">
/* <![CDATA[ */
  \$.get("$base$/_user", {}, function(username, status) {
     \$("#username").text(username);
     if (username == "") {  // nobody logged in
        \$("#logoutlink").css("visibility","hidden");
        \$("#loginlink").css("visibility","visible");
     } else {
        \$("#logoutlink").css("visibility","visible");
        \$("#loginlink").css("visibility","hidden");
     };
   });
/* ]]> */
</script>
