<script type="text/javascript">
/* <![CDATA[ */
  \$.get("$base$/_user", {}, function(username, status) {
     \$("#logged_in_user").text(username);
     if (username == "") {  // nobody logged in
        \$("#logoutlink").hide();
        \$("#loginlink").show();
     } else {
        \$("#logoutlink").show();
        \$("#loginlink").hide();
     };
   });
/* ]]> */
</script>
