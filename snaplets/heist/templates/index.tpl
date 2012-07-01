<apply template="base">

  <ifLoggedIn>
    <p>You're logged in as '<loggedInUser/>'</p>

    <div id="todolist" />

    <p><a href="/logout">Logout</a></p>

  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="login"/>
  </ifLoggedOut>

  <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"></script>
  <script src="http://ajax.cdnjs.com/ajax/libs/underscore.js/1.3.3/underscore-min.js"></script>
  <script src="http://ajax.cdnjs.com/ajax/libs/backbone.js/0.9.2/backbone-min.js"></script>
  <script src="main.js" type="text/javascript"></script>

</apply>
