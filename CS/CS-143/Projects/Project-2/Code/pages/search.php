<link rel="stylesheet" type="text/css" href="./styles/style.css" media="screen" />
<link rel="stylesheet" type="text/css" href="./styles/search.css" media="screen" />
<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Montserrat" />

<div id="navbar">
  <a href="./index.html">
    <h1 id="site-title">CS 143 Project 2</h1>
  </a>
</div>

<div id="content">
  <?php
    // Get queries
    $actor = $_GET["actor"];
    $movie = $_GET["movie"];
    // Render default search layout
    if (!array_key_exists("actor", $_GET) && !array_key_exists("movie", $_GET)) {
      echo '<h2 id="page-header">Search for a Actor/Actress or Movie!</h2>';
      // Render actor search bar
      echo '<form class="search-bar" action="./search.php" method="get">';
      echo    '<input class="search-field" placeholder="Search for an actor/actress..." name="actor" />';
      echo    '<input class="search-button" type="submit" value="GO!" />';
      echo '</form>';
      // Render movie search bar
      echo '<form class="search-bar" action="./search.php" method="get">';
      echo    '<input class="search-field" placeholder="Search for a movie..." name="movie" />';
      echo    '<input class="search-button" type="submit" value="GO!" />';
      echo '</form>';
    // Render actor search
    } else if (array_key_exists("actor", $_GET)) {
      // Connect to class_db
      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0) { 
        die('Unable to connect to database [' . $db->connect_error . ']'); 
      }
      // Select actors from table
      $query = "SELECT first, last, id
                FROM Actor";
      // Build query from search
      if ($actor) {
        $actor = explode(' ', $actor);
        $query .= ' WHERE ';
        foreach ($actor as &$str) {
          $str = "(LOWER(first) LIKE LOWER('%" . $str . "%') 
                  OR LOWER(last) LIKE LOWER('%" . $str . "%'))";
        }
        $query .= implode(' AND ', $actor);
      }
      $res = $db->query($query);
      // Render search bar
      echo '<form class="search-bar" action="./search.php" method="get">';
      echo    '<input class="search-field" placeholder="Search for an actor/actress..." name="actor" />';
      echo    '<input class="search-button" type="submit" value="GO!" />';
      echo '</form>';
      // Render actor list
      echo "<strong id='result-num'>Showing $res->num_rows results</strong>";
      echo '<ul id="result-list">';
      while ($row = $res->fetch_assoc()) {
        $id = $row['id'];
        $firstName = $row['first'];
        $lastName = $row['last'];
        echo "<a href='./actor.php?id=$id'><li class='result'>$firstName $lastName</li></a>";
      }
      echo '</ul>';
      // Free result
      $rs->free();
      // Close connection
      $db->close();
    // Render movie search
    } else {
      // Connect to class_db
      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0) { 
        die('Unable to connect to database [' . $db->connect_error . ']'); 
      }
      // Select movies from table
      $query = "SELECT title, id
                FROM Movie";
      // Build query from search
      if ($movie) {
        $movie = explode(' ', $movie);
        $query .= ' WHERE ';
        foreach ($movie as &$str) {
          $str = "(LOWER(title) LIKE LOWER('%" . $str . "%'))";
        }
        $query .= implode(' AND ', $movie);
      }
      $res = $db->query($query);
      // Render search bar
      echo '<form class="search-bar" action="./search.php" method="get">';
      echo    '<input class="search-field" placeholder="Search for a movie..." name="movie" />';
      echo    '<input class="search-button" type="submit" value="GO!" />';
      echo '</form>';
      // Render movie list
      echo "<strong id='result-num'>Showing $res->num_rows results</strong>";
      echo '<ul id="result-list">';
      while ($row = $res->fetch_assoc()) {
        $id = $row['id'];
        $title = $row['title'];
        echo "<a href='./movie.php?id=$id'><li class='result'>$title</li></a>";
      }
      echo '</ul>';
      // Free result
      $res->free();
      // Close connection
      $db->close();
    }
  ?>
</div>
