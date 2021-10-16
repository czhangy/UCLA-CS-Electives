<link
  rel="stylesheet"
  type="text/css"
  href="./styles/style.css"
  media="screen"
/>
<link
  rel="stylesheet"
  type="text/css"
  href="./styles/movie.css"
  media="screen"
/>
<link
  rel="stylesheet"
  href="https://fonts.googleapis.com/css?family=Montserrat"
/>

<div id="navbar">
  <a href="./index.html">
    <h1 id="site-title">CS 143 Project 2</h1>
  </a>
</div>

<div id="content">
  <?php
    // Get param ID
    $id = $_GET["id"];
    // ID is present and valid
    if ($id) {
      // Connect to class_db
      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0)
        die('Unable to connect to database [' . $db->connect_error . ']'); 
      // Query Movie using ID
      $query = "SELECT * 
                FROM Movie
                WHERE id = $id";
      $row = $db->query($query)->fetch_assoc();
      // Render static layout
      echo '<div id="movie-info">';
      echo  '<div id="info-labels">';
      echo    '<label class="info-label">TITLE:</label>';
      echo    '<label class="info-label">YEAR:</label>';
      echo    '<label class="info-label">MPAA RATING:</label>';
      echo    '<label class="info-label">PRODUCED BY:</label>';
      echo    '<label class="info-label">GENRE:</label>';
      echo  '</div>';
      // Fetch data from ID query
      $title = $row['title'];
      $year = $row['year'];
      $rating = $row['rating'];
      $company = $row['company'];
      echo  '<div id="info-fields">';
      echo    "<label class='info-label'>$title</label>";
      echo    "<label class='info-label'>$year</label>";
      echo    "<label class='info-label'>$rating</label>";
      echo    "<label class='info-label'>$company</label>";
      // Query MovieGenre using ID
      $query = "SELECT * FROM MovieGenre WHERE mid = $id";
      $genre = $db->query($query)->fetch_assoc()['genre'];
      echo    "<label class='info-label'>$genre</label>";
      echo  '</div>';
      echo '</div>';
      echo '<hr class="divider" />';
      echo '<p class="section-header">Cast</p>';
      // Query for cast
      $query = "SELECT first, last, role, aid
                FROM Actor, Movie, MovieActor
                WHERE Actor.id = aid AND Movie.id = mid AND mid = $id";
      $res = $db->query($query);
      // Render cast table
      echo '<table id="movie-cast">';
        echo  '<tr>';
        echo    '<th>Actor</th>';
        echo    '<th>Role</th>';
        echo  '</tr>';
        while ($row = $res->fetch_assoc()) {
          $firstName = $row['first'];
          $lastName = $row['last'];
          $role = $row['role'];
          $aid = $row['aid'];
          echo  '<tr>';
          echo    "<td><a href='./actor.php?id=$aid'>$firstName $lastName</a></td>";
          echo    "<td>$role</td>";
          echo  '</tr>';
        }
      echo '</table>';
      echo '<hr class="divider" />';
      // Render ratings section
      echo '<div id="ratings">';
      // Query for rating
      $query = "SELECT AVG(rating) FROM Review WHERE mid = $id";
      $rating = $db->query($query)->fetch_assoc()['AVG(rating)'];
      // Check if ratings exist
      if (is_null($rating))
        // Placeholder for non-existent ratings
        echo  '<p class="rating-text">No User Ratings!</p>';
      else {
        // Truncate decimal
        $rating = round($rating, 1);
        echo  '<p class="rating-text">RATING:</p>';
        echo  "<p class='rating-text'>$rating / 5</p>";
      }
      echo '</div>';
      echo '<hr class="divider" />';
      // Render reviews section
      echo "<a id='review-link' href='./review.php?id=$id'><button id='review-button'>Write a Review!</button></a>";
      $query = "SELECT * FROM Review WHERE mid = $id";
      $res = $db->query($query);
      echo '<div id="reviews">';
        if ($res->num_rows > 0) {
          // Get all comments
          while ($row = $res->fetch_assoc()) {
            $name = $row['name'];
            $time = $row['time'];
            $rating = $row['rating'];
            $comment = $row['comment'];
            echo '<div class="review-comment">';
            echo  '<div class="comment-header">';
            echo    "<p class='comment-name'>$name</p>";
            echo    "<p class='comment-time'>$time</p>";
            echo  '</div>';
            echo  "<p class='comment-rating'>$rating / 5</p>";
            echo  "<p class='comment-body'>$comment</p>";
            echo '</div>';
          }
        } else
          echo  '<p class="rating-text">This movie has no reviews!</p>';
      echo '</div>';
      // Free result
      $res->free();
      // Close connection
      $db->close();
    // ID is not present
    } else
      echo '<div id="page-header">That page doesn\'t exist!</div>';
  ?>
</div>
