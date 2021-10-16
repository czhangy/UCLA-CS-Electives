<link
  rel="stylesheet"
  type="text/css"
  href="./styles/style.css"
  media="screen"
/>
<link
  rel="stylesheet"
  type="text/css"
  href="./styles/review.css"
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
    $id = $_GET['id'];
    // Check if default page
    if ($id) {
      // Connect to class_db
      $db = new mysqli('localhost', 'cs143', '', 'class_db');
      if ($db->connect_errno > 0) { 
        die('Unable to connect to database [' . $db->connect_error . ']'); 
      }
      // Query Movie using ID
      $query = "SELECT title
                FROM Movie
                WHERE id = $id";
      $res = $db->query($query);
      $row = $res->fetch_assoc();
      $title = $row['title'];
      echo "<form action='review.php' method='get'>";
      echo  "<p id='page-header'>Write a Review for <em>$title</em>!</p>";
      echo  "<input id='hide' name='id' value=$id />";
      echo  "<label for='name-field'>Your name</label>";
      echo  "<input id='name-field' name='name' placeholder='Joe Bruin' />";
      echo  "<label for='rating-field'>Your rating</label>";
      echo  "<select id='rating-field' name='rating'>";
      echo    "<option value='' disabled>Select</option>";
      echo    "<option value='1'>1</option>";
      echo    "<option value='2'>2</option>";
      echo    "<option value='3'>3</option>";
      echo    "<option value='4'>4</option>";
      echo    "<option value='5'>5</option>";
      echo  "</select>";
      echo  "<label for='comment-field'>Your comment</label>";
      echo  "<textarea id='comment-field' name='comment' placeholder='Write a comment'></textarea>";
      echo  "<input type='submit' value='Submit!' />";
      // Check if review is beling posted
      if (array_key_exists("name", $_GET) && array_key_exists("rating", $_GET) && array_key_exists("comment", $_GET)) {
        // Create insert query
        $name = $_GET['name'];
        $rating = $_GET['rating'];
        $comment = $_GET['comment'];
        $query = "INSERT INTO Review
                  VALUES ('$name', NOW(), $id, $rating, '$comment')";
        $db->query($query);
        echo "<p>Your review has been submitted!</p>";
      }
      echo "</form>";
      // Free result
      $res->free();
      // Close connection
      $db->close();
    } else {
      echo '<div id="page-header">That page doesn\'t exist!</div>';
    }
  ?>
</div>
