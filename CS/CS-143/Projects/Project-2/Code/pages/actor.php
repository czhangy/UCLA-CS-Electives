<link
  rel="stylesheet"
  type="text/css"
  href="./styles/style.css"
  media="screen"
/>
<link
  rel="stylesheet"
  type="text/css"
  href="./styles/actor.css"
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
      if ($db->connect_errno > 0) { 
        die('Unable to connect to database [' . $db->connect_error . ']'); 
      }
      // Query Actor using ID
      $query = "SELECT * 
                FROM Actor
                WHERE id = $id";
      $res = $db->query($query);
      $row = $res->fetch_assoc();
      // Render static layout
      echo '<div id="actor-info">';
      echo  '<div id="info-labels">';
      echo    '<label class="info-label">NAME:</label>';
      echo    '<label class="info-label">SEX:</label>';
      echo    '<label class="info-label">DATE OF BIRTH:</label>';
      echo    '<label class="info-label">DATE OF DEATH:</label>';
      echo  '</div>';
      // Fetch data from ID query
      $firstName = $row['first'];
      $lastName = $row['last'];
      $sex = $row['sex'];
      $dob = $row['dob'];
      $dod = $row['dod'];
      echo  '<div id="info-fields">';
      echo    "<label class='info-label'>$firstName $lastName</label>";
      echo    "<label class='info-label'>$sex</label>";
      echo    "<label class='info-label'>$dob</label>";
      // Check if alive based on dod
      if (is_null($dod)) {
        echo "<label class='info-label'>Still Alive</label>";
      } else {
        echo "<label class='info-label'>$dod</label>";
      }
      echo  '</div>';
      echo '</div>';
      // Query Actor using ID
      $query = "SELECT title, role, mid
                FROM Actor, Movie, MovieActor
                WHERE Actor.id = aid AND Movie.id = mid AND $id = aid";
      $res = $db->query($query);
      echo '<hr id="divider" />';
      // Render table of roles
      if ($res->num_rows > 0) {
        echo '<table id="actor-roles">';
        echo  '<tr>';
        echo    '<th>Movie</th>';
        echo    '<th>Role</th>';
        echo  '</tr>';
        while ($row = $res->fetch_assoc()) {
          $title = $row['title'];
          $role = $row['role'];
          $mid = $row['mid'];
          echo  '<tr>';
          echo    "<td><a href='./movie.php?id=$mid'>$title</a></td>";
          echo    "<td>$role</td>";
          echo  '</tr>';
        }
        echo '</table>';
      } else {
        echo '<p id="no-roles">This actor has no roles!</p>';
      }
      // Free result
      $res->free();
      // Close connection
      $db->close();
    // ID is not present
    } else {
      echo '<div id="page-header">That page doesn\'t exist!</div>';
    }
  ?>
</div>

