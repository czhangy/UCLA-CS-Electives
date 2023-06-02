#!/usr/local/bin/php
<?php
if (isset($_POST['username']) && isset($_POST['score'])) {
    $file = fopen('scores.txt', 'w') or die('File could not be opened');
    $line = "$_POST['username'] $_POST['score']";
    fwrite($file, $line);
    fclose($file);
}
?>