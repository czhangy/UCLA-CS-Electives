#!/usr/local/bin/php
<?php
if (isset($_POST['username']) && isset($_POST['score'])) {
    saveScore();
}

function saveScore()
{
    isset($_POST['username']);
    $file = @fopen('scores.txt', 'w');
    fwrite($file, $_POST['username'] .  " " . $_POST['score'] . "\n");
    fclose($file);
}
?>