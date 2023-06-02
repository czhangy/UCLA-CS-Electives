#!/usr/local/bin/php
<?php
if (isset($_POST['username']) && $_POST['score']) {
    $file = fopen('file.txt', 'w') or die("File could not be opened");

    fclose($file);
}
?>