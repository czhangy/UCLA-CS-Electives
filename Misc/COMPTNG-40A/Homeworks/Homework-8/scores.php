#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("shutTheBox");
session_start();

// Redirect if not logged in
if (!isset($_SESSION["loggedin"]) || !$_SESSION["loggedin"]) {
    header("Location: login.php");
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8" />
    <title>Shut The Box</title>
    <script src="scores.js" defer></script>
</head>

<body>
    <header>
        <h1>Shut The Box</h1>
    </header>
    <main>
        <section>
            <h2>Scores</h2>
            <p>
                Well done! Here are the scores so far...
            </p>
            <p id="scores"></p>
        </section>
        <fieldset>
            <button id="play-again">PLAY AGAIN!!!</button>
        </fieldset>
        <fieldset>
            <button id="start-update">Force update / start updating</button>
            <button id="stop-update">Stop updating</button>
        </fieldset>
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>