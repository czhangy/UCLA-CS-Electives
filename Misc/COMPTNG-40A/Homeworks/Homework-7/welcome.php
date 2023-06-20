#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("shutTheBox");
session_start();

// Check for numbers
if (!isset($_SESSION["num1"]) || !isset($_SESSION["num2"])) {
    header("Location: pick_numbers.html");
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8" />
    <title>Shut The Box</title>
    <script src="username.js" defer></script>
    <script src="welcome.js" defer></script>
</head>

<body>
    <header>
        <h1>Welcome! Ready to play "Shut The Box"?</h1>
    </header>
    <main>
        <section>
            <h2>Choose a username</h2>
            <p>
                So that we can post your score(s), please choose a username
            </p>
            <fieldset>
                <label for="username">Username:</label>
                <input id="username" type="text" />
                <input id="submit-button" type="button" value="Submit" />
            </fieldset>
        </section>
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>