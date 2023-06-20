#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("Q1");
session_start();

// Check for numbers
if (!isset($_SESSION["num1"]) || !isset($_SESSION["num2"])) {
    header("Location: pick_numbers.html");
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <title>PIC 40A Final</title>
</head>

<body>
    <header>
        <h1>PIC 40A Final - The Sum Of The Squares Of Your Numbers</h1>
    </header>

    <main>
        <p>
            You chose <?php echo $_SESSION["num1"] ?> and <?php echo $_SESSION["num2"] ?>.
        </p>
        <p>
            The sum of their squares is <?php echo ($_SESSION["num1"] * $_SESSION["num1"]) + ($_SESSION["num2"] * $_SESSION["num2"]) ?>.
        </p>
        <p>
            <a href="numbers.php">Go back</a>.
        </p>
    </main>

    <footer>
        <hr>
        <small>
            &copy; Michael Andrews, 2023
        </small>
    </footer>
</body>

</html>