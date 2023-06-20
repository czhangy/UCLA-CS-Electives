#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("Q1");
session_start();

// Check POST
if (isset($_POST["num1"]) && isset($_POST["num2"])) {
    $_SESSION["num1"] = $_POST["num1"];
    $_SESSION["num2"] = $_POST["num2"];
}

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
        <h1>PIC 40A Final - Number Properties</h1>
    </header>

    <main>
        <p>
            You chose <?php echo $_SESSION["num1"] ?> and <?php echo $_SESSION["num2"] ?>.
        </p>
        <ul>
            <li>See their <a href="sum.php">sum</a>.</li>
            <li>See their <a href="product.php">product</a>.</li>
            <li>See the <a href="sum_squares.php">sum of their squares</a>.</li>
        </ul>
        <p>
            <a href="pick_numbers.html">Choose again</a>.
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