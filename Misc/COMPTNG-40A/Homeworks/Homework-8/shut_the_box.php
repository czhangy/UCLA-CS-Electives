#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("shutTheBox");
session_start();

// Redirect to login if not logged in
if (!isset($_SESSION["loggedin"]) || !$_SESSION["loggedin"]) {
    header("Location: login.php");
}

// Redirect to welcome if no username
if (!isset($_COOKIE["username"])) {
    header("Location: welcome.php");
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8" />
    <title>Shut The Box</title>
    <script src="shut_the_box.js" defer></script>
</head>

<body>
    <header>
        <h1>Shut The Box</h1>
    </header>
    <main>
        <section>
            <h2>The Rules</h2>
            <ol type="i">
                <li>
                    Each turn, you roll the dice (or die), and select one or
                    more boxes that sum to the value of your roll.
                </li>
                <li>
                    You will not be allowed to pick the boxes which you
                    choose on subsequent turns.
                </li>
                <li>
                    When the sum of the remaining boxes is less than or
                    equal to 6, you will only roll a single die.
                </li>
                <li>
                    When you cannot make a move or you give up, the sum of
                    the boxes which are left gives your score.
                </li>
                <li>
                    Lower scores are better and a score of 0 is called
                    "shutting the box."
                </li>
            </ol>
        </section>
        <section>
            <h2>Dice roll</h2>
            <fieldset>
                <button id="roll-button">Roll dice</button>
                <span id="roll-result">Result:</span>
            </fieldset>
        </section>
        <section>
            <h2>Box selection</h2>
            <table>
                <thead>
                    <tr>
                        <?php
                        for ($i = 1; $i <= 9; $i++) {
                            echo "<th>$i</th>";
                        }
                        ?>
                    </tr>
                </thead>
                <tbody>
                    <tr>
                        <?php
                        for ($i = 0; $i < 9; $i++) {
                            echo "<td><input type='checkbox'></td>";
                        }
                        ?>
                    </tr>
                </tbody>
            </table>
            <fieldset>
                <button id="submit-button" disabled>
                    Submit box selection
                </button>
                <button id="end-game-button">
                    I give up/I can't make a valid move
                </button>
            </fieldset>
        </section>
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>