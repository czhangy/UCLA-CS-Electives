#!/usr/local/bin/php
<?php

// Set up session
session_save_path(__DIR__ . "/sessions/");
session_name("shutTheBox");
session_start();

function checkPassword($password, &$invalidPassword)
{
    // Get correct hashed password
    $passFile = @fopen("h_password.txt", "r") or die("Unable to find h_password.txt");
    $hPassword = fgets($passFile);
    fclose($passFile);

    // Check submitted password
    if (hash("md5", $password) === $hPassword) {
        $_SESSION["loggedin"] = true;
    } else {
        $_SESSION["loggedin"] = false;
        $invalidPassword = true;
    }
}

// Check for password
$invalidPassword = false;
if (isset($_POST["submittedPassword"])) {
    checkPassword($_POST["submittedPassword"], $invalidPassword);
    if (!$invalidPassword) {
        header("Location: welcome.php");
        exit;
    }
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <title>Shut the Box</title>
</head>

<body>
    <header>
        <h1>Welcome! Ready to play "Shut The Box"?</h1>
    </header>
    <main>
        <section>
            <h2>Login</h2>
            <p>In order to play you need the password.</p>
            <p>If you know it, please enter it below and login.</p>
        </section>
        <fieldset>
            <form method="POST" action=<?php echo $_SERVER["PHP_SELF"]; ?>>
                <label for="password">Password:</label>
                <input type="password" id="password" name="submittedPassword">
                <input type="submit" value="Login">
            </form>
        </fieldset>
        <?php
        if ($invalidPassword) {
            echo "<p>Invalid password!</p>";
        }
        ?>
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>