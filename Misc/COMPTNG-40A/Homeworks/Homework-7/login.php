#!/usr/local/bin/php
<?php
if (isset($_POST["submittedPassword"])) {
    echo hash("md5", $_POST["submittedPassword"]);
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
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>