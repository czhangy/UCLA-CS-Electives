#!/usr/local/bin/php
<?php
// Check if file has been uploaded
if (isset($_FILES["uploadedFile"]) && is_uploaded_file($_FILES["uploadedFile"]["tmp_name"])) {
    tidy();
}

function tidy()
{
    // Send header to download file after writing
    $newFile = "tidy_" . $_FILES["uploadedFile"]["name"];
    header("Content-Disposition: attachment; filename={$newFile}");

    // Open uploaded file
    $oldFile = @fopen($_FILES["uploadedFile"]["tmp_name"], "r") or die("Can't open file");

    // Parse uploaded file line-by-line
    while (!feof($oldFile)) {
        // Get the next line of the file
        $line = fgets($oldFile);

        // Check for CRLF/LF/"" and write to new file
        if (substr($line, -2) === "\r\n") {
            echo (rtrim($line) . "\r\n");
        } else if (substr($line, -1) === "\n") {
            echo (rtrim($line) . "\n");
        } else {
            echo rtrim($line);
        }
    }

    // Close the file
    fclose($oldFile);

    // Remove the temp file
    unlink($_FILES["uploadedFile"]["tmp_name"]);

    // Stop writing to file
    exit;
}
?>

<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <title>PIC 40A Demo</title>
</head>

<body>
    <header>
        <h1>PIC 40A Demo - Tidy Trailing Space</h1>
    </header>
    <main>
        <form enctype="multipart/form-data" method="POST" action=<?php echo $_SERVER["PHP_SELF"]; ?>>
            <input type="file" accept=".txt" name="uploadedFile">
            <br>
            <input type="submit">
        </form>
    </main>
    <footer>&copy; Charles Zhang, 2023</footer>
</body>

</html>