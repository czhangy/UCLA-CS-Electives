#!/usr/local/bin/php

<!DOCTYPE html>
<html lang="en">


<head>
    <meta charset="UTF-8">
    <link rel="stylesheet" href="primes.css">
    <title>PIC 40A Final</title>
    <script src="primes.js" defer></script>
</head>


<body>
    <header>
        <h1>PIC 40A Final - Primes</h1>
    </header>


    <main>
        <span>
            <label for="n">n = </label>
            <input id="n" type="text">
            <input id="btn1" type="button" value="change the background of non-trivial multiples of n">

            <br>

            <label for="color">color = </label>
            <input id="color" type="text">
            <input id="btn2" type="button" value="use the specified color for multiples of n">
        </span>


        <br><br>


        <table>
            <tbody>
                <?php
                for ($i = 0; $i < 10; ++$i) {
                    echo '<tr>';

                    for ($j = 0; $j < 10; ++$j) {
                        if ($i === 0 && (($j === 0) || ($j === 1))) {
                            echo '<td class="not_prime">';
                        } else {
                            echo '<td>';
                        }

                        echo $i * 10 + $j;
                        echo '</td>';
                    }

                    echo '</tr>';
                }
                echo "\n";
                ?>
            </tbody>
        </table>


        <br><br>


        <span>
            <label for="N">N = </label>
            <input id="N" type="text">
            <input id="btn3" type="button" value="Get N-th prime number">
        </span>

        <p id="Nth_prime_info"></p>
    </main>


    <footer>
        <hr>
        <small>
            &copy; Michael Andrews, 2023
        </small>
    </footer>
</body>

</html>