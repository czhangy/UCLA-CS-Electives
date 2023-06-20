#!/usr/local/bin/php
<?php


header('Content-Type: text/plain; charset=utf-8');


$N = (int) $_POST['N'];


if ($N >= 1) {
  $count = 0;
  $candidate = 2;

  while (true) {
    $candidateIsPrime = true;

    for ($d = 2; $d < $candidate; ++$d) {
      if ($candidate % $d === 0) {
        $candidateIsPrime = false;
        break;
      }
    }

    if ($candidateIsPrime) {
      if (++$count === $N) {
        echo $candidate;
        break;
      }
    }

    ++$candidate;
  }
}


?>
