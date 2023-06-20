#!/usr/local/bin/php
<?php


header('Content-Type: text/plain; charset=utf-8');


$p = (int) $_POST['p'];


if ($p % 4 === 3) {
  echo 'It is not the sum of two positive squares.';
  exit;
}


echo 'It is equal to ';

for ($i = 1; $i < sqrt($p); ++$i) {
  $j = (int) sqrt($p - $i * $i);

  if ($i * $i  +  $j * $j  ===  $p) {
    echo "{$i}^2 + {$j}^2", '.';
    exit;
  }
}


?>
