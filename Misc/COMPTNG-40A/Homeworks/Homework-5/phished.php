#!/usr/local/bin/php

<!DOCTYPE html>
<html lang="en">
	<head>
		<meta charset="UTF-8" />
		<title>Phished</title>
	</head>
	<body>
		<header>
			<h1>HAHAHA</h1>
		</header>
		<main>
			<section>
                <p>You just got phished!!!</p>
				<p>Your password is <?php echo $_POST['password'] ?></p>
			</section>
		</main>
		<footer>&copy; Charles Zhang, 2023</footer>
	</body>
</html>
