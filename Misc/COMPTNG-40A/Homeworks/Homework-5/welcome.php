#!/usr/local/bin/php

<!DOCTYPE html>
<html lang="en">
	<head>
		<meta charset="UTF-8" />
		<title>Interesting</title>
	</head>
	<body>
		<header>
			<h1>Welcome to site with interesting content</h1>
		</header>
		<main>
			<section>
				<h2>Welcome</h2>
				<p>Welcome <?php echo $_POST['username'] ?></p>
			</section>
			<section>
				<h2>Recent posts by users</h2>
				<p>
					niceGuy666 said, "check out my
					<a href="holiday1.html" target="_blank">holiday</a>
					<a href="holiday2.html" target="_blank">pictures</a>!"
				</p>
			</section>
		</main>
		<footer>&copy; Charles Zhang, 2023</footer>
	</body>
</html>
