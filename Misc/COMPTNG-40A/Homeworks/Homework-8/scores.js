// Set up click handlers
document
	.getElementById("play-again")
	.addEventListener("click", () => restartGame());
document
	.getElementById("start-update")
	.addEventListener("click", () => startUpdate());
document
	.getElementById("stop-update")
	.addEventListener("click", () => stopUpdate());

let timeoutID = null;

function restartGame() {
	window.location.href = "welcome.php";
}

function startUpdate() {
	stopUpdate();
	makeGETRequest();
}

function stopUpdate() {
	clearTimeout(timeoutID);
}

function makeGETRequest() {
	timeoutID = setTimeout(makeGETRequest, 8000);
	const request = new XMLHttpRequest();
	request.onload = function () {
		if (this.status === 200) {
			document.getElementById("scores").innerHTML += this.responseText
				.split("\n")
				.join("<br>");
		}
	};
	request.open("GET", "scores.txt?v=" + Math.random());
	request.send();
}

makeGETRequest();
