// Game state
let score = 45;
let currentRoll = null;

// Elements
const rollButton = document.getElementById("roll-button");
const submitButton = document.getElementById("submit-button");
const endGameButton = document.getElementById("end-game-button");
const rollResult = document.getElementById("roll-result");
const boxes = document.getElementsByTagName("th");
const checkboxes = document.getElementsByTagName("input");

// Event listeners
rollButton.addEventListener("click", rollDice);
submitButton.addEventListener("click", submit);
endGameButton.addEventListener("click", endGame);
for (let i = 0; i < boxes.length; i++) {
	boxes[i].addEventListener("click", () => {
		toggleCheckbox(i);
	});
}

function toggleCheckbox(i) {
	if (!checkboxes[i].disabled) {
		checkboxes[i].checked = !checkboxes[i].checked;
	}
}

function rollDie() {
	return Math.floor(Math.random() * 6) + 1;
}

function rollDice() {
	const firstRoll = rollDie();
	let secondRoll = null;
	// Roll second die if needed
	if (score > 6) {
		secondRoll = rollDie();
	}
	currentRoll = firstRoll + (secondRoll ? secondRoll : 0);
	// Update UI
	rollResult.innerHTML = `Result: ${firstRoll}${
		secondRoll ? ` + ${secondRoll} = ${currentRoll}` : ""
	}`;
	rollButton.disabled = true;
	submitButton.disabled = false;
}

function sumValues() {
	let sum = 0;
	// Sum up checked values
	for (let i = 0; i < checkboxes.length; i++) {
		if (!checkboxes[i].disabled && checkboxes[i].checked) {
			sum += i + 1;
		}
	}
	return sum;
}

function handleInvalid() {
	// Alert user of invalid input
	alert(
		"The total of the boxes you selected does not match the dice roll. Please make another selection and try again."
	);
}

function handleValid(sum) {
	// Update score
	score -= sum;
	// Disable used checkboxes
	for (let checkbox of checkboxes) {
		if (checkbox.checked) {
			checkbox.checked = false;
			checkbox.disabled = true;
		}
	}
	// Update UI
	rollResult.innerHTML = `Result:`;
	submitButton.disabled = true;
	rollButton.disabled = false;
}

function submit() {
	// Get input
	const sum = sumValues();
	// Check validity of input
	if (sum != currentRoll) {
		handleInvalid();
	} else {
		handleValid(sum);
	}
}

function endGame() {
	// Disable all buttons and checkboxes
	const buttons = document.getElementsByTagName("button");
	for (let button of buttons) {
		button.disabled = true;
	}
	for (let checkbox of checkboxes) {
		checkbox.disabled = true;
	}
	// Alert user of score
	alert(`Your score is ${score}`);
	// Post user's score
	makePOSTRequest();
}

function makePOSTRequest() {
	const request = new XMLHttpRequest();
	request.onload = function () {
		if (this.status === 200) {
			window.location.href = "scores.php";
		}
	};
	request.open("POST", "score.php");
	request.setRequestHeader(
		"Content-Type",
		"application/x-www-form-urlencoded"
	);
	request.send(`username=${get_username()}&score=${score}`);
}
