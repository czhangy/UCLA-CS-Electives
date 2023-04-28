let unchecked_total = 45;

let dice_roll = null;

const roll_dice_btn = document.getElementById("roll-dice");
const submit_btn = document.getElementById("submit");
const finish_btn = document.getElementById("give-up");
const roll_result = document.getElementById("roll-result");

const boxes = document.getElementsByTagName("th");
const checkboxes = document.getElementsByTagName("input");

for (let i = 0; i < boxes.length; i++) {
	boxes[i].addEventListener("click", () => {
		checkboxes[i].checked = !checkboxes[i].checked;
	});
}

roll_dice_btn.addEventListener("click", roll_dice);
submit_btn.addEventListener("click", submit);
finish_btn.addEventListener("click", finish);

function roll_dice() {
	dice_roll = Math.floor(Math.random() * 6) + 1;
	if (unchecked_total > 6) {
		dice_roll += Math.floor(Math.random() * 6) + 1;
	}
	roll_result.innerHTML = `Result: ${dice_roll}`;
	roll_dice_btn.disabled = true;
	submit_btn.disabled = false;
}

function sum_checked_values() {
	let sum = 0;
	for (let i = 0; i < checkboxes.length; i++) {
		if (!checkboxes[i].disabled && checkboxes[i].checked) {
			sum += i + 1;
		}
	}
	return sum;
}

function submit() {
	const sum = sum_checked_values();
	if (sum != dice_roll) {
		alert(
			"The total of the boxes you selected does not match the dice roll. Please make another selection and try again."
		);
	} else {
		unchecked_total -= sum;
		for (let checkbox of checkboxes) {
			if (checkbox.checked) {
				checkbox.disabled = true;
			}
		}
		roll_result.innerHTML = `Result:`;
		submit_btn.disabled = true;
		roll_dice_btn.disabled = false;
	}
}

function finish() {
	const buttons = document.getElementsByTagName("button");
	for (let button of buttons) {
		button.disabled = true;
	}
	alert(`Your score is ${unchecked_total}`);
}
