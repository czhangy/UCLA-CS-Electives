// Elements
const btn1 = document.getElementById("btn1");
const btn2 = document.getElementById("btn2");
const btn3 = document.getElementById("btn3");
const nInput = document.getElementById("n");
const colorInput = document.getElementById("color");
const NInput = document.getElementById("N");
const nums = document.getElementsByTagName("td");

// Event listeners
btn1.addEventListener("click", () => changeBackground());
btn2.addEventListener("click", () => changeColor());
btn3.addEventListener("click", () => nthPrime());

// Change background of multiples of n
function changeBackground() {
	const n = parseInt(nInput.value);
	if (n === NaN || n < 2 || n > 99) {
		return;
	}
	for (let i = n + n; i <= 99; i += n) {
		nums[i].style.backgroundColor = "black";
	}
}

// Change color
function changeColor() {
	const n = parseInt(nInput.value);
	const color = colorInput.value;
	if (n === NaN || n < 2 || n > 99) {
		return;
	}
	for (let i = n + n; i <= 99; i += n) {
		nums[i].style.color = color;
	}
}

function nthPrime() {
	const n = parseInt(NInput.value);
	if (n < 1 || n > 10000) {
		return;
	}
	const request = new XMLHttpRequest();
	request.onload = function () {
		if (this.status === 200) {
			document.getElementById(
				"Nth_prime_info"
			).innerHTML = `The ${NInput.value}-th prime is ${this.responseText}. `;
			sumOfSquares(this.responseText);
		}
	};
	request.open("POST", "Nth_prime.php");
	request.setRequestHeader(
		"Content-type",
		"application/x-www-form-urlencoded"
	);
	request.send(`N=${NInput.value}`);
}

function sumOfSquares(prime) {
	const request = new XMLHttpRequest();
	request.onload = function () {
		if (this.status === 200) {
			document.getElementById("Nth_prime_info").innerHTML +=
				this.responseText;
		}
	};
	request.open("POST", "sum_of_squares.php");
	request.setRequestHeader(
		"Content-type",
		"application/x-www-form-urlencoded"
	);
	request.send(`p=${prime}`);
}
