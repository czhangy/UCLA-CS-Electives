// Access elements
const username_input = document.getElementById("username");
const submit_button = document.getElementById("submit-button");

// Get username on load
username_input.defaultValue = get_username();

// Add event listeners
username_input.addEventListener("keypress", function (e) {
	if (e.key == "Enter") {
		on_submit();
	}
});
submit_button.addEventListener("click", function () {
	on_submit();
});

function on_submit() {
	if (check_username(username_input.value)) {
		create_cookie(username_input.value);
		start_game();
	}
}

function check_username(username) {
	let errors = "";
	// Handle bullet point #1
	if (username.length < 5) {
		errors += "Username must be 5 characters or longer.\n";
	}
	if (username.length > 40) {
		errors += "Username cannot be longer than 40 characters.\n";
	}

	// Handle bullet point #2
	if (username.includes(" ")) {
		errors += "Username cannot contain spaces.\n";
	}
	if (username.includes(",")) {
		errors += "Username cannot contain commas.\n";
	}
	if (username.includes(";")) {
		errors += "Username cannot contain semicolons.\n";
	}
	if (username.includes("=")) {
		errors += "Username cannot contain equals signs.\n";
	}
	if (username.includes("&")) {
		errors += "Username cannot contain ampersands.\n";
	}

	// Handle bullet point #3 if bullet points #1 and #2 passed
	if (errors.length === 0) {
		const valid_chars =
			"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^*()-_+[]{}:'|`~<.>/?";
		for (let char of username) {
			if (!valid_chars.includes(char)) {
				errors +=
					"Username can only use characters from the following string:\nabcdefghijklmnopqrstuvwxyz\nABCDEFGHIJKLMNOPQRSTUVWXYZ\n0123456789\n!@#$%^*()-_+[]{}:'|`~<.>/?\n";
				break;
			}
		}
	}

	// Return out
	if (errors.length === 0) {
		return true;
	} else {
		// Remove trailing newline and error
		errors = errors.substring(0, errors.length - 1);
		on_invalid_username(errors);
		return false;
	}
}

function on_invalid_username(errors) {
	alert(errors);
}

function create_cookie(username) {
	let expire = new Date();
	expire.setHours(expire.getHours() + 1);
	document.cookie = `username=${username}; expires=${expire.toUTCString()};`;
}

function start_game() {
	window.location.href = "shut_the_box.html";
}
