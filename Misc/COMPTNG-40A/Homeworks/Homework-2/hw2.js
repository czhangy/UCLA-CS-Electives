function roll_dice() {
	const first_roll = Math.floor(Math.random() * 6) + 1;
	const second_roll = Math.floor(Math.random() * 6) + 1;
	return `${first_roll} + ${second_roll} = ${first_roll + second_roll}`;
}

function first_occurrences(arr) {
	let firsts = [];
	for (let x of arr) {
		if (!firsts.includes(x)) {
			firsts.push(x);
		}
	}
	return firsts;
}

function first_minus_second(arr1, arr2) {
	let i = 0;
	while (i < arr1.length) {
		if (arr2.includes(arr1[i])) {
			arr1.splice(i, 1);
		} else {
			i++;
		}
	}
}

function extract_username(cookie) {
	const pairs = cookie.split("; ");
	for (const pair of pairs) {
		if (pair.split("=")[0] === "username") {
			return pair.substring(9);
		}
	}
	return "";
}
