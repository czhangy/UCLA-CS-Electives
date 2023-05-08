function get_username() {
	const cookies = document.cookie.split("; ");
	for (const cookie of cookies) {
		if (cookie.includes("=") && cookie.split("=")[0] === "username") {
			return cookie.substring(9);
		}
	}
	return "";
}
