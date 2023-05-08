#  Homework 4

## To-Do

### HTML

- [x] `welcome.html`
  - [x] Tab titled "Shut the Box"
  - [x] `<header>` displaying "Welcome! Ready to play "Shut The Box"?"
  - [x] Username section
    - [x] `<h2>` saying "Choose a username"
    - [x] `<p>` saying "So that we can post your score(s), please choose a username"
    - [x] `<fieldset>` element with a `<label>`, an `<input type="text">`, and a `<input type="button" value="Submit">`
  - [x] `<footer>` with copyright information
  - [x] HTML must [validate](https://validator.w3.org/)

### JS

- [x] `username.js`
  - [x] `get_username()`
    - [x] Extracts the value corresponding to the name `username` from `document.cookie`
    - [x] If there is no such name, return the empty string
    - [x] Make sure this accounts for the situation where `document.cookie` contains `username; username=user`
- [x] `welcome.js`
  - [x] Should fill the textbox with the user's username if it was discovered in `get_username()`
  - [x] Should add event listeners to trigger submit behavior on button press and on Enter keypress
  - [x] `on_submit()`
    - [x] `check_username(user)`
      - [x] The username should be checked to see if it is between 5 and 40 characters
      - [x] The username doesn't include spaces, commas, semicolons, =, or &
      - [x] Every character is either an alphanumeric or one of "!@#$%^*()-_+[]{}:'|`~<.>/?"
    - [x] `on_invalid_username()`
      - [x] Alert the user in the correct manner
    - [x] `create_cookie(user)`
      - [x] Create a new cookie with name equal to the string "username" and value equal to what the user typed
      - [x] The cookie should expire in an hour
      - [x] Don't specify anything about the path
    - [x] `start_game()`
      - [x] Redirect to `shut_the_box.html`

### Submission

- [ ] Upload the following six files: `README.txt`, `shut_the_box.html`, `shut_the_box.js`, `welcome.html`, `username.js`, `welcome.js`
- [ ] Upload to the [PIC server](www.pic.ucla.edu/~charleszhang/HW4/welcome.html)
- [ ] Upload to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)