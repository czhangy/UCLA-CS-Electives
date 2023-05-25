# Homework 7

## To-Do

### README

- [x] Should contain PIC username (`charleszhang`)

### PHP

- [x] `login.php`
  - [x] HTML
    - [x] `<title>` should contain "Shut the Box"
    - [x] `<h1>` in the `<header>` should say "Welcome! Ready to play "Shut The Box"?"
    - [x] `<section>`
      - [x] `<h2>` saying "Login"
      - [x] `<p>` saying "In order to play you need the password."
      - [x] `<p>` saying "If you know it, please enter it below and login."
    - [x] `<fieldset>` containing a `<form>` with `method="POST"`
      - [x] `<label>` saying "Password:"
      - [x] `<input>` with `type="password"`
      - [x] `<input>` with `type=submit` and `value="Login"`
    - [x] `<footer>` with copyright information
  - [x] PHP
    - [x] If the correct password is submitted, a PHP session with the name `shutTheBox` should record the user as being `loggedin`, and the user should be redirected to `welcome.php`
    - [x] If an incorrect password is submitted, a PHP session with the name `shutTheBox` should record the user as being not `loggedin`, and the user should stay on the same page, with the message "Invalid password!"
  - [x] `h_password.txt`
    - [x] Should contain a hashed version of the password "Immutable"
  - [x] `sessions`
    - [x] Create a folder in the `HW7` directory with `755` permissions
- [x] `welcome.php`
  - [x] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
- [x] `shut_the_box.php`
  - [x] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
  - [x] Use PHP cookies so that if a user is logged in but has not specified a username, they are redirected back to `welcome.php`
  - [x] Use PHP and 2 for-loops to create the box selection table

### JS

- [x] `username.js`
  - [x] Should be copied from Homework 4
- [x] `shut_the_box.js`
  - [x] Should be copied from Homework 4
- [x] `welcome.js`
  - [x] Update file extensions from `.html` to `.php`

### Testing

- [x] Manually directing to `welcome.php` should force a redirect to `login.php`
- [x] Manually directing to `shut_the_box.php` should force a redirect to `login.php`
- [x] An invalid password should cause `login.php` to refresh and a message saying "Invalid password!" to appear below the form
- [x] Entering the password "Immutable" into `login.php` should redirect the page to `welcome.php`
- [x] Entering an invalid password into `login.php` and then manually directing to `welcome.php` should force a redirect back to `login.php`
- [x] After entering the correct password into `login.php`, `welcome.php` should be active
- [x] Manually directing to `shut_the_box.php` should force a redirect to `welcome.php`
- [x] `welcome.php` should enforce username requirements
- [x] Entering a valid username in `welcome.php` should redirect to `shut_the_box.php`, which should now be active
- [x] `shut_the_box.php` should work as intended

### Submission

- Your submission should include 8 files:
  - `README.txt`
  - `login.php`
  - `h_password.txt`
  - `welcome.php`
  - `shut_the_box.php`
  - `welcome.js`
  - `username.js`
  - `shut_the_box.js`

- [x] Submit to the [PIC server](http://www.pic.ucla.edu/~charleszhang/HW7)
- [ ] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)