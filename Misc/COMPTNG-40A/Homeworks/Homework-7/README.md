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
- [ ] `welcome.php`
  - [ ] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
- [ ] `shut_the_box.php`
  - [ ] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
  - [ ] Use PHP cookies so that if a user is logged in but has not specified a username, they are redirected back to `welcome.php`
  - [ ] Use PHP and 2 for-loops to create the box selection table

### JS

- [x] `username.js`
  - [x] Should be copied from Homework 4
- [x] `shut_the_box.js`
  - [x] Should be copied from Homework 4
- [x] `welcome.js`
  - [x] Update file extensions from `.html` to `.php`

### Submission

- Create a folder named `sessions` in the `HW7` directory with `755` permissions
- Your submission should include 8 files:
  - `README.txt`
  - `login.php`
  - `h_password.txt`
  - `welcome.php`
  - `shut_the_box.php`
  - `welcome.js`
  - `username.js`
  - `shut_the_box.js`

- [ ] Submit to the [PIC server](http://www.pic.ucla.edu/~charleszhang/HW7)
- [ ] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)