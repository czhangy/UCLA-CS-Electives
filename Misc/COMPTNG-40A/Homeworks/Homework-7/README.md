# Homework 7

## To-Do

### README

- [ ] Should contain PIC username (`charleszhang`)

### PHP

- [ ] `login.php`
  - [ ] HTML
    - [ ] `<title>` should contain "Shut the Box"
    - [ ] `<h1>` in the `<header>` should say "Welcome! Ready to play "Shut The Box"?"
    - [ ] `<section>`
      - [ ] `<h2>` saying "Login"
      - [ ] `<p>` saying "In order to play you need the password."
      - [ ] `<p>` saying "“If you know it, please enter it below and login."
    - [ ] `<fieldset>` containing a `<form>` with `method="POST"`
      - [ ] `<label>` saying "Password:"
      - [ ] `<input>` with `type="text"`
      - [ ] `<input>` with `type=submit` and `value="Login"`
    - [ ] `<footer>` with copyright information
  - [ ] PHP
    - [ ] If the correct password is submitted, a PHP session with the name `shutTheBox` should record the user as being `loggedin`, and the user should be redirected to `welcome.php`
    - [ ] If an incorrect password is submitted, a PHP session with the name `shutTheBox` should record the user as being not `loggedin`, and the user should stay on the same page, with the message "Invalid password!"
  - [ ] `h_password.txt`
    - [ ] Should contain a hashed version of the password "Immutable"
- [ ] `welcome.php`
  - [ ] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
- [ ] `shut_the_box.php`
  - [ ] Use PHP sessions to redirect a user to `login.php` if they are not `loggedin`
  - [ ] Use PHP cookies so that if a user is logged in but has not specified a username, they are redirected back to `welcome.php`
  - [ ] Use PHP and 2 for-loops to create the box selection table

### JS

- [ ] `username.js`
  - [ ] Should be copied from Homework 4
- [ ] `shut_the_box.js`
  - [ ] Should be copied from Homework 4
- [ ] `welcome.js`
  - [ ] Update file extensions from `.html` to `.php`

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