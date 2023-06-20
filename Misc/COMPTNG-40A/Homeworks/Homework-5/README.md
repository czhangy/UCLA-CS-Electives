# Homework 5

## To-Do

### README

- [x] `README.txt`
  - [x] Must contain your PIC username (charleszhang)

### HTML

- [x] `site_g.html` and `site_e.html`
  - [x] Tab should be called "Interesting"
  - [x] A header with an `<h1>` saying "Login page for site with interesting content"
  - [x] There should be 2 `<section>`s:
    - [x] A login section
      - [x] Should have an `<h2>` that says "Login"
      - [x] Should have a `<form>` element with `method` set to `"POST"`
      - [x] Should have 2 `<label>` elements that say "Username:" and "Password:"
      - [x] Should have 3 `<input>` elements with `type` set to `"text"`, `"password"`, and `"submit"`
    - [x] A comment section
      - [x] Should have an `<h2>` that says "Recent posts by users"
      - [x] Should have a `<p>` that says "niceGuy666 said, "check out my holiday pictures!"
        - [x] "holiday" should be an `<a>` tag with `target="_blank"` and `href="holiday1.html"`
        - [x] "pictures" should be an `<a>` tag with `target="_blank"` and `href="holiday2.html"`
  - [x] A `<footer>` with copyright information
- [x] `site_g.html`
  - [x] The `<form>` should have `action="welcome.php"`
- [x] `site_e.html`
  - [x] The `<form>` should have `action="phished.php"`
- [x] `holiday1.html`
  - [x] Navigate to https://www.pic.ucla.edu/~mjandr/thisDoesNotExist
  - [x] `console.log` the `outerHTML` of the `<html>` element
  - [x] Copy and paste the result into the file
  - [x] Include `phish.js`
- [x] `holiday2.html`
  - [x] Navigate to http://www.utternonsense.notawebsite.com/afterForwardSlash
  - [x] `console.log` the `outerHTML` of the `<html>` element
  - [x] Copy and paste the result into the file
  - [x] Replace all 8 occurrences of "www.utternonsense.notawebsite.com"
  - [x] Edit the function `reloadButtonClick` so that the `location` is updated to "https://www.pic.ucla.edu/~charleszhang/HW5/holiday2.html" regardless of the value of the parameter `url`
  - [x] Include `phish.js`
- [x] `site_g.html` and `site_e.html` should [validate](https://validator.w3.org/)

### PHP

- [x] `welcome.php`
  - [x] Tab should be called "Interesting"
  - [x] A header with an `<h1>` saying "Welcome to site with interesting content"
  - [x] There should be 2 `<section>`s:
    - [x] A welcome section
      - [x] Should have an `<h2>` that says "Welcome"
      - [x] Should have a `<p>` that says "Welcome ${USERNAME}"
    - [x] A comment section
      - [x] Should have an `<h2>` that says "Recent posts by users"
      - [x] Should have a `<p>` that says "niceGuy666 said, "check out my holiday pictures!""
        - [x] "holiday" should be an `<a>` tag with `target="_blank"` and `href="holiday1.html"`
        - [x] "pictures" should be an `<a>` tag with `target="_blank"` and `href="holiday2.html"`
  - [x] A `<footer>` with copyright information
- [x] `phished.php`
  - [x] Tab should be called "Phished"
  - [x] A header with an `<h1>` saying "HAHAHA"
  - [x] There should be a `<section>`:
    - [x] A `<p>` saying "You just got phished!!!"
    - [x] A `<p>` saying "Your password is ${PASSWORD}"
  - [x] A `<footer>` with copyright information

### JS

- [x] `phish.js`
  - [x] Use `window.opener.location` to redirect the opening tab

### Testing

- [x] `welcome.php` correctly uses the username that was submitted
- [x] Entering username and password information on `site_e.html` and submitting leads to being
  directed to `phished.php`
- [x] `phished.php` correctly uses the password that was submitted
- [x] From `site_g.html`, clicking on the "holiday" anchor opens `holiday1.html` in a new tab and
  redirects `site_g.html` to `site_e.html`
- [x] From `site_g.html`, clicking on the "pictures" anchor opens `holiday2.html` in a new tab and
  redirects `site_g.html` to `site_e.html`
- [x] `holiday1.html` looks like a realistic error message created by the PIC servers
- [x] `holiday2.html` looks like a realistic error message created by Google Chrome
- [x] The reload button on `holiday2.html` appears to try to reload the page and appears to continue
  to result in the page not loading
- [x] Instructions regarding the layout of pages have been followed, so pages appear close to identical
  to the example given in the video

### Submission

- [x] Submission must include:
  - [x] `README.txt`
  - [x] `site_g.html`
  - [x] `site_e.html`
  - [x] `welcome.php`
  - [x] `phished.php`
  - [x] `phish.js`
  - [x] `holiday1.html`
  - [x] `holiday2.html`
- [x] Files must be uploaded to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)
- [x] Files must be uploaded to the [PIC server](http://www.pic.ucla.edu/~charleszhang/HW5/site_g.html)