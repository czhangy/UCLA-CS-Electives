# Homework 8

## To-Do

### README

- [x] Should contain PIC username (`charleszhang`)

### PHP

- [x] `login.php` and `h_password.txt`
  - No changes from HW7 are necessary
- [x] `welcome.php`
  - No changes from HW7 are necessary
- [x] `shut_the_box.php`
  - [x] Should change one line
- [x] `score.php`
  - [x] Should write information to `scores.txt` if accessed from `shut_the_box.php`
  - [x] Should not write any information if opened directly

- [x] `scores.php`
  - [x] HTML
    - [x] Tab should be titled "Shut The Box"
    - [x] Should have a header displaying "Shut The Box"
    - [x] Should have a section with a heading "Scores"
      - [x] Should have a paragraph saying "Well done! Here are the scores so far..."
      - [x] Should have a paragraph where the scores will be displayed (empty initially)

    - [x] Should be one `<fieldset>` with a button saying "PLAY AGAIN!!!"
    - [x] Should have another `<fieldset>` with:
      - [x] One button saying "Force update / start updating"
      - [x] One button saying "Stop updating"

    - [x] `<footer>` with copyright information

  - [x] PHP
    - [x] Use sessions so that if a user is not logged in, they are redirected back to `login.php`


### JS

- [x] `username.js`
  - No changes from HW7 are necessary
- [x] `welcome.js`
  - No changes from HW7 are necessary
- [x] `shut_the_box.js`
  - [x] `endGame()`
    - [x] Should make an AJAX `POST` request to send the user's username and score to `score.php`
    - [x] Redirect to `scores.php` if the `POST` was successful

- [x] `scores.js`
  - [x] Should make sure scores are updated every 8 seconds (without refreshing the page)
  - [x] The "PLAY AGAIN!!!" button should redirect a user to `welcome.php`
  - [x] The "Force update / start updating" button should update the scores immediately
  - [x] "Stop updating" should stop the scores from being automatically updated


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
  - `scores.txt`
  - `score.php`
  - `scores.php`
  - `scores.js`
  
- [x] Submit to the [PIC server](http://www.pic.ucla.edu/~charleszhang/HW8)
  - [x] Ensure `scores.txt` is an empty text file
  - [x] Make sure a folder named `sessions` with 755 permissions exists
- [x] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)
  - Don't submit `scores.txt` to Gradescope