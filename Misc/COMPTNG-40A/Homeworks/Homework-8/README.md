# Homework 8

## To-Do

### README

- [x] Should contain PIC username (`charleszhang`)

### PHP

- [ ] `login.php` and `h_password.txt`
  - No changes from HW7 are necessary
- [ ] `welcome.php`
  - No changes from HW7 are necessary
- [ ] `shut_the_box.php`
  - [ ] Should change one line
- [ ] `score.php`
  - [ ] Should write information to `scores.txt` if accessed from `shut_the_box.php`
  - [ ] Should not write any information if opened directly

- [ ] `scores.php`
  - [ ] HTML
    - [ ] Tab should be titled "Shut The Box"
    - [ ] Should have a header displaying "Shut The Box"
    - [ ] Should have a section with a heading "Scores"
      - [ ] Should have a paragraph saying "Well done! Here are the scores so far..."
      - [ ] Should have a paragraph where the scores will be displayed (empty initially)

    - [ ] Should be one `<fieldset>` with a button saying "PLAY AGAIN!!!"
    - [ ] Should have another `<fieldset>` with:
      - [ ] One button saying "Force update / start updating"
      - [ ] One button saying "Stop updating"

    - [ ] `<footer>` with copyright information

  - [ ] PHP
    - [ ] Use sessions so that if a user is not logged in, they are redirected back to `login.php`


### JS

- [ ] `username.js`
  - [ ] No changes from HW7 are necessary
- [ ] `welcome.js`
  - [ ] No changes from HW7 are necessary
- [ ] `shut_the_box.js`
  - [ ] `finish()`
    - [ ] Should make an AJAX `POST` request to send the user's username and score to `score.php`
    - [ ] Redirect to `scores.php` if the `POST` was successful

- [ ] `scores.js`
  - [ ] Should make sure scores are updated every 8 seconds (without refreshing the page)
  - [ ] The "PLAY AGAIN!!!" button should redirect a user to `welcome.php`
  - [ ] The "Force update / start updating" button should update the scores immediately
  - [ ] "Stop updating" should stop the scores from being automatically updated


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
  
- [ ] Submit to the [PIC server](http://www.pic.ucla.edu/~charleszhang/HW8)
  - [ ] Ensure `scores.txt` is an empty text file
  - [ ] Make sure a folder named `sessions` with 755 permissions exists
- [ ] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)
  - Don't submit `scores.txt` to Gradescope