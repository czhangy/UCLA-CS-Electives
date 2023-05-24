# Homework 6

## To-Do

### README

- [ ] Contains your PIC username (`charleszhang`)

### PHP

- [ ] One file named `tidy.php`
- [ ] HTML
  - [ ] `<title>` should be set to "PIC 40A Demo"
  - [ ] There should be an `<h1>` inside a `<header>` that says "PIC 40A Demo - Tidy Trailing Space"
  - [ ] There should be a `<form>` with `type="file"`
    - [ ] The `<form>` should have an attribute `accept=".txt"`
    - [ ] The `<form>` should have an attribute `action=<?php echo $_SERVER['PHP_SELF']; ?>`
    - [ ] Should have an `<input>` with `type="submit"`
  - [ ] Should have a `<footer>` containing a copyright warning
  - [ ] Should [validate](https://validator.w3.org/)
- [ ] PHP
  - [ ] Submitting a file should cause the file to be tidied
    - [ ] A new file with the name `$tidy_{FILE_NAME}.txt` should be downloaded automatically
    - [ ] Each line in the original file should be rewritten with `rtrim()` called on the contents of every line (other than the terminating LF or CRLF)
  - [ ] Should not create any new files during this process
  - [ ] Can be completed with only `substr()` and `rtrim()`

### Testing

- [ ] Submitting the form without selecting a file should reload the page with no changes
- [ ] No new files should be present on the PIC server
- [ ] Tidying `mac.txt` should create a file `tidy_mac.txt` of size 477B
- [ ] Tidying `windows.txt` should create a file `tidy_windows.txt` of size 506B

### Submission

- [ ] The submission should include two files:
  - [ ] `README.txt`
  - [ ] `tidy.php`
- [ ] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)
- [ ] Submit to the [PIC server](https://www.pic.ucla.edu/~charleszhang/HW6/tidy.php)