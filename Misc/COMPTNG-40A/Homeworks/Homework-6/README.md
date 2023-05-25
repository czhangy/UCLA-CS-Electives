# Homework 6

## To-Do

### README

- [x] One file named `README.txt`
- [x] Contains your PIC username (`charleszhang`)

### PHP

- [x] One file named `tidy.php`
- [x] HTML
  - [x] `<title>` should be set to "PIC 40A Demo"
  - [x] There should be an `<h1>` inside a `<header>` that says "PIC 40A Demo - Tidy Trailing Space"
  - [x] There should be a `<form>` with attribute `action=<?php echo $_SERVER['PHP_SELF']; ?>`
    - [x] There should be an `<input>` with attributes `type="file"`and `accept=".txt"`
    - [x] Should have an `<input>` with `type="submit"`
  - [x] Should have a `<footer>` containing a copyright warning
  - [x] Should [validate](https://validator.w3.org/)
- [ ] PHP
  - [x] Submitting a file should cause the file to be tidied
    - [x] A new file with the name `$tidy_{FILE_NAME}.txt` should be downloaded automatically
    - [x] Each line in the original file should be rewritten with `rtrim()` called on the contents of every line (other than the terminating LF or CRLF)
  - [x] Should not create any new files during this process
  - [x] Can be completed with only `substr()` and `rtrim()`

### Testing

- [x] Submitting the form without selecting a file should reload the page with no changes
- [x] No new files should be present on the PIC server
- [x] Tidying `mac.txt` should create a file `tidy_mac.txt` of size 477B
- [x] Tidying `windows.txt` should create a file `tidy_windows.txt` of size 506B

### Submission

- [x] The submission should include two files:
  - [x] `README.txt`
  - [x] `tidy.php`
- [ ] Submit to [Gradescope](https://bruinlearn.ucla.edu/courses/160942/external_tools/408)
- [x] Submit to the [PIC server](https://www.pic.ucla.edu/~charleszhang/HW6/tidy.php)