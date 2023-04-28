# Homework 3

## To-Do

### HTML

- [x] The tab should be titled "Shut The Box"
- [x] There should be a header displaying a heading "Shut The Box"
- [x] There should be a section describing the rules using i, ii, iii, ... numbering
- [x] There should be a section with a heading that says "Dice roll"
  - [x] Under the heading, there should be a button that says "Roll dice"
  - [x] There should be a space which will display the result of a dice roll, made with a `<span>` element
  - [x] The button and space should be wrapped by a `<fieldset>` element
- [x] There should be a section with a heading that says "Box selection"
  - [x] Under the heading, there should be a `<table>` element
  - [x] The table head should contain the numbers 1-9
  - [x] The table body should contain corresponding checkboxes
  - [x] There should be no table foot
  - [x] Under the table, there should be two buttons in a `<fieldset>` element that say "Submit box selection" and "I give up"
- [x] There should be a footer with copyright information
- [x] HTML should be [validated](https://validator.w3.org/)

### JavaScript

- [ ] Clicking a number or the corresponding checkbox toggles the checkbox
  - [ ] This should be done by adding event listeners to the `<td>` elements
- [ ] All buttons should be given functionality through the use of event listeners
  - [ ] When the "Roll dice" button is enabled, the "Submit box selection" button is disabled and vice versa
  - [ ] When the page is initially loaded, the "Roll dice" button is enabled
- [ ] When "Roll dice" is clicked the button must become disabled
  - [ ] The result of a dice roll must be displayed in the `<span>`
  - [ ] The "Submit box selection" button must be enabled
- [ ] When "Submit box selection" is clicked one of the following should happen
  - [ ] If the move is invalid, an alert should be displayed stating that the move is invalid
  - [ ] If the move is valid, the used checkboxes must be unchecked and disabled
  - [ ] If the remaining boxes sum to less than or equal to 6, then one die is used instead of two
  - [ ] The last dice roll is cleared
- [ ] When "I give up" is clicked, all buttons are disabled and the user of the page receives an alert telling them their score

### README

- [ ] Should contain PIC username

### Submission

- [ ] All code must be uploaded to the [PIC server](www.pic.ucla.edu/~charleszhang/HW3/shut_the_box.html)
- [ ] All code must be uploaded to Gradescope