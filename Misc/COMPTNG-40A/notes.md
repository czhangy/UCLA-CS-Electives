# COMPTNG 40A - Spring '23 - Andrews

[TOC]

## Lecture 1: Introduction

Nothing to see here!



## Lecture 2: Basic HTML

Nothing to see here!



## Lecture 3: Primitives and Typing

- Use of `<script>` tag with `src` attribute to inject JS into HTML
  - Use `defer` attribute to force HTML to be parsed first, and then load the JS after
- Use JS to make your pages interactive
- Primitives
  - `string`, `number`, `bigInt`, `boolean`, `undefined`, `simple`
  - `typeof` can be used to determine the type of a literal/variable
- `null`
  - Type `object`
  - Used for values that were previously assigned and are now unassigned
- Typing
  - JS is dynamically typed
  - Variables may change types between assignments
  - `const` keyword prevents the value of variables from being changed after declaration



## Lecture 4: Strings and Type Coercion

- Strings

  - Backticks

    - Can be used for injecting variable values into a string

    - ```js
      let num = 1
      let s = `This is a number: ${num}`
      ```

  - Length

    - String length is a property of the string, not a member function
    - `s.length`

  - Substring Search

    - `s.indexOf(substr)` returns the index of the leftmost matching `substr` in `s` if it exists or `-1` if `substr` is never found

  - Substrings

    - `s.substr(start, len)`
      - `s.substr(i)` returns a substring starting from index `i`
      - `s.substr(i, j)` returns a substring starting from index `i` of length `j`
      - `s.substr(i, k)`, where `k` is too large to be in bounds returns a substring starting from index `i`
      - `s.substr(m)`, where `m` is a negative number, returns the empty string
      - This function is deprecated
    - `s.substring(start, end)`
      - `s.substring(i, j)` returns a substring starting from index `i` until and not including index `j`

  - Indexing

    - `s[i]` returns the character at the `i`th index
      - Negative values of `i` result in an `undefined` value 
    - `s.at(i)` returns the character at the `i`th index
      - Negative values of `i` index in reverse
    - `s.charAt(i)` returns the character at the `i`th index
      - Negative values of `i` returns an empty string
    - Strings are immutable

  - Comparison

    - Comparison occurs character by character (lowercase > uppercase)
    - If strings are equal until end of one string, the longer string is greater

- Type Coercion

  - Implicit Coercion
    - JS uses implicit coercion when comparing between different types
    - Should not be relied on
    - Equality
      - `0 == false` evaluates as true, due to type coercion
      - `0 === false` evaluates as false since `===` prevents the use of type coercion
  - Explicit Coercion
    - Booleans
      - The following values are coerced to `false`: `undefined`, `0`, `NaN`, `''`, `null`
      - The following values are coerced to `true`: any non-zero number, any non-empty string, `[]`, `{}`



## Lecture 5: Type Coercion and Arrays

- Type Coercion (cont.)

  - Implicit Coercion

    - Primitive => String

      - If `v1` or `v2` is a string, this occurs when evaluating `v1 + v2`

    - Primitive => Number

      - Arithmetic operators other than the above `+` are used

    - Primitive => Boolean

      - Control flow

      - Boolean operators (`&&` and `||`)

        - ```js
          console.log('hello' && 0) // 0
          console.log('hello' || 0) // 'hello'
          ```

        - Both operators return the first element that gives them a definite output

- Arrays

  - Member functions
    - `a.push(x)` appends `x` to the end of array `a` and returns the new length of `a`
    - `a.pop()` removes the last element from array `a` and returns the element
    - `a1.concat(a2)` creates a new array with all the elements of `a1` followed by the elements of `a2`
      - Leaves `a1` and `a2` unaffected
  - Indexing
    - Indexing out-of-bounds returns `undefined`
    - Assigning to an out-of-bounds index succeeds and fills with `undefined`

- Objects

  - Object assignment passes by reference



## Lecture 6: Objects, Control Flow, and Functions

- Objects (cont.)
  - `const` objects can still be modified
    - `const` applies to the object's reference, not the contents of the object it refers to
    - `const` protects against assignment
- Control Flow
  - `of` uses C++'s range-based for loop functionality
- Functions
  - Functions without a `return` statement return `undefined` by default
  - Functions are just objects



## Lecture 7: Functions and Scope

- Functions (cont.)

  - Since functions are objects, they can be passed to other functions

    - ```js
      function apply(f, arr) {
        for (let i = 0; i < arr.length; i++) {
          arr[i] = f(arr[i]);
        }
      }
      ```

  - Function expressions can be used to create anonymous functions

    - ```js
      function(x) {
        if (x > 0) {
          return x;
        }
        return -x;
      }
      ```

- Scope

  - Also called a lexical environment

  - Defined by `{}`

  - Closures save lexical environments when necessary

    - ```js
      function greet(name) {
        let msg = name;
        return function() {
          console.log(message);
        }
      }
      
      let greet_me = greet("Me");
      greet_me() // Works
      ```

      

## Lecture 8: Functions and Hoisting

- Functions
  - When passing values into a function, we are assigning the **arguments** into the **parameters**
    - Passing by value => objects have their references passed by value
  - Function parameters can be assigned defaults
    - These defaults are allowed to depend on prior parameters
    - If a parameter has a default expression, all parameters to the right must as well
- `let`
  - Trying to use a `let`-declared variable before declaration results in an error
  - Redeclaring a `let` variable results in an error
  - `let` declares a variable for use in the current lexical environment
    - Not enclosing environments (`{}` and function calls)
  - `const` is the same, but doesn't allow assignments
- `var`
  - Can use variable before declaration
  - Allows redeclarations
  - Doesn't understand `{}`, but still understands functions



## Lecture 9: Objects

- Objects

  - Properties

    - Accessed with the `.` operator on an object
    - Unknown properties are `undefined` by default
    - Properties allow assignment
    - `delete` can be used to remove a property from an object
    - `in` asks if an object can use a property
      - Includes properties obtained through prototypal inheritance
      - `hasOwnProperty` is used to check if an object has a property

  - Classes

    - Constructors can be written as a `function` expression, conventionally denoted with a capitalized name and called with the `new` keyword

      - ```js
        function Person(first, last, user) {
          this.first_name = first;
          this.last_name = last;
          this.username = user;
        }
        
        let person = new Person("Charles", "Zhang", "czhangy");
        ```

      - The final line does the following operations:

        - Creates an empty object
        - Something we discuss later
        - Binds `this` in the function body to the newly created object
        - Have the assigned to variable reference the newly created object
        - Ignores the return value of the constructor if it's a primitive

  - Prototypes

    - Every object has a prototype object
    - `null` is an object by type, but is the only object without a prototype
    - Can be accessed using `Object.getPrototypeOf(obj)`
    - Can be assigned using `Object.setPrototypeOf(obj, proto)`




## Lecture 10: Objects

- Objects
  - `===` compares the references, not the objects
  - `in` keyword can be used to determine if a property exists in an object
  - All objects have a property called `__proto__`, which is the same as `Object.getPrototypeOf(obj)` (this is deprecated)
  - Functions have a property called `prototype`
  - `Class.prototype.__proto__ === Object.prototype`
    - `Object` is a constructor with a `prototype` referencing an object that contains functions that should be inherited by all objects
  - Constructors
    - When a constructor class is used, the `__proto__` property of the new object is made to reference the same object as the `prototype` property of the constructor
    - Therefore, the fill constructor flow is:
      - Creates an empty object
      - Match `__proto__` of the new object to `prototype` of the constructor
      - Binds `this` in the function body to the newly created object
      - Have the assigned to variable reference the newly created object
      - Ignores the return value of the constructor if it's a primitive
    - `Class.prototype.constructor == Class`
  - Functions
    - In function objects, `__proto__` points to the `prototype` of `Function`
    - In `Function`, `__proto__ == prototype`



## Lecture 11: DOM

- DOM
  - The HTML DOM API includes:
    - Access to and control of HTML elements
    - Access to and manipulation of form datas
  - Structure
    - Consists of a hierarchical tree of nodes
      - Nodes for each element
      - Element => node => event target
    - Each document is an instance of `Document`
  - Window functions
    - `alert()` is a basic pop-up in the window
    - `confirm()` is a yes or no pop-up
      - Returns true or false based on user selection
    - `prompt()` is a pop-up that has an input box, returning either the value the user inputs or null if the input is empty
      - Takes an optional string parameter that acts as a default
    - `location.assign()` redirects to a different webpage
    - `onload` can be assigned a function that executes after the page loads
      - More-or-less irrelevant with the use of `defer` in the `script` tag
  - Document functions
    - `getElementsByTagName()` returns an array of all elements of a given tag in the order they appear on the page
    - `innerHTML` is a property of an `HTMLElement` that can be used to modify the contents of a tag



## Lecture 12: Event Listeners

- `addEventListener` can be used to watch for a specific event and call a function
  - `click` should be used over `onclick` to separate HTML and JS



## Lecture 13: Canvas and `this`

- Canvas Elements

  - ```js
    const canvas = document.getElementsByTagName("canvas")[0];
    canvas.width = 400;
    canvas.height = 300;
    
    const context = canvas.getContext("2d");
    context.fillStyle = "blue";
    
    canvas.addEventListener("click", function(e) {
    	const rect = canvas.getBoundingClientRect();
    	const xPos = e.clientX - rect.left;
    	const yPos = e.clientY - rect.top;
    	context.fillRect(xPos, yPos, 4, 4);
    });
    ```

  - ```js
    function Drawing(canvas, width, height, color) {
      this.canvas = canvas
      this.canvas.width = width;
      this.canvas.height = height;
      this.context = this.canvas.getContext("2d");
      this.context.fillStyle = color;
    };
    
    Drawing.prototype.start = function() {
      let self = this;
      self.canvas.addEventListener("click", function(e) {
    		const rect = self.canvas.getBoundingClientRect();
    		const xPos = e.clientX - rect.left;
    		const yPos = e.clientY - rect.top;
    		self.context.fillRect(xPos, yPos, 4, 4);
    	});
    };
    ```

- `this`

  - `this` in a constructor refers to the object it's creating
  - `this` in a member function refers to the caller object
  - `this` in an event handler refers to the listener object
  - `this` anywhere else refers to `window`



## Lecture 14: Cookies

- Cookies
  - Deleted by setting expiration to a `Date` in the past
  - Cookies are stored in `name=value` pairs in the order in which they were edited
    - `name` and `value` can be empty
    - The empty `name` can be assigned without use of `=`
    - Spaces shouldn't be used, but can be in Chrome => leading/ending whitespace in `name` and `value` is trimmed
  - When they're created, they should try to not break anything, but when reading cookies, be ready for anything
- `Date`
  - Components
    - Year: normal
    - Month: 0-indexed
    - Day: 1-indexed
    - Hours/minutes/seconds/milliseconds: 0-indexed
  - Member functions
    - `UTC(time)`
      - Returns the number of milliseconds since the UNIX epoch
    - `toString()`
      - Outputs the `Date` object in a string format with local time
    - `toUTCString()`
      - Outputs the `Date` object in a string format with UTC time
    - `setMinutes()`/`getMinutes()`
      - Sets/gets the minutes of a `Date` object
      - Same for hours/seconds/etc.



## Lecture 15: Cookies

Nothing to see here!



## Lecture 16: Basic PHP

- Main goal:
  - Produce HTML algorithmically
- Basics:
  - Shebang: `#!/usr/local/bin/php`
  - PHP enclosed by `<?php` and `?>`
  - Strings enclosed by `'` within the PHP tags
  - Add `+x` permissions to files
- `echo`
  - Adds string to document contents
- `header()`
  - Communicates the end result's format (HTML by default)
  - Must be the first piece of content in the file (other than the opening PHP tag)
    - No newline between shebang and opening tag
  - `header(Content-Type: text/plain; charset=utf-8)` for plain text
- `phpversion()`
  - Outputs the version of PHP being used
- `$`
  - Prefix for variables
  - Dynamically-typed
  - Can be used in `var_dump()` to print without coercion and with type information
- `'` vs `"`
  - `'` ignores escape sequences (other than backslashes and single quotes) and variables
  - `"` allows for escapes and variables
    - Curly braces can be used to isolate the variable name if needed
- `//`
  - Used to comment
- `&&` vs. `and`
  - `&&` has higher precedence than assignment
  - `and` has lower precedence than assignment
    - Don't use this
  - Same idea for `||` and `or`



## Lecture 17: Strings, Coercions, and Arrays

- `%`

  - Truncates both operands to integers and then performs C++ `%`

- Strings

  - `strlen($str)` returns the number of characters in a string `$str`
  - `[]` can be used to index
    - Assignment works
  - `substr($str, $start)` returns the substring of `$str` starting from index `$start`
  - `strpos($str, $sub)` returns the index at which a substring `$sub` can be found in `$str`
    - Returns `false` if `$sub` cannot be found in `$str`
  - `.` operator can be used to concatenate strings
  - `explode($c, $str)` returns an array of elements of `$str` separated by `$c`
  - `implode($c, $arr)` joins the elements of array `$arr` using `$c`

- `const` Variables

  - Declared with `const`, don't need `$`

- Coercion

  - Casting
    - `(type) $value` coerces `$value` to type `type`
  - Number to Boolean
    - Non-zero values become true, 0 becomes false
  - Boolean to Number
    - True becomes `1`
    - False becomes `0`
  - String to Boolean
    - The string `'0'` and `''` become false
    - Every other string becomes true
  - Boolean to String
    - True becomes `'1'`
    - False becomes `''`
  - Array to Boolean
    - The empty array becomes false
    - Everything else becomes true
  - Number to String
    - Exact copy, but doubles with `.0` become their integer representations
  - String to Number
    - Whitespaces are ignored
    - Leading numbers are converted, with the first non-numeric value terminating the coercion
    - When converting to doubles, `e` can be used for scientific notation
      - `e` is treated as a non-numeric when converting to int
  - Operations
    - Evaluates left to right
    - `.` coerces operands to strings
    - Arithmetic operations coerce operands to numbers

- Arrays

  - Cannot be printed with `echo`, use `var_dump` or `print_r` instead

  - `count($arr)` returns the length of array `$arr`

  - `array_push($arr, $e)` pushes element `$e` to array `$arr`

    - Can push multiple elements at a time

  - `array_pop($arr)` removes the last element from `$arr`

  - Dictionaries

    - ```php
      $dict = ['key1' => 'value1', 'key2' => 'value2'];
      ```

    - `$dict[$key] = $value` sets a key-value pair `($key, $value)` to `$dict`

    - `unset($dict[$key])` removes a key-value pair denoted by `$key` from `$dict`

      - Doesn't make sense to use on non-dictionary arrays



## Lecture 18: Functions, Variables, Control Flow, and PHP with HTML

- Functions

  - Functions are defined using the keyword `function`

    - Otherwise, C++ syntax

    - Default params are allowed

    - ```php
      function foo($bar) {
      	...
      }
      ```

  - Passing by reference can be accomplished using `&`

  - Function names aren't case-sensitive

- Variables

  - Undefined variables evaluate as `NULL`
    - `NULL` is `false` when cast to a boolean
    - Make sure to write control flow carefully to avoid bugs
      - Variable name typos
      - Missing `$`
      - etc.

  - Variables are globally scoped
    - Can be removed from scope with `unset($var)`
    - Can be views as part of the `$GLOBALS` dict

- Control Flow

  - `foreach` can be used to iterate through an array with a range-based for-loop

    - ```php
      foreach($arr as $val) {
        echo $val;
      }
      ```

    - `$val` can be assigned as reference if the underlying array should be mutated

      - When this is done, `unset` must be used after the loop, otherwise bugs can be introduced

    - Can be used with dictionaries:

      - `$dict as $val` to extract values
      - `$dict as $key => $value` to extract key-value pairs

- PHP with HTML

  - `<form>`
    - Form contents are submitted using an `<input type="submit">`
    - `action` attribute specifies a location to be redirected to after submitting the form
      - Gives the location access to a superglobal (e.g., `$_GET`, `$_POST`) that stores what was submitted in a dict

    - `method` defines the method that the form submits with

  - `$_GET`
    - Params appear in URL

  - `$_POST`
    - Better for security




## Lecture 19: Request Methods

- `$_SERVER['REQUEST_METHOD']`
  - Used to access methods (`$_GET`, `$_POST`)
- `$_GET` vs. `$_POST`
  - `$_GET` displays content in a query string in the URL
  - `$_POST` does not transmit data via query string
    - Better for secure communications
  - `isset()` can be used to check if a specific name is set
    - The `name` attribute in `input` tags is used to specify the name of the superglobal that will be submitted
- `<a>`
  - Used to make hyperlinks
  - Can take nested HTML
  - Has a mandatory `href` attribute that specifies where to redirect to
    - Can be given full or relative URLs
  - `title` is an attribute that specifies hover text
  - `download` is a boolean attribute that tells the `<a>` tag to download instead of redirecting
  - `target="_blank"` can be used to redirect in a new tab



## Lecture 20: Files

- `[]`
  - Use of `[]` in the `name` attribute of an input allows for an array submission
- File Submission
  - `type="file"` can be used on an `<input>` tag to submit files
  - Can be accessed with the `$_FILES` superglobal
    - Can then use `['name']` to access the file's name
  - Uploaded files are automatically stored at a location given by `['tmp_name']`
    - Can be moved to a desired location with `move_uploaded_file($currentLocation, $desiredLocation)`
  - `__DIR__` can be used to access the current path
- Files
  - Can be opened with `fopen(${FILE_NAME}, ${MODE})`
    - Returns a resource is successful or `false` if unsuccessful
      - Resources are always coerced to be `true`
    - Can confirm that the file was opened by evaluating the result of `fopen`
    - Can be prepended with `@` to silence warnings
  - Can read a line with `fgets($file)`
    - Takes the first line that has not been previously read and returns it in a string
    - Can check if at EOF with `feof($file)`
  - Can write to a file with `fwrite($file, ${CONTENT})`
    - Saves place from previous writes (since the file has been opened)
    - Setting the mode to `w` writes over existing files while `a` appends on top of any existing file
      - Both create a file if it doesn't already exist
  - Can be closed with `fclose($file)`
    - Should be done every time after you're done with a file



## Lecture 21: PHP Headers and Sessions

- `header()`
  - Must be no space between the shebang and the starting `<php>` tag
  - Cannot `echo` before a header is sent
    - Typing outside `<php>` tags is like `echo`ing
  - Sends HTTP header information
  - `'Location: ${URL}'` redirects to a URL
  - `'Content Disposition: attachement; filename="${FILENAME}"'` signals a file download
    - Anything `echo`ed afterward goes into the new file
    - Not producing anything for the browser to display
  - If not used, a header gets inferred for you
- `exit`
  - Stops a PHP script from executing
- `readfile($file)`
  - Essentially echoes the contents of `$file`
- Sessions
  - Used to allow multiple pages to share common data
    - This data is saved in a file
  - Started with `session_start()`, which must be called before sending headers
    - Can be named with `session_name($name)`
      - If no param is given, it `echo`s the name of the session
      - If a session with the name already exists (as determined by a cookie), then the session is resumed, not started
    - Can specify where data is stored with `session_save_path(__DIR__ . $path)`
    - These options precede `session_start()`
  - The session ID can be `echo`d with `session_id()`
    - Identifies the file name that the session data is stored in
    - Cookie is stored in browser as `${SESSION_NAME}=${SESSION_ID}`
  - Session data can be assigned/accessed using the `$_SESSION` superglobal



## Lecture 22:

- 
