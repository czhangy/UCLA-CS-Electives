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



## Lecture 13:

- 
