# COMPTNG 40A - Spring '23 - Andrews

[TOC]

## Lecture 1: Introduction

Nothing to see here!



## Lecture 2: Basic HTML

Nothing to see here!



## Lecture 3: Basic JS I

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



## Lecture 4: Basic JS II

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



## Lecture 5: Basic JS III

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



## Lecture 6: Basic JS IV

- Objects (cont.)
  - `const` objects can still be modified
    - `const` applies to the object's reference, not the contents of the object it refers to
    - `const` protects against assignment
- Control Flow
  - `of` uses C++'s range-based for loop functionality
- Functions
  - Functions without a `return` statement return `undefined` by default
  - Functions are just objects



## Lecture 7:

- 