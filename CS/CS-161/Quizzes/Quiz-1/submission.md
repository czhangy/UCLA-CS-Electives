# CS 161 Quiz 1

## Problem 1

Which of the following four definitions of AI aims to study the rational agent?

- [ ] Acting humanly
- [x] Acting rationally
- [ ] Thinking rationally
- [ ] Thinking humanly

## Problem 2

Given the function definition as follows:

```lisp
(defun F(x)
  (cond
    ((not x) 0)
    ((atom x) 1)
    (t (+ (F (car x)) (F (cdr x))))
  )
)
```

What is the result of the expression:

```lisp
(F '(a (b c) d e))
```

- [ ] 4
- [x] 5
- [ ] 1
- [ ] 3

## Problem 3

What is the result of the following LISP expression?

```lisp
(cdr (car (cons '(4 5) '(1 2 3))))
```

- [x] (5)
- [ ] 5
- [ ] 1
- [ ] (1 2 3)
- [ ] ERROR

## Problem 4

When formulating a search problem, we need to take what of the following into consideration?

- [ ] Initial state
- [ ] Cost function
- [ ] Transition model
- [ ] State space
- [x] All other options
- [ ] Action space

