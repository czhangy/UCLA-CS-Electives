; ------------------------------------------------------------------------------
; Question 1 - PAD
; PAD takes in a single integer N and returns the Nth value of the Padovan
; sequence
;   - The Padovan sequence is defined by: PAD(N + 1) = PAD(N - 1) + PAD(N - 2),
;                                         PAD(0) = PAD(1) = PAD(2) = 1
; ------------------------------------------------------------------------------

; This implementation checks the base cases (PAD(0) = PAD(1) = PAD(2) = 1) of
; the Padovan sequence. If the value isn't a base case, it recursively calls
; itself twice, once with N - 2 as the argument, and once with N - 3 as the
; argument, and adds the results. This recursive case implements the recurrence
; relation PAD(N + 1) = PAD(N - 1) + PAD(N - 2).
(defun PAD (N)
    ; Check the base case
    (if (or (= N 0) (= N 1) (= N 2))
        ; Handle the base case, N <= 2
        1
        ; Handle the common case, N > 2
        (+ 
            ; Add PAD(N - 2) and PAD(N - 3) for PAD(N)
            (PAD (- N 2))
            (PAD (- N 3))
        )
    )
)

; Function to test PAD
;; (defun PAD_TEST ()
;;     (format t "PAD tests:~%")
;;     (loop for N from 0 to 10
;;         do (format t "PAD(~D): ~D~%" N (PAD N))
;;     )
;;     t
;; )

; ------------------------------------------------------------------------------
; Question 2 - SUMS
; SUMS takes in a single integer N and returns the number of summations required
; by PAD to compute the Nth value of the Padovan sequence
;   - The implementation should not use PAD
; ------------------------------------------------------------------------------

; This implementation mirrors the PAD implementation. We first check the base
; cases of the Padovan sequence, which all require 0 summations to compute. We
; then note that, in all other cases, each call to PAD generates a single
; summation. Therefore, we recursively call SUMS with N - 2 and N - 3 arguments,
; adding 1 for each call to represent the single summation per PAD call.
(defun SUMS (N)
    ; Check the base case
    (if (or (= N 0) (= N 1) (= N 2))
        ; Handle the base case, N <= 2
        0
        ; Handle the common case, N > 2
        (+
            ; Add 1 for each PAD call, as each call to PAD adds exactly 1 time
            1
            ; Recurse for (N - 2) and (N - 3)
            (SUMS (- N 2))
            (SUMS (- N 3))
        )
    )
)

; Function to test SUMS
;; (defun SUMS_TEST ()
;;     (format t "SUMS tests:~%")
;;     (loop for N from 0 to 10
;;         do (format t "SUMS(~D): ~D~%" N (SUMS N))
;;     )
;;     t
;; )

; ------------------------------------------------------------------------------
; Question 3 - ANON
; ANON takes in a single tree TREE and returns a tree with the same structure as
; TREE, but with symbols and numbers replaced by a ?
;   - Trees are represented as follows:
;       - If the tree contains a single leaf node L, it can be represented by
;         atom L
;       - If the tree has more than one node and is rooted at N, then it can be
;         represented by a list (S1 S2 ... Sk) where Si represents the ith
;         subtree of N
; ------------------------------------------------------------------------------

; This implementation first checks for the empty list, and returns nil. It then
; checks if the current "tree" is a single atom, which represents a leaf node.
; In that case, the function returns ?, replacing the atom in the input with ?.
; Finally, in the recursive case, this function appends the result of calling
; itself on the current node (head of TREE) to the result of calling itself on
; the tail of the list. This should generate a deep search of the list,
; replacing all symbols with ?.
(defun ANON (TREE)
    ; Handle base cases
    (cond
        ; Base case, end of list reached
        ((not TREE) nil)
        ; Base case, current element is an atom
        ((atom TREE) '?)
        ; Common case, recurse through subtree
        (t (cons (ANON (car TREE))
                 (ANON (cdr TREE))
           )
        )
    )
)

; Function to test ANON
;; (defun ANON_TEST ()
;;     (format t "ANON tests:~%")
;;     (format t "(ANON ’42):~% ~D~%" (ANON '42))
;;     (format t "(ANON ’FOO)::~% ~D~%" (ANON 'FOO))
;;     (format t "(ANON ’(((L E) F) T))::~% ~D~%" (ANON '(((L E) F) T)))
;;     (format t "(ANON ’(5 FOO 3.1 -0.2))::~% ~D~%" (ANON '(5 FOO 3.1 -0.2)))
;;     (format t "(ANON ’(1 (FOO 3.1) -0.2))::~% ~D~%" (ANON '(1 (FOO 3.1) -0.2)))
;;     (format t "(ANON ’(((1 2) (FOO 3.1)) (BAR -0.2)))::~% ~D~%" (ANON '(((1 2) (FOO 3.1)) (BAR -0.2))))
;;     (format t "(ANON ’(R (I (G (H T)))))::~% ~D~%" (ANON '(R (I (G (H T))))))
;;     t
;; )
