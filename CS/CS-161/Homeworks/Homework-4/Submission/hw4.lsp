; ------------------------------------------------------------------------------
; General Utility Functions
; ------------------------------------------------------------------------------

(defun reload()
    (load "hw4.lsp")
)

; ------------------------------------------------------------------------------
; Exercises
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Returns the index of the variable that corresponds to the fact that "node n
; gets color c" (when there are k possible colors).
;   - n is a node index
;   - c is a color index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun node2var (n c k)
    ; Return based on variable index = (n - 1) * k + c
    (+ (* (- n 1) k) c)
)

; ------------------------------------------------------------------------------
; Returns a clause for the constraint: "node n gets at least one color from the
; set { c, c + 1,..., k }."
;   - n is a node index
;   - c is the current color index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun at-least-one-color (n c k)
    ; Check base case, where all colors have been applied
    (if (= c k)
        ; End recursion
        (list (node2var n c k))
        ; Recurse through { c + 1, ... , k }
        (cons (node2var n c k) (at-least-one-color n (+ c 1) k))
    )
)

; ------------------------------------------------------------------------------
; at-most-one-color Helper Functions
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Returns the negative form of a positive integer.
;   - num is the number to negate
; ------------------------------------------------------------------------------
(defun negate (num)
    (- 0 num)
)

; ------------------------------------------------------------------------------
; Returns a list of clauses representing the negation of node n being colored
; with color cur and all colors [c, k].
;   - n is a node index
;   - cur is the color index
;   - c is the current color index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun buildPairs (n cur c k)
    ; Check base case, where all colors have been applied
    (if (= c k)
        ; Return a pair consisting of cur and the last color, ending recursion
        (list (list (negate (node2var n cur k)) (negate (node2var n c k))))
        ; Build a pair consisting of cur and the current color and recurse
        (cons
            (list (negate (node2var n cur k)) (negate (node2var n c k)))
            (buildPairs n cur (+ c 1) k)
        )
    )
)

; ------------------------------------------------------------------------------
; Returns a list of clauses for the constraint: "node n gets at most one color
; from the set { c, c + 1,..., k }."
;   - n is a node index
;   - c is the starting color index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun at-most-one-color (n c k)
    ; Check base case, no more pairs to build clauses with
    (if (= c k)
        ; Return the empty list
        NIL
        ; Build pairs using the current color and recurse
        (append
            (buildPairs n c (+ c 1) k)
            (at-most-one-color n (+ c 1) k)
        )
    )
)

; ------------------------------------------------------------------------------
; Returns a list of clauses to ensure that "node n gets exactly one color from
; the set { 1, 2,..., k }."
;   - n is the node index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun generate-node-clauses (n k)
    ; Enforce at-most-one-color and at-least-one-color
    (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
)

; ------------------------------------------------------------------------------
; generate-edge-clauses Helper Function
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Returns a list of clauses enforcing that for a color i from [1, k], either x
; may be that color, or y may be that color, not both.
;   - x is the first node
;   - y is the second node
;   - c is the current color index
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun buildEdgeClauses (x y c k)
    ; Check base case, all colors have been applied
    (if (> c k)
        ; Return empty list
        NIL
        ; Build clauses for current color and recurse
        (append
            (list
                ; x isn't this color or y isn't this color
                (list (negate (node2var x c k)) (negate (node2var y c k)))
            )
            (buildEdgeClauses x y (+ c 1) k)
        )
    )
)

; ------------------------------------------------------------------------------
; Returns a list of clauses to ensure that "the nodes at both ends of edge e
; cannot have the same color from the set { 1, 2,..., k }."
;   - e is an edge (pair)
;   - k is the maximum color index
; ------------------------------------------------------------------------------
(defun generate-edge-clauses (e k)
    ; Begin recursion
    (buildEdgeClauses (first e) (second e) 1 k)
)

; ------------------------------------------------------------------------------
; Top-Level and Utility Functions
; ------------------------------------------------------------------------------

; ------------------------------------------------------------------------------
; Top-level function for converting the graph coloring problem of the graph
; defined in 'fname' using k colors into a SAT problem. The resulting SAT
; problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)

; This function also returns the cnf written to file.
 
; Works only for k > 0
; ------------------------------------------------------------------------------
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      )
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	)
      )
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    ) 
  )

; ------------------------------------------------------------------------------
; A utility function for parsing a pair of integers.
; ------------------------------------------------------------------------------
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

; ------------------------------------------------------------------------------
; Writes clause to file handle 'out'.
; ------------------------------------------------------------------------------
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     )
	   )
	)
  )

; ------------------------------------------------------------------------------
; Writes the formula cnf with vc variables to 'fname'.
; ------------------------------------------------------------------------------
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      )
    (close out)
    )
  )
