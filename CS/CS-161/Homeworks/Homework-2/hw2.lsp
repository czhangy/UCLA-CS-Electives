; ------------------------------------------------------------------------------
; Question 1 - BFS
; BFS takes in a single argument FRINGE, representing a list of nodes and
; returns a list of leaf nodes, ordered by how they are visited in a breadth-
; first search. The initial call to BFS passes the root of the search tree in
; FRINGE.
;   - Trees are represented as follows:
;       - If the tree contains a single leaf node L, it can be represented by
;         atom L
;       - If the tree has more than one node and is rooted at N, then it can be
;         represented by a list (S1 S2 ... Sk) where Si represents the ith
;         subtree of N
; ------------------------------------------------------------------------------

; This implementation starts by checking if FRINGE is empty, and terminating the
; search if so. If FRINGE is not empty, this code then checks if the first
; element of FRINGE is an atom. If it is, we know it's a leaf node, so we cons
; the first element with the result of continuing BFS on later elements.
; Finally, if none of these cases trigger, we know the first element of FRINGE
; is a subtree, so we append the subtree to the end of FRINGE, allowing FRINGE
; to act as a FIFO queue to facilitate BFS.
(defun BFS (FRINGE)
    (cond
        ; Empty list base case
        ((not FRINGE) NIL)
        ; Current element is leaf node, cons and explore
        ((atom (car FRINGE))
            (cons (car FRINGE) (BFS (cdr FRINGE))))
        ; Current element is subtree, append to FRINGE and explore
        (T (BFS (append (cdr FRINGE) (car FRINGE))))
    )
)

; BFS test function
;; (defun BFS_TEST ()
;;     (format t "BFS tests:~%")
;;     (format t "(BFS '(ROOT)):~%> ~D~%" (BFS '(ROOT)))
;;     (format t "(BFS '((((L E) F) T))):~%> ~D~%" (BFS '((((L E) F) T))))
;;     (format t "(BFS '((R (I (G (H T)))))):~%> ~D~%" (BFS '((R (I (G (H T)))))))
;;     (format t "(BFS '(((A (B)) C (D)))):~%> ~D~%" (BFS '(((A (B)) C (D)))))
;;     (format t "(BFS '((T (H R E) E))):~%> ~D~%" (BFS '((T (H R E) E))))
;;     (format t "(BFS '((A ((C ((E) D)) B)))):~%> ~D~%" (BFS '((A ((C ((E) D)) B)))))
;;     t
;; )

; ------------------------------------------------------------------------------
; Question 2 - DFS
; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; - FINAL-STATE takes a single argument S, the current state, and returns T if
;   it is the goal state (T T T T) and NIL otherwise.
; - NEXT-STATE returns the state that results from applying an operator to the
;   current state.
;       - It takes three arguments: the current state (S), and which entity to
;         move (A, equal to h for homer only, b for homer with baby, d for
;         homer with dog, and p for homer with poison). 
;       - It returns a list containing the state that results from that move.
;       - If applying this operator results in an invalid state (because the
;         dog and baby, or poison and baby are left unsupervised on one side of
;         the river), or when the action is impossible (homer is not on the same
;         side as the entity) it returns NIL.
;       - NOTE that NEXT-STATE returns a list containing the successor state
;         (which is itself a list); the return should look something like
;         ((NIL NIL T T)).
; - SUCC-FN returns all of the possible legal successor states to the current
;   state.
;       - It takes a single argument (s), which encodes the current state, and
;         returns a list of each state that can be reached by applying legal
;         operators to the current state.
; - ON-PATH checks whether the current state is on the stack of states visited
;   by this depth-first search.
;       - It takes two arguments: the current state (S) and the stack of states
;         visited by DFS (STATES).
;       - It returns T if s is a member of states and NIL otherwise.
; - MULT-DFS is a helper function for DFS.
;       - It takes two arguments: a list of states from the initial state to
;         the current state (PATH), and the legal successor states to the last,
;         current state in the PATH (STATES).
;           - PATH is a first-in first-out list of states; that is, the first
;             element is the initial state for the current search and the last
;             element is the most recent state explored.
;       - MULT-DFS does a depth-first search on each element of STATES in turn.
;       - If any of those searches reaches the final state, MULT-DFS returns the
;         complete path from the initial state to the goal state.
;       - Otherwise, it returns NIL.
; - DFS does a depth first search from a given state to the goal state.
;       - It takes two arguments: a state (S) and the path from the initial
;         state to S (PATH). 
;           - If S is the initial state in our search, PATH is set to NIL.
;       - DFS performs a depth-first search starting at the given state.
;       - It returns the path from the initial state to the goal state, if any,
;         or NIL otherwise.
;       - DFS is responsible for checking if S is already the goal state, as
;         well as for ensuring that the depth-first search does not revisit a
;         node already on the search path.
; ------------------------------------------------------------------------------

; This implementation checks if S is equal to the goal state in list form: 
; (T T T T)
(defun FINAL-STATE (S)
    (equal S '(T T T T))
)

; This implementation first checks for the action type specified by A. Once it
; knows what action to execute, it then checks if the current state can execute
; the specified action and end up in a valid state. If it cannot, it returns
; NIL. If it can, then we "move" the desired components by inverting their
; values in the state, returning a single-element list, containing the resultant
; state.
(defun NEXT-STATE (S A)
    (cond
        ; Move Homer only
        ((eql A 'h)
            (cond
                ; Don't leave dog and baby alone
                ((and (eql (first S) (second S)) (eql (second S) (third S)) (not (eql (third S) (fourth S)))) NIL)
                ; Don't leave poison and baby alone
                ((and (eql (first S) (second S)) (eql (second S) (fourth S)) (not (eql (third S) (fourth S)))) NIL)
                ; Move Homer
                (T (list (list (not (first S)) (second S) (third S) (fourth S))))
            )
        )
        ; Move Homer and baby
        ((eql A 'b)
            (cond
                ; Make sure both are on the same side
                ((not (eql (first S) (second S))) NIL)
                ; Move Homer and the baby
                (T (list (list (not (first S)) (not (second S)) (third S) (fourth S))))
            )
        )
        ; Move Homer and dog
        ((eql A 'd)
            (cond
                ; Make sure both are on the same side
                ((not (eql (first S) (third S))) NIL)
                ; Don't leave poison and baby alone
                ((or (equal S '(T T T T)) (equal S '(NIL NIL NIL NIL))) NIL)
                ; Move Homer and the dog
                (T (list (list (not (first S)) (second S) (not (third S)) (fourth S))))
            )
        )
        ; Move Homer and poison
        ((eql A 'p)
            (cond
                ; Make sure both on the same side
                ((not (eql (first S) (fourth S))) NIL)
                ; Don't leave dog and baby alone
                ((or (equal S '(T T T T)) (equal S '(NIL NIL NIL NIL))) NIL)
                ; Move Homer and the poison
                (T (list (list (not (first S)) (second S) (third S) (not (fourth S)))))
            )
        )
    )
)

; This implementation discovers all valid states that can be expanded from S
; by appending the result of calling NEXT-STATE on S with all four actions to
; each other.
(defun SUCC-FN (S)
    (append
        ; Try to move Homer
        (NEXT-STATE S 'h)
        ; Try to move Homer + baby
        (NEXT-STATE S 'b)
        ; Try to move Homer + dog
        (NEXT-STATE S 'd)
        ; Try to move Homer + poison
        (NEXT-STATE S 'p)
    )
)

; This implementation searches STATES one element at a time, checking if the
; head of STATES is equal to the state S. If the end of STATES is reached and S
; has not been found, we know STATES doesn't contain S
(defun ON-PATH (S STATES)
    (cond
        ; End of STATES reached => S not found
        ((not STATES) NIL)
        ; S found in STATES => T
        ((equal S (car STATES)) T)
        ; Recurse through STATES
        (T (ON-PATH S (cdr STATES)))
    )
)

; This implementation first checks if there are any valid successors of the
; current state left to be checked. If there aren't NIL is returned. Next, the
; solution calls DFS with the current successor to check if it can reach a goal
; state. If it can, it returns the solution path generated by DFS. Otherwise, it
; tries the next successor.
(defun MULT-DFS (STATES PATH)
    (cond
        ; Nothing left to search
        ((not STATES) NIL)
        ; Solution found using current states, return the path
        ((DFS (car STATES) PATH) (DFS (car STATES) PATH))
        ; Move to next state
        (T (MULT-DFS (cdr STATES) PATH))
    )
)

; This implementation first checks if the current state has already been found
; on the path using ON-PATH. If it has, NIL is returned. Next, it checks if the
; current state is a goal state, in which case the current state is appended to
; the path, and the new path is returned to indicate a solution has been found.
; Otherwise, this function calls MULT-DFS with the successors of S generated by
; SUCC-FN, and the current state appended to the PATH to continue searching the
; current state's successors
(defun DFS (S PATH)
    (cond
        ; State already searched => end search
        ((ON-PATH S PATH) NIL)
        ; State is goal state => append current state to path and end search
        ((FINAL-STATE S) (append PATH (list S)))
        ; State is possibly on path => get children and search
        (T (MULT-DFS (SUCC-FN S) (append PATH (list S))))
    )
)
