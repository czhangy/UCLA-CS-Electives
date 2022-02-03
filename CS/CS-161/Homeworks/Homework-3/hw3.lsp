; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.

; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;

; Reloads
(defun reload()
    (load "hw3.lsp")
    (load "a-star.lsp")
)

; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
(defun sokoban (s h)
    (a* s #'goal-test #'next-states h)
)

; Grid global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Grid checking predicates
(defun isBlank (v) (= v blank))
(defun isWall (v) (= v wall))
(defun isBox (v) (= v box))
(defun isKeeper (v)(= v keeper))
(defun isStar (v) (= v star))
(defun isBoxStar (v) (= v boxstar))
(defun isKeeperStar (v) (= v keeperstar))

; Helper function of getKeeperPosition
(defun getKeeperColumn (r col)
    (cond 
        ((null r)
            NIL
        )
	    (T
            (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	            col
	            (getKeeperColumn (cdr r) (+ col 1))
	        )
	    )
    )
)

; Returns a list indicating the position of the keeper (c r)
(defun getKeeperPosition (s row)
    (cond 
        ((null s)
            NIL
        )
	    (T
            (let ((x (getKeeperColumn (car s) 0)))
                (if x 
                    (list x row)
                    (getKeeperPosition (cdr s) (+ row 1))
                )
            )
	    )
	)
)

; Returns L with any NIL element removed
(defun cleanUpList (L)
    (cond 
        ((null L)
            NIL
        )
	    (T
            (let ((cur (car L)) (res (cleanUpList (cdr L))))
                (if cur 
                    (cons cur res)
                    res
                )
            )
	    )
	)
)

; Returns true if s is a goal state
; s is the game state
(defun goal-test (s)
    (cond
        ; End of row/grid reached
        ((null s)
            ; No failure found => return T
            T
        )
        ; Check grid square
        ((atom s)
            ; Return if the square is not a box
            (not (isBox s))
        )
        ; Check row
        (T
            ; Test head and tail of state/row
            (and (goal-test (car s)) (goal-test (cdr s)))
        )
    )
)

; Gets the contents of the grid at (r, c)
; S is the game state
; r is the row coordinate
; c is the col coordinate
(defun get-square (S r c)
    ; Check if out of bounds
    (if (or (< r 0) (>= r (length S))
            (< c 0) (>= c (length (car S))))
        ; Return wall if out of bounds
        wall
        ; Get the target value
        (car (nthcdr c (car (nthcdr r S))))
    )
)

; Sets the contents of the grid at (r, c) to v
; Returns NIL if S is not a valid state
; r and c are assumed to be in bounds
; S is the game state
; r is the row coordinate
; c is the col coordinate
; v is the new value
(defun set-square (S r c v)
    ; Check if S is a valid state
    (if (null S)
        ; Fail out immediately
        NIL
        ; Set the value
        (append
            ; Get all rows above the target row
            (butlast S (- (length S) r))
            ; Attach the modified row to all rows below the target row
            (cons
                (append
                    ; Get all row entries to the left of the target entry
                    (butlast (car (nthcdr r S)) (- (length (car S)) c))
                    ; Attach the new value
                    (cons
                        v
                        ; Get all row entries to the right of the target entry
                        (cdr (nthcdr c (car (nthcdr r S))))
                    )
                )
                ; Get all rows below the target row
                (cdr (nthcdr r S))
            )
        )
    )
)

; Moves the keeper from (r, c) to (r + dr, c + dc)
; Returns NIL if the move fails, otherwise returns new state with keeper moved
; Assumes the move is always valid
; S is the game state
; r is the current row of the keeper
; c is the current col of the keeper
; dr is the attempted change in row
; dc is the attempted change in col
(defun move-keeper (S r c dr dc)
    (let* (
        ; Get target coordinates
        (newR (+ r dr))
        (newC (+ c dc))
		; Get current square
		(cur (get-square S r c))
        ; Get target square
        (target (get-square S newR newC))
    )
        (cond
			; Both squares are goals
			((and (isKeeperStar cur) (isStar target))
				; Move the keeper onto the new goal and off of the old goal
				(set-square (set-square S newR newC keeperstar) r c star)
			)
			; Only the current square is a goal
			((isKeeperStar cur)
				; Move the keeper off of the goal
				(set-square (set-square S newR newC keeper) r c star)
			)
			; Only the target square is a goal
			((isStar target)
				; Move the keeper onto the goal
				(set-square (set-square S newR newC keeperstar) r c blank)
			)
			; Neither square is a goal
			(T
				; Move the keeper onto the new square
				(set-square (set-square S newR newC keeper) r c blank)
			)
		)
    )
)

; Attempts to move a box at (r, c) to (r + dr, c + dc)
; Returns NIL if the move fails, otherwise returns new state with box moved
; S is the game state
; r is the current row of the box
; c is the current col of the box
; dr is the attempted change in row
; dc is the attempted change in col
(defun move-box (S r c dr dc)
    (let* (
        ; Get target coordinates
        (newR (+ r dr))
        (newC (+ c dc))
		; Get current square
		(cur (get-square S r c))
        ; Get target square
        (target (get-square S newR newC))
    )
        ; Check if target square is open
        (if (or (isBlank target) (isStar target))
            ; Move the box
            (cond
				; Both squares are goals
				((and (isBoxStar cur) (isStar target))
					; Move the keeper with updated state
					(move-keeper
						; Move the box onto the new goal and off of the old goal
						(set-square (set-square S newR newC boxstar) r c star)
						; Get the keeper's coordinates
						(- r dr) (- c dc) dr dc
					)
				)
				; Only the current square is a goal
				((isBoxStar cur)
					; Move the keeper with updated state
					(move-keeper
						; Move the box off of the goal
						(set-square (set-square S newR newC box) r c star)
						; Get the keeper's coordinates
						(- r dr) (- c dc) dr dc
					)
				)
				; Only the target square is a goal
				((isStar target)
					; Move the keeper with updated state
					(move-keeper
						; Move the box onto the goal
						(set-square (set-square S newR newC boxstar) r c blank)
						; Get the keeper's coordinates
						(- r dr) (- c dc) dr dc
					)
				)
				; Neither square is a goal
				(T
					; Move the keeper with updated state
					(move-keeper
						; Move the box onto the new square
						(set-square (set-square S newR newC box) r c blank)
						; Get the keeper's coordinates
						(- r dr) (- c dc) dr dc
					)
				)
			)
            ; Fail out
            NIL
        )
    )
)

; Returns a new state S' that results from performing a single action on S
; Returns NIL if the resultant state is invalid
; S is the game state
; r is the current row of the keeper
; c is the current col of the keeper
; dr is the attempted change in row
; dc is the attempted change in col
(defun try-move (S r c dr dc)
    (let* (
        ; Get target coordinates
        (newR (+ r dr))
        (newC (+ c dc))
        ; Get target square
        (target (get-square S newR newC))
    )
        (cond
            ; Check if target square is a wall/out of bounds
            ((isWall target)
                ; Fail out
                NIL
            )
            ; Check if target square is a box
            ((or (isBox target) (isBoxStar target))
                ; Attempt to move the box
                (move-box S newR newC dr dc)
            )
            ; Target square is a goal or blank
            (T
                ; Move the keeper
                (move-keeper S r c dr dc)
            )
        )
    )
)

; Returns the possible states that caould result from s with a single action
; s is the game state
(defun next-states (s)
    (let* (
        ; Set pos to the keeper's coordinates
        (pos (getKeeperPosition s 0))
        ; Set r, c to the keeper's row and col
	    (c (car pos))
	    (r (cadr pos))
        ; Get next states
	    (result (list (try-move s r c -1 0) (try-move s r c 1 0)
                      (try-move s r c 0 -1) (try-move s r c 0 1))
        )
    )
        ; Removes NILs
        (cleanUpList result)
    )
)

; Trivial admissible heuristic
; s is the game state
(defun h0 (s)
    ; Return 0, disregarding s
    0
)

; Admissible heuristic that computes the number of misplaced boxes in s
; s is the game state
; We know h1 is admissible because if a box is misplaced, it takes at minimum
; 1 move to place it on a goal (the keeper pushes it once). Therefore, since we
; are simply counting the number of misplaced boxes (adding 1 per box), we are
; guaranteed to never overestimate the number of moves it would take to solve
; the game.
(defun h1 (s)
	(cond
		; End of list reached
		((null s)
			; 0 misplaced boxes in empty state
			0
		)
		; Middle of list
		(T
			; Count number of boxes in current row, add to following rows
			(+ (count box (car s)) (h1 (cdr s)))
		)
	)
)

; getPositions helper
; row is the list containing the current row
; r is the current row number
; c is the current column number
; coords is the list of coordinates containing boxes or goals in row
(defun getColumns (row r c coords)
	; Check if end of row
    (if (null row)
		; Return cols
		coords
		; Otherwise, check (r, c)
		(cond
			; Box found, add (r, c, 'b) to coords
			((isBox (car row))
				(getColumns (cdr row) r (+ c 1) (cons (list r c 'b) coords))
			)
			; Goal found, add (r, c, 'g) to coords
			((isStar (car row))
				(getColumns (cdr row) r (+ c 1) (cons (list r c 'g) coords))
			)
			; Not relevant, move on without adding to coords
			(T
				(getColumns (cdr row) r (+ c 1) coords)
			)
		)
	)
)

; Returns a list indicating the positions of the boxes and goals
; S is the state
; r is the current row number
; coords is the list of coordinates containing boxes or goals in S
(defun getPositions (S r coords)
	; Check if end of state
    (if (null S)
		; If end, return list of coordinates in S
		coords
		; Otherwise, insert relevant coordinates in current row to coords
		; and check next row
		(getPositions (cdr S) (+ r 1) (append (getColumns (car S) r 0 NIL) coords))
	)
)

; Returns a list indicating the positions of the boxes
; coords is the list of coordinates containing boxes or goals
; boxes is the list of coordinates containing boxes
(defun getBoxPositions (coords boxes)
	(cond
		; Check if end of list
		((null coords)
			; Return boxes
			boxes
		)
		; Check if head is a box
		((eql (third (car coords)) 'b)
			; Add simplified head to boxes
			(getBoxPositions
				(cdr coords)
				(cons (list (first (car coords)) (second (car coords))) boxes)
			)
		)
		; Head is not a box
		(T
			; Move on without modifying boxes
			(getBoxPositions (cdr coords) boxes)
		)
	)
)

; Returns a list indicating the positions of the goals
; coords is the list of coordinates containing boxes or goals
; goals is the list of coordinates containing goals
(defun getGoalPositions (coords goals)
	(cond
		; Check if end of list
		((null coords)
			; Return goals
			goals
		)
		; Check if head is a box
		((eql (third (car coords)) 'g)
			; Add simplified head to goals
			(getGoalPositions
				(cdr coords)
				(cons (list (first (car coords)) (second (car coords))) goals)
			)
		)
		; Head is not a goal
		(T
			; Move on without modifying goals
			(getGoalPositions (cdr coords) goals)
		)
	)
)

; Calculates the Manhattan distance between a and b
; a is the first coordinate
; b is the second coordinate
(defun manhattanDist (a b)
	(+
		(abs (- (first a) (first b)))
		(abs (- (second a) (second b)))
	)
)

; Calculates the Manhattan distance for the first perfect matching
; boxes is the list of all box coordinates
; goals is the list of all goal coordinates
; d is the accumulated distance so far
(defun getPerfectMatching (boxes goals d)
	; Check if all boxes/goals have been matched
	(if (or (null boxes) (null goals))
		; Return accumulated distance
		d
		; Match current box with next goal
		(getPerfectMatching
			(cdr boxes)
			(cdr goals)
			(+ d (manhattanDist (car boxes) (car goals)))
		)
	)
)

; Calculates the minimum Manhattan distance between the orig and any coordinate
; in dests
; orig is the origin coordinates;
; dests is a list of all destination coordinates
; minD is the minimum distance so far
; isStart is a boolean flag that tells the function if it should simply set
; minD instead of comparing
(defun getMinDist (orig dests minD isStart)
	(cond
		; All destinations have been searched
		((null dests)
			; Return the minimum distance found
			minD
		)
		; Is first destination being searched
		(isStart
			; Set minD to Manhattan distance and continue
			(getMinDist
				orig
				(cdr dests)
				(manhattanDist orig (car dests))
				NIL
			)
		)
		; Normal case
		(T
			; Update minD accordingly and continue
			(getMinDist
				orig
				(cdr dests)
				(min minD (manhattanDist orig (car dests)))
				NIL
			)
		)
	)
)

; Minimize the distance for a perfect matching of box to goal and the distance
; from keeper to the nearest box
; Idea is to get boxes on goals and coerce the keeper towards boxes
; S is the game state
(defun h305413659 (S)
	(let* (
		; Get coordinates of all boxes and goals
		(coords (getPositions S 0 NIL))
		; Get coordinates of all boxes
		(boxes (getBoxPositions coords NIL))
		; Get coordinates of all goals
		(goals (getGoalPositions coords NIL))
		; Get coordinates of keeper
		(keeperPos (getKeeperPosition S 0))
		(keeper (list (second keeperPos) (first keeperPos)))
	)
		; Match boxes to goals in order, add to keeper-box dist
		(+ (getPerfectMatching boxes goals 0) (getMinDist keeper boxes 0 T))
	)
)

; Tests

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

; Utility functions for printing states and moves.
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  )
    )
)

(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	)
)

(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	)
  )

(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  )

; Print a state
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    )
  )

; Print a list of states with delay.
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    )
  )