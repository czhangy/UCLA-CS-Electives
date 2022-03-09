# COM SCI 161 - Winter '22 - Gu

[TOC]

## Lecture 1: What is Artificial Intelligence?

- What is AI?
  - There are hundreds of definitions of AI
  - Thinking vs. Acting and Humanly vs. Rationally => 4 combinations
    - Thinking humanly: cognitive modeling, systems should solve problems the same way humans do
    - Thinking rationally: the use of logic, need to worry about modeling uncertainty and dealing with complexity
    - Acting humanly: the Turing Test approach
    - Acting rationally: the study of rational agents: agents that maximize the expected value of their performance measure given what they currently know
- Current Consensus Definition
  - AI is the study of intelligent, rational agents
    - Perception/sensing
    - Thinking/reasoning/inference
    - Acting
  - Environment => Percepts => Sensors => ? => Actuators => Action
  - For each possible percept sequence, a rational agent should select an action that is **expected** to maximize its **performance measure**, given the evidence provided by the percept sequence and whatever built-in knowledge the agent has
- Rational Agents
  - "Expected": not perfect
  - No mention of humanly
  - Which performance measure?
    - $1.01 now or "1 in a million" chance of $1 million
- The AI Pipeline
  - World => (Observations/Data) => User/Agent => (Knowledge representation/Machine learning) => Model => Inference (Reasoning) => Action => World
- Strong vs. Weak AI
  - Weak AI: as if intelligent
  - Strong AI: actually intelligent
    - Artificial General Intelligence (AGI): hypothetical intelligence of a machine that has the capacity to understand or learn any intellectual task that a human being can
  - Most of current AI research is on Weak AI
- Brief History
  - 1940s: interest in neurons, neural networks, and their relationship to mathematics and learning
  - 1950s: Turing's paper
  - 1956: Dartmouth conference
  - 1950s and 1960s: enthusiasm and optimism; big promises
  - Late 1960s and 1970s: realization that further progress was really hard; disillusionment
  - 1980s: Expert Systems, neural networks, etc.; AI now a little different; quiet successes
  - 1990s to present: intelligent agents
  - 2000s: robot pets, self-driving cars
- Conclusions
  - This course: a reality check!
    - Knowledge representation formalisms
    - Automated reasoning and search algorithms
    - Bayesian networks
  - "We can see only a short distance ahead, but we can see that much remains to be done"



## Lecture 2: Introduction to Lisp

- What is Lisp?
  - Originally specified in 1958 by John McCarthy, Lisp is the second oldest high-level programming language
  - Lisp has changed since its early days, and many dialects have existed over its history
    - One of the best-known general-purpose Lisp dialects is Common Lisp
  - Why use it in this class?
    - Lisp has been the primary AI language since the 80s
    - Lisp is popular for traditional AI programming because it supports symbolic computation very well
  - Common Lisp
    - The modern, multi-paradigm, high-performance, compiled, ANSI-standardized, most prominent descendant of the long-running family of Lisp programming languages
    - OOP and fast prototyping capabilities

- CLISP on SEASnet

  - ```bash
    ssh -X lnxsrv.seas.ucla.edu -l charlesx
    clisp
    ```

- Syntax

  - Two fundamental pieces: `ATOM` and `S-EXPRESSION`

  - `;` is the beginning of comments

  - Atom

    - ```lisp
      30 ; => 30
      "Hello!" ; => string
      t ; => true, any non-nil value is true
      nil ; => false, the empty list: ()
      :A ; => symbol
      A ; => error, not defined
      99999 ; => integer
      #b1111 ; => binary
      #x1111 ; => hexadecimal
      3.14159s0 ; => single precision float
      3.14159d0 ; => double precision float
      1/2 ; => ratios
      #C(1 2) ; => complex number
      ```

  - S-Expression

    - Super simple, super elegant

    - `(f x y z ...)`

      - Operator first, arguments follow!

    - ```lisp
      (+ 1 2 3 4) ; 1 + 2 + 3 + 4 => 10
      ```

    - Use `quote` or `'` to prevent it from being evaluated

      - ```lisp
        '(+ 1 2) ; => (+ 1 2)
        (quote (+ 1 2)) ; => (+ 1 2)
        '(1 2 3) ; => list (1 2 3)
        ```

  - Basic Arithmetic Operations

    - ```lisp
      (+ 1 1) ; => 2
      (- 8 1) ; => 7
      (* 10 2) ; => 20
      (expt 2 3) ; => 8
      (mod 5 2) ; => 1
      (/ 35 5) ; => 7
      (/ 1 3) ; => 1/3
      (+ #C(1 2) #C(6 -4)) ; => #C(7 -2)
      ```

  - Booleans and Equality

    - ```lisp
      (not nil) ; => T
      (and 0 t) ; => T
      (or 0 nil) ; => 0
      (and 1 ()) ; => NIL
      ```

      - `and` evaluates each argument from left to right => if any argument is `nil` or `()`, then it returns `nil`
        - Otherwise returns the last argument
      - `or` evaluates each argument from left to right => if any argument is not `nil` or `()`, then it immediately returns it
        - Otherwise returns `NIL`

    - ```lisp
      (= 3 3.0) ; => T
      (= 2 1) ; => NIL
      
      (eql 3 3) ; => T
      (eql 3 3.0) ; => NIL
      (eql (list 3) (list 3)) ; => NIL
      (eql 'a 'a) ; => T
      
      (equal (list 'a 'b) (list 'a 'b)) ; => T
      (equal (list 'a 'b) (list 'b 'a)) ; => NIL
      ```

      - `=` is for comparing numbers
      - `eql` is for comparing object identity
        - Similar to pointer comparison in C/C++
        - Comparing lists will return `NIL`
        - Mismatched types return `NIL`
      - `equal` is for comparing lists/strings
        - Order matters

  - Strings

    - ```lisp
      (concatenate 'string "Hello," "world!") ; => "Hello,world"
      
      (format nil "Hello, ~a" "Alice") ; => "Hello, Alice"
      (format t "Hello, ~a" "Alice") ; => NIL, formatted string goes to stdout
      
      (print "hello") ; => value is returned and printed to stdout
      (+ 1 (print 2)) ; prints 2, returns 3
      ```

      - `~a` represents strings, `~d` represents integers, `~f`/`~2f` represents single/double precision floats

  - Variables

    - The variable name can use any character except: ``()",'`;#|\``

    - Global (dynamically scoped) variables

      - ```lisp
        (defparameter age 35)
        age ; => 35
        
        (defparameter *city* "LA")
        *city* ; => "LA"
        ```

      - ```lisp
        (defparameter age 35) ; age => 35
        (defparameter age 60) ; age => 60
        
        (defvar newage 20) ; newage => 20
        (defvar newage 60) ; newage => 20, defvar doesn't change the value
        
        (setq newage 30) ; newage => 30
        ```

    - Local Variables

      - ```lisp
        (let ( (a 1) (b 2) ) ; binding
             (+ a b) ; body
        )
        ```

      - You will not be allowed to set global variables in your homework, use `let` only

      - A lot of parentheses => these define lists and programs

  - Lists

    - Linked list data structures

    - Made of `cons` pairs

    - ```lisp
      (cons 1 2) ; => '(1 2)
      (cons 3 nil) ; => '(3)
      (cons 1 (cons 2 (cons 3 nil))) ; => `(1 2 3)
      (list 1 2 3) ; => '(1 2 3)
      (cons 4 '(1 2 3)) ; => '(4 1 2 3)
      (cons '(4 5) '(1 2 3)) ; => ?
      
      (append '(1 2) '(3 4)) ; => '(1 2 3 4)
      (append 1 '(1 2)) ; => ERROR
      (concatenate 'list '(1 2) '(3 4)) ; => '(1 2 3 4)
      
      (car '(1 2 3 4)) ; => 1
      (cdr '(1 2 3 4)) ; => '(2 3 4)
      ```

      - `append` must be used with `list` arguments
      - `car` and `cdr` should be used with lists only
        - `car` => head
        - `cdr` => tail

  - Functions

    - Define a function:

      - ```lisp
        (defun hello (name) (format nil "Hello, ~A" name))
        ```

    - Call the function:

      - ```lisp
        (hello "Bob") ; => "Hello, Bob"
        ```

  - Control Flow

    - ```lisp
      (if (equal *name* "bob") ; test expression
          "ok" ; then expression
          "no") ; else expression
      ```

    - Chains of tests: `cond`

      - ```lisp
        (cond ((> *age* 20) "Older than 20")
          ((< *age* 20) "Younger than 20")
          (t "Exactly 20"))
        
        (cond ((> *age* 20) "Older than 20")
          ((< *age* 20) "Youger than 20")) ; returns NIL when *age* = 20
        ```

      - First condition that returns `T` is used

- Practice

  - Factorial

    - ```lisp
      (defun factorial (n)
        (if (< n 2)
            1 ; returns 1 when n < 2
            (* n (factorial (- n 1))) ; when n >= 2
        )
      )
      
      (factorial 5) ; => 120
      ```

  - Compute list length

    - ```lisp
      (defun listlength (x)
        (if (not x) ; base case: empty list
            0
            (+ (listlength (cdr x)) 1)
        )
      )
      ```

  - Compute list length (deep)

    - ```lisp
      (defun deeplength (x)
        (cond ((not x) 0) ; empty list => returns 0
          ((atom x) 1) ; atom => returns 1
          (t (+ (deeplength (car x)) ; else
                (deeplength (cdr x))
             )
         	)
        )
      )
      ```



## Lecture 3: Problem Solving

- More Practice

  - Recursion - check if list contains an element

    - ```lisp
      (defun contains (e x)
        (cond ((not x) nil)
          ((atom x) (equal e x))
          (t (or (contains e (car x)) (contains e (cdr x))))
        )
      )
      ```

  - Recursion - check if list contains a number

    - ```lisp
      (defun contains_number (x)
        (if (atom x) ; NIL if x is a list
            (numberp x) ; numberp: check if x is a number
            (or (contains_number (car x))
                (contains_number (cdr x)) ; recursively flatten
            )
        )
      )
      ```

  - Recursion - find `k`th element (top-level)

    - ```lisp
      (defun find_kth (k x)
        (if (= k 1)
            (car x)
            (find_kth (- k 1) (cdr x))
        )
      )
      ```

  - Recursion - delete `k`th element

    - ```lisp
      (defun delete_kth (k x)
        (if (= k 1)
            (cdr x)
            (cons (car x)
                  (delete k_th (- k 1) (cdr x))
            )
        )
      )
      ```

- Recursion

  - ```lisp
    (defun x () (x))
    ```

    - This runs forever

- Iteration

  - ```lisp
    (loop for x in '(1 2 3 4 5)
          do (print x)
    )
    ```

  - ```lisp
    (loop for x in '(1 2 3 4 5)
          for y in '(1 2 3 4 5)
          collect (+ x y) ; => (2 4 6 8 10)
    )
    
    (loop for x in '(1 2 3 4 5)
          for y in '(1 2 3 4)
          collect (+ x y) ; => (2 4 6 8)
    )
    ```

  - How do we calculate a factorial iteratively?

    - ```lisp
      (setf fact 1)
      (defun factorial (n)
        (loop for x from 2 to n
              do (setf fact (* x fact))
        )
        (print fact)
      )
      ```

- Problem Solving

  - Problem Solving as a Search Problem
    - Many AI problems can be formulated as search
    - For example: "Farmer Crosses River Puzzle", "Vacuum World"
  - Problem-Solving Agents - Example: Romania
    - On holiday in Romania: currently in Arad, flight leaves tomorrow from Bucharest
    - Formulate goal: be in Bucharest
    - Formulate problem:
      - States: various cities
      - Actions: drive between cities
    - Find solution: sequence of cities
  - Single-State Problem Formulation
    - A problem is defined by the following items:
      - States (e.g., city names)
      - Initial state (e.g., "at Arad")
      - Actions (e.g., `<Arad => Zerind>`)
      - Successor function `S(x)` = set of action-state pairs
        - e.g., `S(Arad) = {<Arad => Zerind, Zerind>, ...}`
      - Goal test, which can be:
        - Explicit (e.g., "at Bucharest")
        - Implicit (e.g., `NoDirt(x)`)
      - Path cost (additive)
        - e.g., sum of distances, number of actions executed, etc.
        - `c(x, a, y)` is the step/action cost, assumed to be `>= 0`
    - A solution is a sequence of actions leading from the initial state to the goal state
  - Selecting a State Space
    - Real world is absurdly complex
      - State space must be abstracted for problem solving
    - (Abstract) state = set of real states
    - (Abstract) action = complex combination of real actions
      - e.g., `<Arad => Zerind>` represents a complex set of possible routes, detours, rest stops, etc.
    - For guaranteed realizability any real state "in Arad" must get to some real state "in Zerind"
    - (Abstract) solution = set of real paths that are solutions in the real world
    - Any abstract action should be easier than the original problem 



## Lecture 4: Uninformed Search

- Tree Search Algorithms

  - Basic idea:

    - Offline, simulated exploration of state space by generating successors of already-explored states (aka expanding states)

  - ```pseudocode
    function TREE_SEARCH(problem, strategy) returns a solution, or failure
    	initialize the search tree using the initial state of problem
    	loop do
    		if there are no candidates for expansion then return failure
    		choose a leaf node for expansion according to strategy
    		if the node contains a goal state then return the corresponding solution
    		else expand the node and add the resulting nodes to the search tree
      end
    ```

- Implementation: States vs. Nodes

  - A state is a (representation of) a physical configuration
  - A node is a data structure constituting part of a search tree
    - Includes parent, children, depth, and path cost `g(x)`
    - States don't have parents, children, depth, or path cost
  - The `EXPAND` function creates new nodes, filling in the various fields and using the `SUCCESSOR_FN` of the problem to create the corresponding states

- Implementation: General Tree Search

  - ```pseudocode
    function TREE_SEARCH(problem, fringe) returns a solution, or failure
    	fringe <- INSERT(MAKE_NODE(INITIAL_STATE[problem]), fringe)
    	loop do
    		if fringe is empty then return failure
    		node <- REMOVE_FRONT(fringe)
    		if GOAL_TEST(problem, STATE(node)) then return node
    		fringe <- INSERTALL(EXPAND(node, problem), fringe)
    		
    function EXPAND(node, problem) returns a set of nodes
    	successors <- the empty set
    	for each action, result in SUCCESSOR_FN(problem, STATE[node]) do
    		s <- a new NODE
    		PARENT_NODE[s] <- node; ACTION[s] <- action; STATE[s] <- result
    		PATH_COST[s] <- PATH_COST[node] + STEP_COST(STATE[node], action, result)
    		DEPTH[s] <- DEPTH[node] + 1
    		add s to successors
    	return successors
    ```

- Search Strategies

  - A strategy is defined by picking the order of node expansion
  - Strategies are evaluated along the following dimensions:
    - Completeness - does it always find a solution if one exists?
    - Time complexity - number of nodes generated/expanded
    - Space complexity - maximum number of nodes in memory
    - Optimality - does it always find a least-cost solution?
  - Time and space complexity are measured in terms of:
    - `b` - maximum branching factor of the search tree
    - `d` - depth of the least-cost solution
    - `m` - maximum depth of the state space (may be infinite)
      - `d <= m`

- Uninformed Search Strategies

  - Uninformed strategies use only the information available in the problem definition

  - Breadth-First Search

    - Expand shallowest unexpanded node

    - Implementation:

      - ```pseudocode
        function BREADTH_FIRST_SEARCH(problem) returns a solution or failure
        	node <- NODE(problem.INITIAL)
        	if problem.IS_GOAL(node.STATE) then return node
        	frontier <- a FIFO queue, with node as an element
        	reached <- { problem.INITIAL }
        	while not IS_EMPTY(frontier) do
        		node <- POP(frontier)
        		for each child in EXPAND(problem, node) do
        			s <- child.STATE
        			if problem.IS_GOAL(s) then return child
        			if s is not in reached then
        				add s to reached
        				add child to frontier
          return failure
        ```

        - `fringe` is a FIFO queue, i.e., new successors go to the end

    - Properties:

      - Complete if `b` is finite
      - `O(b^d)` time
      - `O(b^d)` space
      - Optimal if the cost is `1` per step; not optimal in general
      - Space is the largest problem

  - Depth-First Search

    - Expand deepest unexpanded node
    - Implementation:
      - `fringe` is a LIFO queue, i.e., put successors at front
    - Properties:
      - Not complete, fails in infinite-depth spaces and spaces with loops
        - Modify to avoid repeated states along path => complete in finite space
      - `O(b^m)` time
        - Terrible if `m` is much greater than `d`
        - If solutions are dense, may be much faster than BFS
      - `O(bm)` space
      - Not optimal

  - Depth-Limited Search

    - DFS with depth limit `l`

      - i.e., nodes at depth `l` have no successors

    - Implementation:

      - ```pseudocode
        function ITERATIVE_DEEPENING_SEARCH(problem) returns solution or failure
        	for depth = 0 to INF do
        		result <- DEPTH_LIMITED_SEARCH(problem, depth)
        		if result != cutoff then return result
        ```

    - Properties:

      - Complete
      - `O(b^d)` time
      - `O(bd)` space
      - Optimal if step cost is `1`

  - Repeated States

    - Failure to detect repeated states can turn a linear problem into an exponential one

  - Uniform Cost Search

    - When all step costs are equal, BFS is optimal, what to do otherwise?

    - ```pseudocode
      function UNIFORM_COST_SEARCH(problem) returns a solution, or failure
      	node <- a node with STATE = problem.INITIAL_STATE, PATH_COST = 0
      	frontier <- a priority queue ordered by PATH_COST, with node
      	explored <- an empty set
      	loop do
      		if EMPTY?(frontier) then return failure
      		node <- POP(frontier)
      		if problem.GOAL_TEST(node.STATE) then return SOLUTION(node)
      		add node.STATE to explored
      		for each action in problem.ACTIONS(node.STATE) do
      			child <- CHILD_NODE(problem, node, action)
      			if child.STATE is not in explored or frontier then
      				frontier <- INSERT(child, frontier)
      			else if child.STATE is in frontier with higher PATH_COST then
      				replace that frontier node with child
      ```



## Lecture 5: Informed Search

- Uninformed Search

  - Uniform-Cost Search

    - Expand least-cost unexpanded node

    - Let `g(n)` be the sum of the cost (path cost) from start to node `n`

    - Implementation:

      - `fringe` = queue ordered by path cost, lowest first
      - Equivalent to BFS if step costs all equal

    - Properties:

      - Complete if step cost greater than some finite constant

      - Complexity, where `C*` is the cost of the optimal solution

        - $$
          O(b^{\lceil C^*/\epsilon\rceil})
          $$

        - Defined by the number of nodes with `g(n)` less than the cost of the optimal solution

      - Optimal, as long as nodes are expanded in increasing order of `g(n)`

  - Summary

    - Problem formulation usually requires abstracting away real-world details to define a state space that can feasibly be explored
    - Variety of uninformed search strategies
    - Iterative deepening search uses only linear space and not much more time than other uninformed algorithms
    - Graph search can be exponentially more efficient than tree search

- Informed Search

  - Outline

    - Best-first search
    - A* search
    - Heuristics

  - Best-First Search

    - Idea: use an evaluation function for each node
      - Estimate of "desirability"
      - Expand the most desirable unexpanded node
    - Implementation:
      - `fringe` is a queue sorted in decreasing order of desirability
      - Special cases:
        - Greedy search
        - A* search

  - Greedy Search

    - Evaluation function `h(n)` (heuristic)
      - Estimate of cost from `n` to the closest goal
      - e.g., `hSLD(n)` is the straight-line distance from `n` to Bucharest
    - Greedy search expands the node that appears to be closest to the a goal
    - Properties:
      - Not complete, can get stuck in infinite loop
        - Complete in finite space with repeated-space checking
      - `O(b^m)` time
        - A good heuristic can give dramatic improvements
      - `O(b^m)` space
      - Not optimal

  - A* Search

    - Idea: avoid expanding paths that are already expensive

    - Evaluation function: `f(n) = g(n) + h(n)`

      - `g(n)` = cost so far to reach node `n`
      - `h(n)` = estimated cost to goal from `n`
      - `f(n)`= estimated total cost of path through `n` to goal

    - A* search uses an admissible heuristic

      - i.e., `h(n) <= h*(n)`, where `h*(n)` is the true cost from `n`
        - e.g., `hSLD(n)` never overestimates the actual road distance
      - Requires that `h(n) >= 0` so that `h(G) = 0` for any goal `G`

    - Properties:

      - Theorem: A* search is optimal

        - Suppose some suboptimal goal `G2` has been generated and is in the queue

          - Let `n` be an unexpanded node on a shortest path to an optimal goal `G`

          - $$
            f(G_2)=g(G_2)>g(G)=g(n)+h^*(n)\ge g(n)+h(n)=f(n)
            $$

            - Since `f(G2) > f(n)`, A* will never select `G2` for expansion

        - Lemma: A* expands nodes in order of increasing `f` value

          - Gradually adds "`f`-contours" of nodes
            - Contour `i` has all nodes with `f <= fi`, where `fi < fi+1`

      - Complete unless there are infinitely many nodes with `f <= f(G)`

      - `O(b^Δ)` time, `Δ = h* - h`

        - `h*`: actual cost from the root to the foal
        - `h`: estimated cost

      - `O(b^Δ)` space, keeps all nodes in memory

      - Optimal

        - A* expands all nodes with `f(n) < C*`
        - A* expands some nodes with `f(n) = C*`
        - A* expands no nodes with `f(n) > C*`

  - Proof of Lemma: Consistency

    - A heuristic is consistent if:

      - $$
        h(n)\le c(n,a,n')+h(n')
        $$

        - `c(n, a, n')` is the cost of path from `n` to `n'` by choosing action `a`

    - If `h` is consistent, we have:

      - $$
        f(n')=g(n')+h(n')=g(n)+c(n,a,n')+h(n')\ge g(n)+h(n)=f(n)
        $$

        - i.e., `f(n)` is nondecreasing along any path

    - The goal state with the lowest `f`-cost will be found first

    - Every consistent heuristic is admissible

  - Summary

    - Heuristic functions estimate costs of shortest paths
    - Good heuristics can dramatically reduce search cost
    - Greedy best-first search expands lowest `h`
      - Incomplete and not always optimal
    - A* search expands lowest `g + h`
      - Complete and optimal
      - Also optimally efficient (up to tiebreaks, for forward search)
    - Admissible heuristics can be derived from exact solution of relaxed problems



## Lecture 6: Local Search Algorithms

- Motivation: attempt to achieve constant space complexity

- Outline:

  - Hill-Climbing
  - Simulated Annealing
  - Local Beam Search
  - Genetic Algorithms
  - Local Search in Continuous Spaces

- Iterative Improvement Algorithms

  - In many optimization problems, the path is irrelevant, the goal state itself is the solution
    - Ex) In the 8-queens problem, only the final configuration of the queens matters
  - In such cases, we can use iterative improvement algorithms
    - Keep a current state and try to improve it
    - Constant space, suitable for both offline and online searches
    - No guarantee of optimality
  - Example: Traveling Salesperson Problem
    - Goal: to find the shortest path that visits each city and returns to the origin city
    - Start with any complete tour (may have cross-paths, not optimal)
    - Perform pairwise exchanges, each iteration reduces the length of the path
      - Variants of this approach get within 1% of the optimal solution very quickly with thousands of cities
  - Example: `n`-Queens
    - Goal: put `n` queens on an `n x n` board with no two queens on the same row, column, or diagonal
    - Move a queen to reduce number of conflicts
      - Almost always solves `n`-queens problems almost instantaneously for very large `n`

- State-Space Landscape

  - Goal: to find global maximum
  - Complete: finds a goal if one exists
  - Optimal: finds a global maximum/minimum

- Hill-Climbing (or gradient ascent/descent)

  - Moves in the direction of increasing value

  - ```pseudocode
    function HILL_CLIMBING(problem) returns a state that is a local maximum
    	inputs: problem, a problem
    	local variables: current, a node
    	                 neighbor, a node
    	
    	current <- MAKE_NODE(INITIAL_STATE[problem])
    	loop do
    		neighbor <- a highest-valued successor of current
    		if VALUE[neighbor] <= VALUE[current] then return STATE[current]
    		current <- neighbor
      end
    ```

  - Useful to consider the state-space landscape

    - Escape from shoulders: random sideways moves, maybe loop on flat maxima
    - Escape from local maxima: random-restart hill climbing, trivially complete

- Simulated Annealing

  - Idea: escape local maxima by allowing some "bad" moves, but gradually decrease their size and frequency

  - ```pseudocode
    function SIMULATED_ANNEALING(problem, schedule) returns a solution state
    	inputs: problem, a problem
    	        schedule, a mapping from time to "temperature"
    	local variables: current, a node
    	                 next, a node
    	                 T, a "temperature" controlling prob. of downward steps
    	
    	current <- MAKE_NODE(INITIAL_STATE[problem])
    	for t <- 1 to INF do
    		T <- schedule[t]
    		if T = 0 then return current
    		next <- a randomly selected successor of current
    		ΔE <- VALUE[next] - VALUE[current]
    		if ΔE > 0 then current <- next
    		else current <- next only with probability e^(ΔE/T)
    ```

  - Properties:

    - If the move improves the situation, it is always accepted

      - Otherwise, the algorithm accepts the move with some probability less than `1`

    - The probability decreases exponentially with the "badness" of the move (the amount `ΔE` by which the evaluation is worsened)

    - The probability also decreases as the "temperature" `T` goes down: "bad" moves are more likely to be allowed at the start when `T` is high, and they become more unlikely as `T` decreases

    - If the schedule lowers `T` slowly enough, the algorithm will find a global optimum with probability approaching `1`

    - At fixed temperature `T`, state occupation probability reaches Boltzmann distribution:

      - $$
        p(x)=\alpha e^{\frac{E(x)}{kT}}
        $$

      - `T` decreased slowly enough => always reaches best state `x*` 

        - For small `T`:

          - $$
            e^{\frac{E(x^*)}{kT}}/e^{\frac{E(x)}{kT}}=e^{\frac{E(x^*)-E(x)}{kT}}>> 1
            $$

        - Thus, very likely to choose `x*`

- Local Beam Search

  - Idea: keep `k` states instead of 1
    - At each step, all the successors of all `k` states jare generated
    - If any one is a goal, the algorithm halts
    - Otherwise, it selects the best `k` successors from the complete list and repeats
  - Not the same as `k` searches running in parallel!
    - Searches that find good states recruit other searches to join them
  - Problem: quite often, all `k` states end up on the same local hill
  - Stochastic beam search: choose `k` successors randomly, biased towards good ones
    - Observe the close analogy to natural selection

- Evolutionary Algorithms/Genetic Algorithms

  - Stochastic local beam search + generate successors from pairs of states
  - Fitness => Selection => Pairs => Crossover => Mutation
    - Fitness function: higher score, higher chance to be selected
    - Crossover: crossover point is chosen randomly
    - Mutation: small probability
  - GAs require states encoded as strings
  - Crossover helps iff substrings are meaningful components



## Lecture 7: Local Search and Constraint Satisfaction

- Local Search in Continuous State Spaces

  - Discretization methods turn continuous space into discrete space

    - e.g., empirical gradient considers `±δ` change in each coordinate

  - Gradient Methods

    - Compute:

      - $$
        \nabla f=(\frac{\partial f}{\partial x_1},\frac{\partial f}{\partial y_1},\frac{\partial f}{\partial x_2},\frac{\partial f}{\partial y_2},\frac{\partial f}{\partial x_3},\frac{\partial f}{\partial y_3})
        $$

      - To increase/decrease `f` by:

        - $$
          x\leftarrow x+\alpha\nabla f(x)
          $$

          - ` α` is the step size

  - Newton-Raphson Method

    - Sometimes can solve for `∇f(x) = 0` exactly

    - Newton-Raphson iterates:

      - $$
        x\leftarrow x-H_f^{-1}(x)\nabla f(x)
        $$

      - $$
        H_{ij}=\partial^2f/\partial x_i\partial x_j
        $$

    - Avoid explicit choice of the step size

- Constraint Satisfaction

  - Outline

    - CSP Examples
    - Constraint Propagation
    - Backtracking Search for CSPs
    - Problem Structure and Problem Decomposition

  - Constraint Satisfaction Problems (CSPs)

    - A constraint satisfaction problem (CSP) consists of three components: `X`, `D`, and `C`
      - `X`: a set of variables, `{X_1, ..., X_n}`
      - `D`: a set of domains, `{D_1, ..., D_n}`, one for each variable
      - `C`: a set of constraints, `<scope, rel> ∈ C`
        - `scope`: tuple of variables
        - `rel`: values that those variables can take on
    - State of CSP: an assignment of values to some or all of the variables
      - Complete assignment: every variable is assigned
      - Partial assignment: assigns values to only some of the variables
      - Consistent: not violate any constraints
    - Solution: a consistent, complete assignment

  - Example: Map-Coloring

    - Variables: `WA`, `NT`, `Q`, `NSW`, `V`, `SA`, `T`
    - Domains: `D_i = {red, green, blue}`
    - Constraints: adjacent regions must have different colors
    - Solutions are assignments satisfying all constraints
      - e.g. `{WA = red, NT = green, Q = red, NSW = green, V = red, SA = blue, T = green}`

  - Varieties of CSPs

    - Discrete Variables
      - Finite domains; size `d`  =>  `O(d^n)` complete assignments
        - e.g., Boolean CSPs, including Boolean satisfiability (NP-complete)
      - Infinite domains (integers, strings, etc.)
        - e.g., job scheduling, variables are start/end days for each job
        - Need a constraint language
          - e.g., `StartJob_1 + 5 <= StartJob_3`
    - Continuous Variables
      - e.g., start/end times for Hubble Telescope observations
      - Linear constraints solvable in polynomial time by linear programming

  - Varieties of Constraints

    - Unary constraints involve a single variable
      - e.g., `SA != green`
    - Binary constraints involve pairs of variables
      - e.g., `SA != WA`
    - Higher-order (Global) constraints involve 3+ variables
      - e.g., crypt-arithmetic column constraints, `alldiff`, etc.
    - Preferences (soft constraints)
      - e.g., `red` is better than `green`
      - Often representable by a cost for each variable assignment
      - Constrained optimization problems

  - Constraint Graph

    - Binary CSP: each constraint relates at most two variables
    - Constraint graph: nodes are variables, arcs show constraints
    - General-purpose CSP algorithms use the graph structure to speed up search

  - Constraint Propagation

    - Using the constraints to reduce the number of legal states for a variable

    - May be done as a preprocessing step before search starts

    - Node consistency: all the values in the variable's domain satisfy the variable's unary constraints

    - Arc consistency: every value in its domain satisfies the variable's binary constraints

      - For any variables `X_i`, `X_j`, `X_i` is arc-consistent with respect to `X_j` if for every value in `D_i`, there is some value in `D_j` that satisfies the binary constraint on the arc `(X_i, X_j)`

      - Algorithm

        - Each binary constraint becomes two arcs, one in each direction

        - AC-3: remove values from the domains of variables until no more arcs are in the queue

        - ```pseudocode
          function AC-3(csp) returns the CSP, possibly with reduced domains
          	inputs: csp, a binary CSP with variables {X_1, ..., X_n}
          	local variables: queue, a queue of arcs, initially all the arcs in csp
          	
          	while queue is not empty do
          		(X_i, X_j) <- REMOVE_FIRST(queue)
          		if REMOVE_INCONSISTENT_VALUES(X_i, X_j) then
          			for each X_k in NEIGHBORS[X_i] do
          				add (X_k, X_i) to queue
          	return true
          	
          function REMOVE_INCONSISTENT_VALUES(X_i, X_j) returns true iff succeeds
          	removed <- false
          	for each x in DOMAIN[X_i] do
          		if no value y in DOMAIN[X_j] allows (x, y) to satisfy the constraint X_i <-> X_j
          			then delete x from DOMAIN[X_i]; removed <- true
          	return removed
          ```

          - If AC-3 revises `D_i`, then we add `(X_k, X_i)`, where `X_k` is a neighbor of `X_i`, since the change in `D_i` might enable further reductions in the domains of `D_k`
          - Time complexity: `O(n^2d^3)`
            - Each arc `(X_k, X_i)` can be inserted in the queue only `d` times because `X_i` has at most `d` values to delete
            - At most `n^2` arcs
            - Checking consistency of an arc can be done in `d^2` time

      - Simplest form of propagation makes each arc consistent

        - `X -> Y` is consistent iff for every value `x` of `X` there is some allowed `y`
        - If `X` loses a value, neighbors of `X` need to be rechecked



## Lecture 8: Constraint Satisfaction

- Standard Search Formulation (incremental)

  - Let's start with the straightforward, dumb approach, then fix it
  - States are defined by the values assigned so far
  - Initial state: the empty assignment, `{}`
  - Successor function: assign a value to an unassigned variable that doesn't conflict with the current assignment
    - Fail if no legal assignments
  - Goal test: the current assignment is complete
  - Evaluation:
    - This is the same for CSPs
    - Every solution appears at depth `n` with `n` variables
      - Use DFS
    - Path is irrelevant, so we can also use complete-state formulation
    - `b = (n - l)d` at depth `l`, hence `n!d^n` leaves

- Backtracking Search

  - Variable assignments are commutative

    - i.e., `[WA = red then NT = green]` is the same as `[NT = green then WA = red]`
    - Only need to consider assignments to a single variable at each node
      - `b = d` and there are `d^n` leaves

  - DFS for CSPs with single assignments is called backgracking search

    - The most basic uninformed algorithm for CSPs

  - DFS that chooses values for one variable at a time and backtracks when a variable has no legal values left to assign

  - ```pseudocode
    function BACKTRACKING_SEARCH(csp) returns solution/failure
    	return RECURSIVE_BACKTRACKING({}, csp)
    	
    function RECURSIVE_BACKTRACKING(assignment, csp) returns solution/failure
    	if assignment is complete then return assignment
    	var <- SELECT_UNASSIGNED_VARIABLE(VARIABLES[csp], assignment, csp)
    	for each value in ORDER_DOMAIN_VALUES(var, assignment, csp) do
    		if value is consistent with assignment given CONSTRAINTS[csp] then
    			add {var = value} to assignment
    			result <- RECURSIVE_BACKTRACKING(assignment, csp)
    			if result != failure then return result
    			remove {var = value} from assignment
      return failure
    ```

  - Improving Backtracking Efficiency

    - General-purpose methods can give huge gains in speed:

      - Which variable should be assigned next?
        - Minimum Remaining Values (MRV): choose the variable with the fewest legal values
        - Degree: choose the variable with the most constraints on remaining variables
          - Tie-breaker among MRV variables

      - In what order should its values be tried?
        - Least Constraining Value: choose the least constraining variable, the one that rules out the fewest values in the remaining variables
          - Leave the maximum flexibility for subsequent variable assignments
      - Can we detect inevitable failure early?
        - Forward Checking
          - How do we detect failure early (if there is no solution)?
          - If we do `AC-3` before the search, we already know if there is a failure
            - What if we don't?
          - Idea: keep track of remaining legal values for unassigned variables
            - Terminate search when any variable has no legal values
      - Can we take advantage of problem structure?



## Lecture 9: CSPs and Game Playing

- CSPs

  - Problem Structure

    - Independent subproblems are identifiable as multiple connected components of a constraint graph

      - Suppose each subproblem has `c` variables out of `n` total, then the worst-case solution cost is:

        - $$
          O(\frac{n}{c}\times d^c)
          $$

        - Linear in `n`

    - Tree-Structured CSPs

      - Theorem: if the constraint graph has no loops, the CSP can be solved in `O(nd^2)` time
      - Compare to general CSPs, where worst-case time is `O(d^n)`
      - This property also applies to logical and probabilistic reasoning: an important example of the relation between syntactic restrictions and the complexity of reasoning
      - Algorithm:
        - Choose a variable as root, order variables from root to leaves such that every node's parent precedes it in the ordering
        - For `j` from `n` down to `2`, apply `REMOVEINCONSISTENT(Parent(X_j), X_j)`
        - For `j` from `1` to `n`, assign `X_j` consistently with `Parent(X_j)`

    - Nearly Tree-Structured CSPs

      - Conditioning: instantiate a variable, prune its neighbors' domains

      - Cutset conditioning: instantiate (in all ways) a set of variables such that the remaining constraint graph is a tree

        - Cutset size `c` results in a runtime of:

          - $$
            O(d^c\times(n-c)d^2)
            $$

        - Search the resultant graphs for a solution

        - Produce a cutset such that the remaining nodes form a tree structure

          - Cut out nodes with higher degrees

  - Iterative Algorithms for CSPs

    - Hill-climbing, simulated annealing typically work with "complete" states
      - i.e., all variables assigned
    - To apply to CSPs:
      - Allow states with unsatisfied constraints
      - operators reassign variable names
    - Variable selection: randomly select any conflicted value
    - Value selection by min-conflicts heuristic:
      - Choose value that violates the fewest constraints
      - i.e., hill climb with `h(n)` being the total number of violated constraints

  - Summary

    - CSPs are a special kind of problem:
      - States defined by values of a fixed set of variables
      - Goal test defined by constraints on variable values
    - Backtracking is DFS with one variable assigned per node
    - Variable ordering and value selection heuristics help significantly
    - Forward checking prevents assignments that guarantee later failure
    - Constraint propagation (e.g., arc consistency) does additional work to constraint values and detect inconsistencies
    - The CSP representation allows analysis of problem structure
      - Tree-structured CSPs can be solved in linear time
    - Iterative min-conflicts is usually effective in practice

- Game Playing

  - Outline

    - Games
    - Perfect Play
      - Minimax decisions
      - Alpha-beta pruning
    - Resource limits and approximate evaluation
    - Games of chance
    - Games of imperfect information

  - Types of Games

    - |                       | Deterministic                 | Chance                               |
      | --------------------- | ----------------------------- | ------------------------------------ |
      | Perfect Information   | Chess, checkers, Go, Othello  | Backgammon, Monopoly                 |
      | Imperfect Information | Battleship, blind tic-tac-toe | Bridge, poker, Scrabble, nuclear war |

  - Games vs. Search Problems

    - Can we use search strategies to win games?
      - What would be the solution when applying search strategies to games?
      - The solution will be a strategy that specifies a move for every possible opponent reply
    - Challenges
      - Very large search space
      - Time limits

  - Game as a Search Problem

    - `S_0`: the initial state, which specifies how the game is set up at the start
    - `PLAYER(s)`: defines which player has the move in a state
    - `ACTIONS(s)`: returns the set of legal moves in a state
    - `RESULT(s, a)`: the transition model, which defines the result of a move
    - `TERMINAL-TEST(s)`: a terminal test, which is true when the game is over and false otherwise
      - States where the game has ended are called terminal states
    - `UTILITY(s, p)`: a utility function that defines the final numeric value for a game that ends in terminal state `s` for a player `p`
      - Also called an objective function or payoff function
      - In chess, the outcome is a win, loss, or draw, with values `+1`, `0`, or `1/2`



## Lecture 10: Game Playing I

- Optimal Decisions in Games

  - How to find the optimal decisions in a deterministic, perfect-information game?
  - Idea: choose the move with highest achievable payoff against the best play of the other player
  - Important assumption: we assume that both players play optimally from beginning to end of the game

- Minimax

  - We refer to the two players as `MAX` and `MIN`

    - Without loss of generality, we imagine that we are `MAX` playing against `MIN`

  - We refer to the payoff as the minimax value

    - The player `MAX` will always choose the move with the maximum minimax value
    - The player `MIN` will always choose the move with the minimum minimax value

  - $$
    \texttt{MINIMAX}(s)=\begin{cases}
    	\texttt{UTILITY}(s) & \text{if}\texttt{ TERMINAL-TEST}(s)\\
    	\text{max}_{a\in Actions(s)}\texttt{MINIMAX}(\texttt{RESULT}(s,a)) & \text{if}\texttt{ PLAYER}(s)=\texttt{MAX}\\
    	\text{min}_{a\in Actions(s)}\texttt{MINIMAX}(\texttt{RESULT}(s,a)) & \text{if}\texttt{ PLAYER}(s)=\texttt{MIN}
    \end{cases}
    $$

    - Bottom-up recursion

  - ```pseudocode
    function MINIMAX-DECISION(state) returns an action
    	inputs: state, current state in game
    	
    	return the a in ACTIONS(state) maximizing MIN-VALUE(RESULT(a, state))
    
    function MAX-VALUE(state) returns a utility value
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- -INF
    	for a, s in SUCCESSORS(state) do v <- MAX(v, MIN-VALUE(s))
    	return v
    	
    function MIN-VALUE(state) returns a utility value
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- INF
    	for a, s in SUCCESSORS(state) do v <- MIN(v, MAX-VALUE(s))
    	return v
    ```

  - Properties:

    - Complete only if the tree is finite
    - Optimal against an optimal opponent
    - `O(b^m)` time
      - `b` is the branching factor, `m` is the maximum depth of the game tree

    - `O(bm)` space complexity
      - Essentially same as DFS

  - But do we need to explore every path and compute `MINIMAX` for every node?

- Alpha-Beta Pruning

  - Motivation: do we really need to calculate the minimax value for every node?

  - `α`: the best value (to `MAX`) found so far off the current path

    - If `v` is worse than `α`, `MAX` will avoid it => prune that branch

  - `β`: similarly defined for `MIN`

  - ```pseudocode
    function ALPHA-BETA-DECISION(state) returns an action
    	return the a in ACTIONS(state) maximizing MIN-VALUE(RESULT(a, state), -INF, INF)
    	
    function MAX-VALUE(state, α, β) returns a utility value
    	inputs: state, current state in game
    	α, the value of the best alternative for MAX along the path to state
    	β, the value of the best alternative for MIN along the path to state
    	
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- -INF
    	for a, s in SUCCESSORS(state) do
    		v <- MAX(v, MIN-VALUE(s, α, β))
    		if v >= β then return v
    		α <- MAX(α, v)
    	return v
    
    function MIN-VALUE(state, α, β) returns a utility value
    	inputs: state, α, β
    	
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- +INF
    	for a, s in SUCCESSORS(state) do
    		v <- MIN(v, MAX-VALUE(s, α, β))
    		if v <= α then return v
    		β <- MIN(β, v)
    	return v
    ```




## Lecture 11: Game Playing II

- Alpha-Beta Pruning

  - Properties

    - Pruning doesn't affect the final result
    - Good move ordering improves effectiveness of pruning
      - With "perfect ordering", time complexity = `O(b^m/2)`

  - Improvements Under Resource Limits

    - Standard approach:

      - Based on depth-limited search, stopping in the middle of the game tree

      - Use `CUTOFF-TEST` instead of `TERMINAL-TEST`
        - e.g., depth limit

      - Use `EVAL` instead of `UTILITY`
        - i.e., an evaluation function that estimates desirability of position

    - Evaluation Functions

      - For chess, we typically use a linear weighted sum of features

        - $$
          Eval(s)=w_1f_1(s)+w_2f_2(s)+...+w_nf_n(s)
          $$

        - An example of a feature:

          - $$
            f_1(s)=\text{number of white queens}-\text{number of black queens}
            $$

      - Digression: exact values don't matter

        - Behavior is preserved under any monotonic transformation of the original `EVAL`
        - Monotonic => the relative ordering of values is preserved

- Deterministic Games in Practice

  - Checkers: Chinook ended 40-year reign of human world champion Marion Tinsley in 1994
    - Used an endgame database defining perfect play for all positions involving 8 or fewer pieces on the board, a total of 443,748,401,247 positions

  - Chess: Deep Blue defeated human world champion Gary Kasparov in a six-game match in 1997
    - Deep Blue searches 200 million positions per second, uses very sophisticated evaluation, and undisclosed methods for extending some lines of search up to 40 ply

  - Go: Alpha Go, which is based on deep reinforcement learning and Monte Carlo tree search

- Nondeterministic (Stochastic) Games in General

  - In nondeterministic games, chances are introduced
    - Ex) dice rolls, card shuffling, coin flips, etc.

  - Introduce chance nodes to model the probabilities of various results
    - Branches into other nodes, with values weighted by the probability of that path's occurrence

- Algorithm for Nondeterministic Games

  - `EXPECTIMINIMAX` gives perfect play

    - Just like `MINIMAX`, except we must also handle chance nodes

  - ```pseudocode
    if state is a MAX node then
    	return the highest EXPECTIMINIMAX-VALUE of SUCCESSORS(state)
    if state is a MIN node then
    	return the lowest EXPECTIMINIMAX-VALUE of SUCCESSORS(state)
    if state is a chance node then
    	return average of EXPECTIMINIMAX-VALUE of SUCCESSORS(state)
    ```

  - Digression: exact values do matter

    - Behavior is preserved only by positive linear transformation of `EVAL`, therefore `EVAL` should be proportional to the expected payoff

- Games of Imperfect Information

  - Example:
    - In card games, opponent's initial cards are unknown
    - Typically, we can calculate a probability for each possible deal
      - Consider as a big dice roll at the beginning of the game

    - Compute the minimax value of each action in each deal
    - Then choose the action with highest expected value over all deals
    - Special case: if an action is optimal for all deals, it's optimal
    - The current best bridge program approximates this idea by:
      - Generating 100 deals consistent with bidding information
      - Picking the action that wins most tricks on average




## Lecture 12: Game Playing and Logic

- Games of Imperfect Information

  - Suppose that each deal `s` occurs with probability `P(s)`, the move we want is:

    - $$
      \text{argmax}_a\sum_sP(s)\text{MINIMAX}(\text{RESULT}(s,a))
      $$

  - In practice, the number of possible deals is rather large

  - We resort to Monte Carlo approximation:

    - We take a random sample of `N` deals instead of adding up all the deals

    - The probability of deal `s` appearing in the sample is proportional to `P(s)`

    - $$
      \text{argmax}_a\frac{1}{N}\sum^N_{i=1}\text{MINIMAX}(\text{RESULT}(s_i,a))
      $$

  - Example: Kriegspiel

    - A partially observable variant of chess in which pieces can move but are completely invisible to the opponent
    - Rules:
      - White and Black each see a board containing only their own pieces
      - A referee, who can see all the pieces
      - White proposes to the referee any move that would be legal if there were no black pieces
        - If the move is in fact illegal (because of the black pieces), the referee announces "illegal"

    - Can White win the game?
    - Guaranteed checkmate: for each possible percept sequence, leads to an actual checkmate for every possible board state, regardless of how the opponent moves

  - Proper Analysis

    - Intuition that the value of an action is the average of its values in all actual states is wrong
    - With partial observability, value of an action depends on the information state or belief state the agent is in
    - Can generate and search a tree of information states
    - Leads to rational behaviors such as:
      - Acting to obtain information
      - Signaling to one's partner
      - Acting randomly to minimize information disclosure

- Summary

  - Games are fun and dangerous to work on
  - They illustrate several important points about AI:
    - Perfection is unattainable => must approximate
      - Results in information loss, and possibly a suboptimal solution

    - Good idea to think about what to think about
    - Uncertainty constraints the assignment of values to stress
    - Optimal decisions depend on information state, not real state

  - Games are to AI as grand prix racing is to automobile design

- Logical Agents

  - Outline:

    - Knowledge-Based Agents
    - Wumpus World
    - Logic in General - Models and Entailment
    - Propositional (Boolean) Logic
    - Equivalence, Validity, Satisfiability
    - Inference Rules and Theorem Proving
      - Forward Chaining
      - Resolution

  - Knowledge Bases

    - A set of sentences in a formal language
    - Declarative approach to building an agent (or other system):
      - `TELL` it what it needs to know
      - Then it can `ASK` itself what to do - answers should follow from the KB

    - The inference engine contains domain-independent algorithms
    - The KB contains domain-specific content

  - A Simple Knowledge-Based Agent

    - ```pseudocode
      function KB-AGENT(percept) returns an action
      	static: KB, a knowledge base
      	        t, a counter, initially 0, indicating time
          
          TELL(KB, MAKE-PERCEPT-SENTENCE(percept, t))
          action <- ASK(KB, MAKE-ACTION-QUERY(t))
          TELL(KB, MAKE-ACTION-SEQUENCE(action, t))
          t <- t + 1
          return action
      ```

    - The agent must be able to:

      - Represent states, actions, etc.
      - Incorporate new percepts
      - Update internal representations of the world
      - Deduce hidden properties of the world
      - Deduce appropriate actions

  - Wumpus World

    - Informal Description:
      - Grid world
      - Pit causes breeze in adjacent cells
      - Wumpus causes stench in adjacent cells
      - Find the gold




## Lecture 13: Entailment, Syntax, and Semantics

- Logic in General

  - Logics are formal languages for representing information such that conclusions can be drawn
  - Syntax defines the sentences in the language
  - Semantics define the "meaning" of sentences
    - i.e., define truth of a sentence in a world

- Entailment

  - Entailment means that one thing follows from another

  - $$
    KB\models\alpha
    $$

  - Knowledge base `KB` entails sentence `α` iff `α` is true in all worlds where `KB` is true

  - Entailment is a relationship between sentences (i.e., syntax) that is based on semantics

- Models

  - Logicians typically think in terms of models, which are formally structured worlds with respect to which truth can be evaluated
  - We say `m` is a model of a sentence `α` if `α` is true in `m`
  - `M(α)` is the set of all models of `α`
  - Given sentences `α` and `β`, we say `α` entails `β` (`α ⊨ β`), iff `M(α) ⊆ M(β)`
  - `KB ⊨ α` iff `M(KB) ⊆ M(α)`
  - Model checking is the process of enumerating all possible models to check that `α` is true in all models in which `KB` is true

- Inference

  - $$
    KB\vdash_i\alpha
    $$

  - Sentence `α` can be derived from `KB` by some inference algorithm `i`

    - For instance, model checking is an inference algorithm

  - `i` is sound if whenever `KB` infers `α` using `i`, it is also true that `KB ⊨ α`

  - `i` is complete if whenever `KB ⊨ α`, it is also true that `KB` infers `α` using `i`

  - Preview:

    - We will define a logic (first-order logic) which is expressive enough to say almost anything of interest, and for which there exists a sound and complete inference procedure
    - That is, the procedure will answer any question whose answer follows from what is known by the `KB`


- Propositional Logic: Syntax

  - Propositional logic is the simplest logic, aka boolean logic
  - The proposition symbols `P1`, `P2`, etc. are atomic sentences
  - We can generate complex sentences based on atomic sentences using logical connectives
    - If `S` is a sentence, `¬S` is a sentence (negation)
    - If `S1` and `S2` are sentences, `S1 ∧ S2` is a sentence (conjunction)
    - If `S1` and `S2` are sentences, `S1 ∨ S2` is a sentence (disjunction)
    - If `S1` and `S2` are sentences, `S1 ⇒ S2` is a sentence (implication)
    - If `S1` and `S2` are sentences, `S1 ⇔ S2` is a sentence (biconditional)

- Propositional Logic: Semantics

  - Each model specifies true/false for each proposition symbol

  - Rules for evaluating truth with respect to a model `m`:

    - `¬S` is true iff `S` is false
    - `S1 ∧ S2` is true iff `S1` is true and `S2` is true
    - `S1 ∨ S2` is true iff `S1` is true of `S2` is true
    - `S1 ⇒ S2` is true unless `S1` is true and `S2` is false
    - `S1 ⇔ S2` is true iff `S1 ⇒ S2` is true and `S2 ⇒ S1` is true

  - Truth Table for Connectives

    - | `P`  | `Q`  | `¬P` | `P ∧ Q` | `P ∨ Q` | `P ⇒ Q` | `P ⇔ Q` |
      | :--: | :--: | :--: | :-----: | :-----: | :-----: | :-----: |
      |  F   |  F   |  T   |    F    |    F    |    T    |    T    |
      |  F   |  T   |  T   |    F    |    T    |    T    |    F    |
      |  T   |  F   |  F   |    F    |    T    |    F    |    F    |
      |  T   |  T   |  F   |    T    |    T    |    T    |    T    |



## Lecture 14: Validity, Satisfiability, and Resolution

- Inference by Enumeration

  - Depth-first enumeration of all models is sound and complete

  - ```pseudocode
    function TT-ENTAILS?(KB, α) returns true or false
    	inputs: KB, the knowledeg base, a sentence in propositional logic
    	        α, the query, a sentence in propositional logic
        
        symbols <- a list of the proposition symbols in KB and α
        return TT-CHECK-ALL(KB, α, symbols, {})
        
    function TT-CHECK-ALL(KB, α, symbols, model) returns true or false
    	if EMPTY?(symbols) then
    		if PL-TRUE?(KB, model) then return PL-TRUE?(α, model)
    		else return true // when KB is false, always return true
        else do
        	P <- FIRST(symbols)
        	rest <- REST(symbols)
        	return (TT-CHECK-ALL(KB, α, rest, model ∪ { P = true })
        			and
        			TT-CHECK-ALL(KB, α, rest, model ∪ { P = false }))
    ```

  - `O(2^n)` for `n` symbols; problem is co-NP-complete

- Logical Equivalence

  - Two sentences are logically equivalent iff they are true in the same models

  - $$
    \alpha\equiv\beta\text{ iff }\alpha\models\beta\text{ and }\beta\models\alpha
    $$

- Validity and Satisfiability

  - A sentence is valid if it is true in all models

    - e.g. `True`, `A ∨ ¬A`, `A ⇒ A`, `(A ∧ (A ⇒ B)) ⇒ B`, etc.

    - Validity is connected to inference via the Deduction Theorem:

      - $$
        \alpha\models\beta\text{ iff }(\alpha\Rightarrow\beta)\text{ is valid}
        $$

  - A sentence is satisfiable if it is true in some model

    - e.g. `A ∨ B`, `C`, etc.

  - A sentence is unsatisfiable if it is true in no models

    - e.g. `A ∧ ¬A`, etc.

    - Satisfiability is connected to inference via the following:

      - $$
        \alpha\models\beta\text{ iff }(\alpha\land\neg\beta)\text{ is unsatisfiable}
        $$

      - i.e., prove `β` by contradiction

- Proof Methods

  - Proof methods divide into (roughly) two kinds:
    - Application of inference rules (detailed)
      - Legitimate (sound) generation of new sentences from old
      - Proof = a sequence of inference rule applications
        - Can use inference rules as operators in a standard search algorithm

      - Typically require translation of sentences into a normal form

    - Model checking (brief)
      - Truth table enumeration (always exponential in `n`)
      - Improved backtracking, e.g., Davis-Putnam-Lagemann-Loveland heuristic search in model space (sound but incomplete)
        - e.g., min-conflicts-like hill-climbing algorithms

      - We can use `α/β` to imply that `α` can infer `β`

- Resolution

  - Conjunctive Normal Form (CNF - universal)

    - Conjunction of disjunctions of literals (clauses)

  - Resolution inference rule (for CNF):

    - $$
      \frac{l_1\lor\ ...\ \lor\ l_k,\quad m_1\lor\ ...\ \lor\ m_n}{l_1\lor\ ...\ \lor\ l_{i-1}\lor l_{i+1}\ \lor\ ...\ \lor\ l_k\lor m_1\ \lor\ ...\ \lor m_{j-1}\lor m_{j+1}\ \lor\ ...\ \lor m_n}
      $$

      - Where `l_i` and `m_j` are complementary literals

  - Resolution is sound and complete for propositional logic

- Conversion to CNF

  - Eliminate `⇔`, replacing `α ⇔ β` with `(α ⇒ β) ∧ (β ⇒ α)`
  - Eliminate `⇒`, replacing `α ⇒ β` with `¬α ∨ β`
  - Move `¬` inwards using de Morgan's rules
  - Apply distributivity law (`∨` over `∧`) and flatten

- Resolution Algorithm

  - Proof by contradiction, i.e., show `KB ∧ ¬α` unsatisfiable

  - ```pseudocode
    function PL-RESOLUTION(KB, α) returns true or false
    	inputs: KB, the knowledge base, a sentence in propositional logic
    			α, the query, a sentence in propositional logic
    			
        clauses <- the set of clauses in the CNF representation of KB ∧ ¬α
        new <- {}
        loop do
        	for each C_i, C_j in clauses do
        		resolvents <- PL-RESOLVE(C_i, C_j)
        		if resolvents contains the empty clause then return true
        		new <- new ∪ resolvents
            if new ⊆ clauses then return false
            clauses <- clauses ∪ new
    ```

  - Proves `KB ⊨ α`




## Lecture 15: Forward Chaining and First-Order Logic

- Forward Chaining

  - The knowledge base must be a conjunction of Horn clauses

    - Horn clauses: disjunction of literals of which at most one is positive
    - Horn clauses:
      - Literal
      - (Conjunction of symbols) ⇒ Literal
      - e.g., `C ∧ (B ⇒ A) ∧ (C ∧ D ⇒ B)`

  - Modus Ponens (for Horn Form): complete for Horn KBs

    - $$
      \frac{\alpha_1,...,\alpha_n,\quad\alpha_1\ \land\ ...\ \land\alpha_n\Rightarrow\beta}{\beta}
      $$

  - Can be used with forward chaining, which is very natural and runs in linear time

  - Idea: fire any rule whose premises are satisfied in the KB, add its conclusion to the KB, until the query is found

  - Algorithm:

    - ```pseudocode
      function PL-FC-ENTAILS?(KB, q) returns true or false
      	inputs: KB, the knowledge base, a set of propositional definite clauses
      			q, the query, a proposition symbol
          count <- a table, where count[c] is the number of symbols in c's premise
          inferred <- a table, where inferred[s] is initially false for all symbols
          agenda <- a queue of symbols, initially symbols known to be true in KB
          
          while agenda is not empty do
          	p <- POP(agenda)
          	if p = q then return true
          	if inferred[p] = false then
          		inferred[p] <- true
          		for each clause c in KB where p is in c.PREMISE do
          			decrement count[c]
          			if count[c] = 0 then add c.CONCLUSION to agenda
          return false
      ```

- Propositional Logic Summary

  - Logical agents apply inference to a knowledge base to derive new information and make decisions
  - Basic concepts of logic:
    - Syntax: formal structure of sentences
    - Semantics: truth of sentences with respect to models
    - Entailment: necessary truth of one sentence given another
    - Inference: deriving sentences from other sentences
    - Soundness: derivation produce only entailed sentences
    - Completeness: derivations can produce all entailed sentences

  - Propositional logic lacks expressive power

- First-Order Logic

  - Outline
    - Why first-order logic (FOL)?
    - Syntax and semantics of FOL
    - Kinship example
    - Wumpus world in FOL

  - Pros and Cons of Propositional Logic
    - Pros:
      - Propositional logic is declarative: pieces of syntax correspond to facts
      - Propositional logic allows partial/disjunctive/negated information (unlike most data structures and databases)
      - Propositional logic is compositional
      - Meaning in propositional logic is context-independent (unlike natural language, where meaning depends on context)

    - Cons:
      - Propositional logic has very limited expressive power

  - Whereas propositional logic assumes the world contains facts, FOL (like natural language) assumes the world contains objects, relations and functions
  - Syntax: Basic Elements
    - Constants
    - Predicates
      - Return true or false

    - Functions
    - Variables
    - Connectives
    - Equality
    - Quantifiers




## Lecture 16: First-Order Logic: Representation and Inference

- First-Order Logic: Representation

  - First-Order Logic Sentences

    - Atomic Sentences

      - Term: a logical expression that refers to an object
        - Constant
        - Variable
        - Function
          - Makes atomic sentences very expressive due to recursive nature

      - Atomic sentence: `predicate(term_1, ... , term_n)`

        - Examples:
          - `Brother(KingJohn, RichardTheLionheart)`
          - `>(Length(LeftLeg(Richard)), Length(LeftLeg(KingJohn)))`

      - An atomic sentence is true in a given model if the relation referred to by the predicate symbol holds among the objects referred to by the arguments

    - Complex Sentences
      - Complex sentences are made from atomic sentences using connectives
      - Examples:
        - `Sibling(KingJohn, Richard) ⇒ Sibling(Richard, KingJohn)`
        - `>(1, 2) ∨ ≥(2, 1)`
        - `>(1, 2) ∧ ¬>(1, 2)`

  - Truth in First-Order Logic
    - Sentences are true with respect to a model and an interpretation
    - Model contains ≥1 objects (domain elements) and relations among them
    - Interpretation specifies referents for:
      - Constant symbols => objects
      - Predicate symbols => relations
      - Function symbols => functional relations

    - An atomic sentence `predicate(term_1, ... , term_n)` is true iff the objects referred to by `term_1, ... , term_n` are in the relation referred to by `predicate`

  - Models for First-Order Logic
    - Entailment in propositional logic can be computed by enumerating models
    - We can enumerate the first-order logic models for a given KB vocabulary:

      - For each number of domain elements `n` from `1` to `INF`

        - For each `k`-ary predicate `P_k` in the vocabulary

          - For each possible `k`-ary relation on `n` objects

            - For each constant symbol `C` in the vocabulary

              - For each choice of referent for `C` from `n` objects
                - etc.

    - Computing entailment by enumerating first-order logic models is not easy!
      - In first-order logic, we use universal quantification and existential quantification to entail

  - Quantifiers

    - Universal Quantification
      - `∀ <variables> <sentence>`
      - Ex) everyone at UCLA is smart: `∀x At(x, UCLA) ⇒ Smart(x)`
      - `∀x P` is true in a model `m` iff `P` is true with `x` being each possible object in the model

        - Roughly speaking, equivalent to the conjunction of instantiations of `P`
          - `(At(KingJohn, UCLA) ⇒ Smart(KingJohn)) ∧ (At(Richard, UCLA) ⇒ Smart(Richard)) ∧ ...`

      - A Common Mistake to Avoid
        - Typically, `⇒` is the main connective with `∀`
        - Common mistake: using `∧` as the main connective with `∀`:
          - Ex) `∀x At(x, UCLA) ∧ Smart(x)` means "everyone is at UCLA and everyone is smart"

    - Existential Quantification
      - `∃ <variables> <sentence>`
      - Ex) someone at USC is smart: `∃x At(x, USC) ∧ Smart(x)`
      - `∃x P` is true in a model `m` iff `P` is true with `x` being some possible object in the model

        - Roughly speaking, equivalent to the disjunction of instantiations of `P`
          - `(At(KingJohn, USC) ∧ Smart(KingJohn)) ∨ (At(Richard, USC) ∧ Smart(Richard)) ∨ ...`

      - Another Common Mistake to Avoid
        - Typically, `∧` is the main connective with `∃`
        - Common mistake: using `⇒` as the main connective with `∃`:
          - Ex) `∃x At(x, USC) ⇒ Smart(x)` means "someone at USC is smart or there exists anyone who is not at USC"

    - Properties of Quantifiers
      - `∀x ∀y` is the same as `∀y ∀x`
      - `∃x ∃y` is the same as `∃y ∃x`
      - `∃x ∀y` is not the same as `∀y ∃x`

        - Example:
          - `∃x ∀y Loves(x, y)` means "there is a person who loves everyone in the world"
          - `∀y ∃x Loves(x, y)` means "everyone in the world is loved by at least one person"

      - Quantifier duality

        - i.e., De Morgan rules: each can be expressed using the other
          - `∀x Likes(x, IceCream)` is the same as `¬∃x ¬Likes(x, IceCream)`
          - `∃x Likes(x, Broccoli)` is the same as `¬∀x ¬Likes(x, Broccoli)`

  - Example: Kinship
    - Brothers are siblings: `∀x, y Brother(x, y) ⇒ Sibling(x, y)`
    - "Sibling" is symmetric: `∀x, y Sibling(x, y) ⇔ Sibling(y, x)`
    - A first cousin is a child of a parent's sibling: `∀x, y FirstCousin(x, y) ⇔ ∃p, ps Parent(p, x) ∧ Sibling(ps, p) ∧ Parent(ps, y)`

  - Example: Wumpus World
    - Perception of agent at time `t`: `Percept([Breeze, Glitter, Smell], t)`
    - Percept data implies certain facts about the current state:
      - `∀t, s, g Percept([Breeze, g, s], t) ⇒ Breeze(t)`
      - `∀s, b, t Percept([b, Glitter, s], t) ⇒ Glitter(t)`

    - Whether an agent is at square `s` at time `t`: `At(agent, s, t)`
    - Agent is at `s` and perceives a breeze, then `s` is breezy: `∀s, t At(agent, s, t) ∧ Breeze(t) ⇒ Breeze(s)`

  - Summary

    - First-order logic:
      - Objects and relations are semantic primitives
      - Syntax: constants, functions, predicates, equality, quantifiers

    - Increased expressive power: sufficient to define Wumpus world

- First-Order Logic: Inference

  - Outline

    - Reducing first-order inference to propositional inference
    - Unification
    - Generalized Modus Ponens
    - Resolution

  - Universal Instantiation (UI)

    - Whenever a KB contains a universally quantified sentence, we may add to the KB any instantiation of that sentence, where the logic variable `v` is replace by a concrete ground term `g`:

      - For any variable `v` and ground term `g`, we denote substitution `θ` as `θ = { v/g }` and `SUBST(θ, α)` as the result of applying the substitution `θ` to the sentence `α`

      - Every instantiation of a universally quantified sentence is entailed by it:

        - $$
          \frac{\forall v\quad\alpha}{\texttt{SUBST}(\{v/g\},\alpha)}
          $$

    - e.g., `∀x King(x) ∧ Greedy(x) ⇒ Evil(x)` yields:

      - `King(John) ∧ Greedy(John) ⇒ Evil(John)`
      - `King(Richard) ∧ Greedy(Richard) ⇒ Evil(Richard)`
      - `King(Father(John)) ∧ Greedy(Father(John)) ⇒ Evil(Father(John))`

  - Existential Instantiation (EI)

    - Whenever a KB contains an existentially quantified sentence `∃v α`, we may add a single instantiation of that sentence to the KB, where the logic variable `v` is replaced by a Skolem constant symbol `k` which must not appear elsewhere in the knowledge base:

      - $$
        \frac{\exists v\quad\alpha}{\texttt{SUBST}(\{v/k\},\alpha)}
        $$

    - Example 1: `∃x Crown(x) ∧ OnHead(x, John)` yields:

      - `Crown(C_1) ∧ OnHead(C_1, John)`
      - Provided `C_1` is a new constant symbol, called a Skolem constant

    - Example 2: from `∃x d(x^y) / dy = x^y` we obtain:

      - `d(e^y) / dy = e^y`
      - Provided `e` is a new constant symbol

  - Reduction to Propositional Inference

    - Instantiating all quantified sentences allows us to ground the KB, that is, to make the KB propositional

    - Example:

      - Suppose the KB contains just the following:
        - `∀x King(x) ∧ Greedy(x) ⇒ Evil(x)`
        - `King(John)`
        - `Greedy(John)`
        - `Brother(Richard, John)`

      - Instantiating the universal sentence in all possible ways, we have:
        - `King(John) ∧ Greedy(John) ⇒ Evil(John)`
        - `King(Richard) ∧ Greedy(Richard) ⇒ Evil(Richard)`
        - `King(John)`
        - `Greedy(John)`
        - `Brother(Richard, John)`

      - The new KB is propositionalized: propositional symbols are:
        - `King(John)`
        - `Greedy(John)`
        - `Evil(John)`
        - `King(Richard)`
        - etc.

    - Every first-order logic KB can be propositionalized so as to preserve entailment

      - Then, first-order logic inference can be done by: propositionalize KB and query, apply resolution, return result
      - Problem: with function symbols, there are infinitely many ground terms
        - Ex) `Father(Father(Father(John)))`

    - Theorem: Herbrand (1930), if a sentence `α` is entailed by a first-order logic KB, it is entailed by a finite subset of the propositional KB

      - ```pseudocode
        for n = 0 to INF do
        	create a propositional KB by instantiating with depth-n terms
        	see if α is entailed by this KB
        ```

      - Works if `α` is entailed, loops forever if `α` is not entailed

    - Theorem: Turing (1936), Church (1936), entailment in first-order logic is semidecidable - that is, algorithms exist that say yes to every entailed sentence, but no algorithm exists that also says no to every nonentailed sentence




## Lecture 17: First-Order Logic: Inference

- Problems with Propositionalization

  - Propositionalization seems to generate lots of irrelevant sentences
  - With `p` `k`-ary predicates and `n` constants, there are `pn^k` instantiations
  - With function symbols, it gets much worse 

- Unification

  - Instead of instantiating quantified sentences in all possible ways, we can compute specific substitutions "that make sense"

    - These are substitutions that unify abstract sentences so that rules can be applied

  - Unification: finding substitutions that make different logical expressions look identical

    - `UNIFY(α, β) = θ if SUBST(θ, α) = SUBST(θ, β)`

  - Suppose we want to know: whom does John know?

    - Answers to this query can be found by finding all sentences in the KB that unify with `Knows(John, x)`

    - | `α`              | `β`                 | `θ`                       |
      | ---------------- | ------------------- | ------------------------- |
      | `Knows(John, x)` | `Knows(John, Jane)` | `{ x/Jane }`              |
      | `Knows(John, x)` | `Knows(y, OJ)`      | `{ x/OJ, y/John }`        |
      | `Knows(John, x)` | `Knows(y, Mom(y))`  | `{ y/John, x/Mom(John) }` |
      | `Knows(John, x)` | `Knows(x, OJ)`      | Fail                      |
      | `Knows(John, x)` | `Knows(x_17, OJ)`   | `{ x/OJ, x_17/John }`     |

  - Standardizing Apart

    - The fourth unification above fails only because the two sentences happen to use the same variable name `x`
    - The problem can be avoided by standardizing apart one of the two sentences being unified, which means renaming its variables to avoid name clashes
    - Standardizing apart eliminates overlap of variables
      - e.g., `Knows(x, OJ)` to `Knows(x_17, OJ)`

  - Most General Unifier

    - `UNIFY(α, β)` returns a substitution that makes `α`, `β` look the same
    - Maybe more than one unifier
      - e.g. `UNIFY(Knows(John, x), Knows(y, z))` could return `{ y/John, x/z }` or `{ y/John, x/John, z/John }`
      - The first one is more general

    - For every unifiable pair of sentences, there is a single most general unifier (MGU) that is unique up to renaming and substitution of variables
      - For `UNIFY(Knows(John, x), Knows(y, z))`, the MGU is `{ y/John, x/z }` 

- Generalized Modus Ponens

  - For atomic sentences `p_i`, `p_i'`, q, where there is a substitution `θ` such that `∀i, SUBST(θ, p_i') = SUBST(θ, p_i)`, then:

    - $$
      \frac{p_1',p_2',...,p_n',\quad(p_1\land p_2\land\ ...\ \land p_n\Rightarrow q)}{\texttt{SUBST}(\theta,q)}
      $$

  - Definite Clauses

    - GMP is used with a KB of definite clauses
    - Definite clauses:
      - Atomic sentences
      - Implication whose premises is a conjunction of positive literals and whose conclusions is a single positive literal
      - All variables assumed universally quantified
      - Examples:
        - `King(John)`
        - `King(x) ∧ Greedy(x) ⇒ Evil(x)`

    - Comparison: Horn clauses in propositional logic
      - Proposition symbol
      - (conjunction of symbols) ⇒ symbol
      - Examples:
        - `C`
        - `A ∧ B ⇒ C`

  - Soundness of GMP

    - GMP is a sound inference rule (only derives entailed sentences)

- Conversion to CNF

  - Every sentence of first-order logic can be into an inferentially equivalent CNF sentence (it is satisfiable exactly when the original sentence is satisfiable)

  - Example: "Everyone who loves all animals is loved by someone"

    - $$
      \forall x\ [\forall y\ Animal(y)\Rightarrow Loves(x,y)]\Rightarrow[\exists y\ Loves(y,x)]
      $$

    - Eliminate biconditionals and implications:

      - $$
        \forall x\ [\neg\forall y\ \neg Animal(y)\lor Loves(x,y)]\lor[\exists y\ Loves(y,x)]
        $$

    - Move `¬` inwards:

      - $$
        \neg\forall x\ P\equiv\exists x\ \neg P\\
        \neg\exists x\ P\equiv\forall x\ \neg P
        $$

      - $$
        \forall x\ [\exists y\ \neg(\neg Animal(y)\lor Loves(x,y))]\lor[\exists y\ Loves(y, x)]\\
        \forall x\ [\exists y\ \neg\neg Animal(y)\land\neg Loves(x,y)]\lor[\exists y\ Loves(y, x)]\\
        \forall x\ [\exists y\ Animal(y)\land\neg Loves(x,y)]\lor[\exists y\ Loves(y, x)]\\
        $$

    - Standardize apart variables: each quantifier should use a different variable:

      - $$
        \forall x\ [\exists y\ Animal(y)\land\neg Loves(x,y)]\lor[\exists z\ Loves(z,x)]
        $$

    - Skolemize: a more general form of existential instantiation

      - Each existential variable is replaced by a Skolem function of the enclosing universally quantified variables

      - $$
        \forall x\ [Animal(F(x))\land\neg Loves(x,F(x))]\lor Loves(G(x),x)
        $$

    - Drop universal quantifiers:

      - $$
        [Animal(F(x))\land\neg Loves(x,F(x))]\lor Loves(G(x),x)
        $$

    - Distribute `∧` over `∨`:

      - $$
        [Animal(F(x))\lor Loves(G(x),x)]\land[\neg Loves(x,F(x))\lor Loves(G(x),x)]
        $$

- Resolution

  - Brief Summary

    - Full first-order version:

      - $$
        \frac{l_1\lor\ ...\ \lor l_k,\quad m_1\lor\ ...\ \lor m_n}{\texttt{SUBST}(\theta,(...\ \lor l_{i-1}\lor l_{i+1}\ \lor\ ...\ \lor m_{j-1}\lor m_{j+1}\lor\ ...))}
        $$

        - Where `UNIFY(l_i, ¬m_j) = θ`

    - Note `UNIFY(α, β) = θ` if `SUBST(θ, α) = SUBST(θ, β)`

    - Example: `[Animal(F(x)) ∨ Loves(G(x), x)]` and `[¬Loves(u, v) ∨ ¬Kills(u, v)]`

      - We could eliminate `Loves(G(x), x)` and `¬Loves(u, v)` with unifier `θ = { u/G(x), v(x) }` to produce the resolvent clause `Animal(F(x)) ∨ ¬Kills(G(x), x)`

  - Example KB

    - Assume:

      - The law says that it is a crime for an American to sell weapons to hostile nations
      - The country Nono, an enemy of America, has some missiles, and all of its missiles were sold to it by Colonel West, who is American

    - Prove that Colonel West is a criminal

    - KB:

      - $$
        American(x)\land Weapon(y)\land Sells(x,y,z)\land Hostile(z)\Rightarrow Criminal(x)\\
        Owns(Nono,M_1)\land Missile(M_1)\\
        \forall x\ Missile(x)\land Owns(Nono,x)\Rightarrow Sells(West,x,Nono)\\
        Missile(x)\Rightarrow Weapon(x)\\
        Enemy(x,America)\Rightarrow Hostile(x)\\
        American(West)\\
        Enemy(Nono,America)
        $$

    - Translate KB into CNF:

      - $$
        \neg American(x)\lor\neg Weapon(y)\lor\neg Sells(x,y,z)\lor\neg Hostile\lor Criminal(x)\\
        \neg Missile(x)\lor\neg Owns(Nono,x)\lor Sells(West,x,Nono)\\
        \neg Enemy(x,America)\lor Hostile(x)\\
        \neg Missile(x)\lor Weapon(x)
        $$

    - We need to show `KB ∧ ¬Criminal(West)` is unsatisfiable




## Lecture 18: Uncertainty and Probability

- Outline

  - Uncertainty
  - Probability
  - Syntax and Semantics
  - Inference
  - Independence and Bayes' Rule

- Uncertainty

  - Let action `A_t` be leaving for the airport `t` minutes before a flight
  - Will `A_t` get me there on time?
  - Here's a purely logical approach:
    - "`A_25` will get met there on time" may be wrong
    - "`A_25` will get me there on time if there's no accident on the bridge and it doesn't rain and my tires remain intact, etc." => there exists uncertainty

  - How do we handle uncertainty?
  - Methods for Handling Uncertainty
    - Making assumptions:
      - Assume my car doesn't have a flat tire
      - Assume `A_25` works unless contradicted by evidence

    - Issues: what assumptions are reasonable? How to handle contradictions?
    - Probability can help us
      - Given the available evidence, `A_25` will get me there on time with probability 0.04

    - What is probability?

  - Making Decisions Under Uncertainty
    - Suppose I believe the following:
      - `P(A_25 gets me there on time | ...) = 0.04`
      - `P(A_90 gets me there on time | ...) = 0.70`
      - `P(A_120 gets me there on time | ...) = 0.95`
      - `P(A_1440 gets me there on time | ...) = 0.9999`

    - Which action do I choose?
      - Depends on my preferences for missing flight vs. airport cuisine, etc.
      - Utility theory says that every state has a degree of usefulness, or utility, to an agent and that the agent will prefer states with higher utility
      - Decision theory = utility theory + probability theory

- Probability

  - Probabilistic assertions summarize effects of:

    - Laziness: failure to enumerate exceptions, qualifications, etc.
    - Ignorance: lack of relevant facts, initial conditions, etc.

  - Subjective or Bayesian probability:

    - Probabilities relate propositions to one's own state of knowledge
      - e.g., `P(A_25 | no reported accidents) = 0.06`

    - Probabilities of propositions change with new evidence:
      - e.g., `P(A_25 | no reported accidents, 5am) = 0.15`

  - Probability Basics

    - Begin with a set `Ω` - the sample space

      - e.g., 6 possible rolls of a dice
      - `ω ϵ Ω` is a sample point/possible world/atomic event

    - A probability space or probability model is a sample space with an assignment `P(ω)` for every `ω ϵ Ω` such that:

      - $$
        0\le P(\omega)\le1\\
        \sum_\omega P(\omega)=1
        $$

      - e.g., `P(1) = P(2) = P(3) = P(4) = P(5) = P(6) = 1/6`

    - An event `A` is any subset of `Ω`:

      - $$
        P(A)=\sum_{\{\omega\in A\}}P(\omega)
        $$

      - e.g., `P(die roll < 4) = P(1) + P(2) + P(3) = 1/6 + 1/6 + 1/6 = 1/2`

  - Random Variables

    - A random variable is a function from sample points to some range, e.g., the reals or Booleans

      - e.g., `Odd(1) = true`

    - `P` produces a probability distribution for any real value `X`:

      - $$
        P(X=x_i)=\sum_{\{\omega:X(\omega)=x_i\}}P(\omega)
        $$

      - e.g., `P(Odd = true) = P(1) + P(3) + P(5) = 1/6 + 1/6 + 1/6 = 1/2`

  - Propositions

    - We call events in probability as propositions in AI

      - Events: given Boolean random variables `A` and `B`:
        - Event `a` = set of sample points where `A(ω) = true`
        - Event `¬a` = set of sample points where `A(ω) = false`
        - Event `a ∧ b` = set of sample points where `A(ω) = true` and `B(ω) = true`

    - With Boolean variables, sample point = propositional logic model

      - e.g., `A = true`, `B = false`, or `a ∧ b`

    - Proposition = disjunction of events in which it is true

      - Example:

        - $$
          (a\lor b)\equiv(\neg a\land b)\lor(a\land\neg b)\lor(a\land b)\Rightarrow\\
          P(a\lor b)=P(\neg a\land b)+P(a\land\neg b)+P(a\land b)
          $$

  - Prior Probability

    - Prior or unconditional probabilities of propositions correspond to belief prior to arrival of any (new) evidence

      - e.g., `P(Cavity = true) = 0.1` and `P(Weather = sunny) = 0.72`

    - Probability distribution gives values for all possible assignments:

      - e.g., `P(Weather) = <0.72, 0.1, 0.08, 0.1>` (normalized, i.e., sums to `1`)
        - Tips: we use `P` (italic) to denote probability for a specific event, `P` (bold) to denote the distribution over all events - a vector

    - Joint probability distribution for a set of real values gives the probability of every atomic event on those real values (i.e., every sample point)

      - `P(Weather, Cavity)` = a 4 x 2 matrix of values:

        - |   `Weather =`    | `sunny` | `rain` | `cloudy` | `snow` |
          | :--------------: | :-----: | :----: | :------: | :----: |
          | `Cavity = true`  |  0.144  |  0.02  |  0.016   |  0.02  |
          | `Cavity = false` |  0.576  |  0.08  |  0.064   |  0.08  |

      - Every question about a domain can be answered by the joint distribution because every event is a sum of sample points

  - Conditional Probability

    - Conditional or posterior probabilities

      - e.g., `P(cavity | toothache) = 0.8`
        - If `toothache` and we have no further information, then 80% chance of `cavity`
        - Not "if `toothache` then 80% chance of `cavity`"
          - If we know more, e.g., `cavity` is also given, then we have `P(cavity | toothache, cavity) = 1`

    - New evidence may be irrelevant, allowing simplification

      - e.g., `P(cavity | toothache, LakersWin) = P(cavity | toothache) = 0.8`

    - Definition of conditional probability:

      - $$
        P(a|b)=\frac{P(a\land b)}{P(b)}\text{ if }P(b)\ne0
        $$

    - Product rule gives an alternative formulation:

      - $$
        P(a\land b)=P(a|b)P(b)=P(b|a)P(a)
        $$

    - A general version holds for whole distributions

      - e.g., `P(Weather, Cavity) = P(Weather | Cavity)P(Cavity)`
        - View as a 4 x 2 set of equations, not matrix multiplication

    - Chain rule is derived by successive application of product rule:

      - $$
        P(X_1,...,X_n)=P(X_1,...,X_{n-1})P(X_n|X_1,...,X_{n-1})=\\
        P(X_1,...,X_{n-2})P(X_{n-1}|X_1,...,X_{n-2})P(X_n|X_1,...,X_{n-1})=\\
        \prod^n_{i=1}P(X_i|X_1,...,X_{i-1})
        $$

- Syntax for Propositions

  - Propositional or Boolean random variables
    - e.g., `Cavity` (do I have a cavity?)
    - `Cavity = true` is a proposition, also written `cavity`

  - Discrete random variables (finite or infinite)
    - e.g., `Weather` is one of `<sunny, rain, cloudy, snow>`
    - `Weather = rain` is a proposition
    - Values must be exhaustive and mutually exclusive

  - Continuous random variables (bounded or unbounded)
    - e.g., `Temp = 21.6`; also allow e.g., `Temp < 22.0`

  - Arbitrary Boolean combinations of basic propositions

- Inference by Enumeration

  - A naive way of doing probablistic inference is inference by enumeration

  - Start with the joint distribution

  - For any proposition `φ`, sum the atomic events where it is true:

    - $$
      P(\phi)=\sum_{\omega:\omega\models\phi}P(\omega)
      $$

  - Normalization

    - Denominator can be viewed as a normalization constant `α`
    - General idea: compute distribution on query variable by fixing evidence variables and summing over hidden variables

  - Let `X` be all the variables

    - Typically, we want the posterior joint distribution fo the query variables `Y` given specific values `e` for the evidence variables `E`

  - Let the hidden variables be `H = X - Y - E`

    - Then the required summation of joint entries is done by summing out the hidden variables:

      - $$
        P(Y|E=e)=\alpha P(Y,E=e)=\alpha\sum_hP(Y,E=e,H=h)
        $$

  - The terms in the summation are joint entries because `Y`, `E`, and `H` together exhaust the set of random variables

  - Obvious problems:

    - Worst-case time complexity `O(d^m)` where `d` is the largest domain size
    - Space complexity `O(d^m)` to store the joint distribution
    - How to find the numbers for `O(d^m)` entries?

- Independence

  - Those problems can be solved by exploring independencies between variables
    - `A` and `B` are independent iff `P(A | B) = P(A)` or `P(B | A) = P(B)` or `P(A, B) = P(A)P(B)`

  - Example:
    - `P(Toothache, Catch, Cavity, Weather) = P(Toothache, Catch, Cavity)P(Weather)`
    - 32 entries reduced to 12; for `n` independent biased coins: `2^n` => `n`

  - Absolute independence is powerful, but rare
    - e.g., dentistry is a large field with hundreds of variables, none of which are independent

  - Conditional Independence
    - `P(Toothache, Cavity, Catch)` has `2^3 - 1 = 7` independent entries
    - If I have a cavity, the probability that the probe catches in it doesn't depend on whether I have a toothache:
      - `P(catch | toothache, cavity) = P(catch | cavity)`

    - The same independence holds if I haven't got a cavity:
      - `P(catch | toothache, ¬cavity) = P(catch | ¬cavity)`

    - `Catch` is conditionally independent of `Toothache` given `Cavity`:
      - `P(Catch | Toothache, Cavity) = P(Catch | Cavity)`

    - Equivalent statements:
      - `P(Toothache | Catch, Cavity) = P(Toothache | Cavity)`
      - `P(Toothache, Catch | Cavity) = P(Toothache | Cavity)P(Catch | Cavity)`

    - Write out full joint distribution using chain rule:
      - `P(Toothache, Catch, Cavity) = P(Toothache | Catch, Cavity)P(Catch, Cavity) = P(Toothache | Catch, Cavity)P(Catch | Cavity)P(Cavity) = P(Toothache | Cavity)P(Catch | Cavity)P(Cavity)`
      - i.e., `2 + 2 + 1 = 5` independent numbers

    - In most cases, the use of conditional independence reduces the size of the representation of the joint distribution from exponential in `n` to linear in `n`
    - Conditional independence is our most basic and robust form of knowledge about uncertain movements

- Bayes' Rule

  - Bayes' Rule can be using in probability inference when we have `P(b | a)` but not `P(a | b)`

  - Product rule `P(a ∧ b) = P(a | b)P(b) = P(b | a)P(a)` => Bayes' Rule:

    - $$
      P(a|b)=\frac{P(b|a)P(a)}{P(b)}
      $$

    - Or, in distribution form:

      - $$
        P(Y|X)=\frac{P(X|Y)P(Y)}{P(X)}-\alpha P(X|Y)P(Y)
        $$

  - Useful for assessing diagnostic probability from causal probability:

    - $$
      P(Cause|Effect)=\frac{P(Effect|Cause)P(Cause)}{P(Effect)}
      $$

  - This is an example of a naive Bayes model:

    - $$
      P(Cause,Effect_1,...,Effect_n)=P(Cause)\prod_iP(Effect_i|Cause)
      $$

    - Total number of parameters is linear in `n`




## Lecture 19:

- 



## Reading 1: Introduction

- What is AI?

  - Definitions of AI concern themselves with 2 dimensions

    - Thought processes and reasoning vs. behavior

    - Human performance vs. ideal/rational performance

    - |              | Humanly                                                      | Rationally                                                   |
      | ------------ | ------------------------------------------------------------ | ------------------------------------------------------------ |
      | **Thinking** | "The exciting new effort to make computers think... machines with minds, in the full and literal sense."<br /><br />"[The automation of] activities that we associate with human thinking, activities such as decision-making, problem solving, learning..." | "The study of mental faculties through the use of computational models."<br /><br />"The study of the computations that make it possible to perceive, reason, and act." |
      | **Acting**   | "The art of creating machines that perform functions that require intelligence when performed by people."<br /><br />"The study of how to make computers do things at which, at the moment, people are better." | "Computational Intelligence is the study of the design of intelligent agents."<br /><br />"AI... is concerned with intelligent behavior in artifacts." |

      - All 4 methods have been followed in history
      - Human-centered approach must be in part an empirical science, while a rationalist approach centers around a combination of mathematics and engineering

  - Acting Humanly: The Turing Test Approach

    - The Turing Test was designed to provide a satisfactory operational definition of intelligence
      - Computer passes the test if the interrogator cannot tell whether the written responses come from a person or a computer
      - Computer must possess the following capabilities to pass:
        - Natural language processing to enable it to communicate successfully
        - Knowledge representation to store what it knows/hears
        - Automated reasoning to use the stored information to answer questions and to draw new conclusions
        - Machine learning to adapt to new circumstances and to detect and extrapolate patterns

      - Deliberately avoids physical interaction, as appearance is not necessary for intelligence
        - The total Turing Test includes a video signal to test the subject's perceptual abilities, necessitating:
          - Computer vision to perceive objects
          - Robotics to manipulate objects and move about

    - The requirements to pass the Turing Test compose most of AI
      - Belief that it is more important to study the underlying principles of intelligence than to actually pass the Turing Test

  - Thinking Humanly: The Cognitive Modeling Approach

    - We must determine how humans think through one of 3 methods:
      - Introspection - trying to catch our own thoughts as they go by
      - Psychological experiments - observing a person in action
      - Brain imaging - observing the brain in action
      - Once a precise theory of the mind is developed, we can then express said theory in a computer program
        - If the program's I/O matches corresponding human behavior, then that's evidence that some of the program's mechanisms are accurate

      - Cognitive science brings together computer models from AI and experimental techniques from psychology to construct precise and testable theories of the human mind

  - Thinking Rationally: The "Laws of Thought" Approach

    - Idea is to build upon logical notation solvers to create intelligent systems

    - Two main obstacles to this approach:
      - Not easy to take informal knowledge and state it in the formal terms required by logical notation
        - Especially when the knowledge is not absolutely certain

      - Large difference between solving a problem "in principle" and in practice

    - Even problems with just a few hundred facts can exhaust computational resources without guidance
    - The same obstacles appear in all approaches, but appeared here first
    - Emphasizes correct inferences

  - Acting Rationally: The Rational Agent Approach

    - Computer agents are expected to operate autonomously, perceive their environment, persist over a prolonged period of time, adapt to change, and create/pursue goals
    - A rational agent is one that acts to achieve the best outcome or the best expected outcome in cases of uncertainty
    - Sometimes involves correct inferences
      - One way to act rationally is to reason logically to a conclusion and then act upon it
      - This approach must still act in cases where there is no provably correct action
      - There also exist ways of acting rationally that don't involve inference at all
    - The skills needed for the Turing Test enable this action
      - Knowledge representation and reasoning enable agents to reach good decisions
      - Natural language processing allows for the generation of comprehensible sentences to get by in a complex society
      - Learning improves our ability to generate effective behavior
    - 2 advantages:
      - More general than the "laws of thought"/thinking rationally approach
        - Correct inference is just one of several possible mechanisms for achieving rationality
      - More amenable to scientific development than human-centric approaches
        - Rationality can be provably achieved
        - Standard of rationality is mathematically well defined, while human standards are based on the sum total of all things humans do
    - Note that achieving perfect rationality is impossible in complex environments
      - Computational demands are too high
      - Adopt the working hypothesis that perfect rationality is a good starting point for analysis
      - Idea of limited rationality (acting appropriately when there is not enough time to do all the computations necessary for perfect rationality)

- The Foundations of Artificial Intelligence

  - Philosophy
    - Main ideas:
      - Can formal rules be used to draw all valid conclusions?
      - How does the mind arise from a physical brain?
      - Where does knowledge come from?
      - How does knowledge lead to action?
    - The Mind
      - Rationalism: focus on the power of reasoning in understanding the world
        - Descartes, Aristotle, Leibnitz, etc.
      - Dualism: there is a part of the human mind/soul/spirit beyond nature, exempt from physical laws
        - Animals didn't possess this quality, and could therefore be treated as machines
      - Materialism: the brain's operation based on the laws of physics constitutes the mind
        - Free will is just the way possible choices appear to the entity in question
    - The Source of Knowledge
      - Empiricism: "Nothing is in the understanding, which was not first in the senses"
      - Principle of induction: general rules are acquired by exposure to repeated associations between their elements
      - Logical positivism: all knowledge can be characterized by logical theories connects to observation sentences that correspond to sensory inputs
        - Combines rationalism and empiricism
      - Confirmation theory: attempts to analyze the acquisition of knowledge from experience
    - The Connection between Knowledge and Action
      - Only by understanding how actions are justified can we understand how to build an agent whose actions are rational
      - Goal-based analysis useful, but limited
        - Doesn't say what to do when multiple actions achieve the goal or when no action successfully achieves it
  - Mathematics
    - Main ideas:
      - What are the formal rules to draw valid conclusions?
      - What can be computed?
      - How do we reason with uncertain information?
    - While philosophers staked out the fundamental ideas of AI, its transition to a formal science involves mathematical formalization in the areas of logic, computation, and probability
    - Formal Logic
      - Development begins with the working out of details of propositional/Boolean logic
      - Later extended to include objects and relations, resulting in today's first-order logic
      - Theory of reference introduced that shows how to relate the objects in logic to real-world objects
    - Limits of Logic and Computation
      - The first nontrivial algorithm is Euclid's algorithm for computing GCDs
      - Efforts to formalize general mathematical reasoning as logical deduction
        - Effective procedure developed to prove any true statement in first-order logic
        - First-order logic could not capture the principle of mathematical induction needed to characterize the natural numbers
      - Limits on deduction shown by the incompleteness theorem
        - Any formal theory as strong as Peano arithmetic contains true statements that are undecidable in the sense that they have no proof within the theory
        - Some functions on the integers cannot be represented by an algorithm/computed
      - The Church-Turing Thesis
        - States that the Turing machine is capable of computing any computable function
          - Serves as the definition for "computable"
        - Shows that there are some problems that no Turing machine can compute
          - i.e., the halting problem
    - Tractability
      - A problem is called intractable if the time required to solve instances of the problem grows exponentially with the size of the instances
      - Emphasis on the distinction between polynomial and exponential growth in complexity
        - Strive to solve the overall problem of generating intelligent behavior into tractable subproblems rather than intractable ones
    - NP-Completeness
      - Solution for how to recognize an intractable problem
      - Shows the existence of large classes of canonical combinatorial search and reasoning problems that are NP-complete
      - Any problem class to which the class of NP-complete problems can be reduced is likely to be intractable
        - This has not been formally proved
      - Increasing speed of computers contrasted with the requirement for a careful use of resources needed to characterize intelligent systems
      - Work in AI helps differentiate between NP-hard and NP-complete problem classes
    - Probability
      - Development of theories helps to deal with uncertain measurements and incomplete theories
      - Underlies most modern approaches to uncertain reasoning in AI systems
  - Economics
    - Main ideas:
      - How should we make decisions so as to maximize payoff?
      - How should we do this when others may not go along?
      - How should we do this when the payoff may be far in the future?
    - Economics as a study of how people make choices that lead to preferred outcomes
      - Mathematical treatment of "preferred outcomes"/utility was formalized
    - Decision Theory
      - Combines probability theory with utility theory
      - Provides a formal and complete framework for decisions made under uncertainty
      - Suitable for "large" economies where each agent doesn't need to worry about the actions of other agents
        - "Small" economies are much more like a game: the actions of agents impact the utility of other agents
        - Game theory introduces the surprising result that a rational agent should adopt policies that appear to be randomized in some games
          - Does not offer unambiguous prescription for selecting actions
    - Operations Research
      - Economists didn't address how to make rational decisions when payoffs from actions are not immediate but are instead a result of several actions taken in sequence
      - Formalization of a class of sequential decision problems called Markov decision processes
    - Satisficing
      - Despite importance of economics and operations research, AI developed on a separate path for a long period of time
        - Due to apparent complexity of making rational decisions
      - Models based on satisficing gave a better description of actual human behavior
        - Making decisions that aren't optimal, but are good enough
  - Neuroscience
    - Main idea: how do brains process information?
    - The study of neuroscience, particularly the brain
    - Data on mappings between areas of the brain and parts of the body they control or receive sensory input from
    - Brains vs. Digital Computers
      - Computers have a cycle time that is a million times faster than a brain
      - Brains have far more storage and interconnection
        - Largest supercomputers are an exception
      - Futurists point to an approaching singularity at which computers will reach a superhuman level of performance
        - Despite reaching the brain's capacity, we still don't know how to achieve the brain's level of intelligence
  - Psychology
    - Main idea: how do humans and animals think and act?
    - Branches
      - Behaviorism: studied only objective measures of the percepts/stimulus given to an animal and its response
        - Worked for animals, found less success with humans
      - Cognitive psychology: views the brain as an information-processing device
        - Reestablished the legitimacy of beliefs and goals, arguing for their scientific nature
        - Specified 3 key steps for processing:
          - The stimulus is translated into an internal representation
          - The representation is manipulated by cognitive processes to derive new internal representations
          - These are in turn translated back to action
        - Idea that a cognitive theory should be like a computer program
          - Should describe a detailed information-processing mechanism whereby some cognitive function might be implemented
  - Computer Engineering
    - Main idea: how can we build an efficient computer?
    - AI necessitates intelligence and an artifact (the computer)
    - Computer generations bring with them an increase in speed/capacity and a decrease in price
      - Performance was doubling every ~18 months until power dissipation problems led to manufacturers turning attention away from clock speed and towards multiplying CPU cores
    - The software side of computer engineering has supplied the OSes, programming languages, and tools needed to write modern programs
      - Work in AI has assisted in the development of aspects of mainstream computer science: time sharing, interactive interpreters, personal computers, rapid development environments, linked lists, automatic storage management, and key concepts of symbolic/functional/declarative/OOP
  - Control Theory and Cybernetics
    - Main idea: how can artifacts operate under their own control?
    - Control Theory
      - View of purposive behavior as arising from a regulatory mechanism trying to minimize "error" - the difference between current state and goal state
      - Modern control theory has its goal as the design of systems that maximize an objective function over time
        - Roughly matches our view of AI: designing systems that behave optimally
      - Different from AI in that the main tools of control theory (calculus and matrix algebra) lend themselves to systems that are describable by fixed sets of continuous variables
        - AI was founded in part as a way to escape from these perceived limitations
        - Logical inference and computation allow AI researchers to consider problems like language, vision, and planning that fall outside the realm of control theory
  - Linguistics
    - Main idea: how does language relate to thought?
    - Computational linguistics/natural language processing
      - Understanding language requires an understanding of the subject matter and context, not just the structure of sentences
      - Early work in knowledge representation (how to put knowledge into a form that computers can interact with) was tied to language

- The State of the Art
  - Robotic Vehicles
  - Speech Recognition
  - Autonomous Planning and Scheduling
  - Game Playing
  - Spam Fighting
  - Logistics Planning
  - Robotics
  - Machine Translation

- Summary
  - Different people approach AI with different goals in mind
    - Two important questions:
      - Are you concerned with thinking or behavior?
      - Do you want to model humans or work from an ideal standard?
  - We focus on rational action
    - Ideally, an intelligent agent should take the best possible course of action in a situation
    - Study the problem of building agents that are intelligent in this sense
  - Philosophers made AI conceivable by considering the ideas that the mind is in some ways like a machine, that it operates on knowledge encoded in some internal language, and that thought can be used to choose what actions to take
  - Mathematicians provided the tools to manipulate statements of logical certainty as well as uncertain, probabilistic statements
    - They also set the groundwork for understanding computation and reasoning about algorithms
  - Economists formalized the problem of making decisions that maximize the expected outcome to the decision maker
  - Neuroscientists discovered some facts about how the brain works and the ways in which it is similar to and different from computers
  - Psychologists adopted the idea that humans and animals can be considered information-processing machines
    - Linguists showed that language use fits into this model
  - Computer engineers provided the ever-more-powerful machines that make AI applications possible
  - Control theory deals with designing devices that act optimally on the basis of feedback from the environment
    - Initially, the mathematical tools of control theory were quite different from AI, but the fields are coming closer together
  - The history of AI has had cycles of success, misplaced optimism, and resulting cutbacks in enthusiasm and funding
    - There have also been cycles of introducing new creative approaches and systematically refining the best ones
  - AI has advanced more rapidly in the past decade because of greater use of the scientific method in experimenting with and comparing approaches
  - Recent progress in understanding the theoretical basis for intelligence has gone hand in hand with improvements in the capabilities of real systems
    - The subfields of AI have become more integrated, and AI has found common ground with other disciplines



## Reading 2: Intelligent Agents

- Agents and Environments
  - Agents
    - An agent is anything that can be viewed as perceiving its environment through sensors and acting upon that environment through actuators
    - Human agents have eyes, ears, and other senses as sensors, and hands, legs, vocal tract, etc. for actuators
    - Robotic agents might have cameras and infrared range finders for sensors and various motors for actuators
    - Software agents might have keystrokes, file contents, and network packets as sensors and displays, writing to files, and sending network packets as actuators

  - Percepts
    - Percepts refer to the agent's perceptual inputs at any given instant
    - An agent's percept sequence is the complete history of everything the agent has ever perceived
      - An agent's choice of action at any given instant can depend on the entire percept sequence so far, but not on anything it hasn't perceived

    - An agent's behavior is described by the agent function that maps any given percept sequence to an action

  - Agent Program
    - For most agents, the process of tabulating the agent function that describes the agent would produce an infinite table
      -  Unless the length of percept sequences was bounded
      -  The table is an external characterization of the agent
      -  The agent function is an abstract mathematical description
    - The agent program is a concrete implementation, running within some physical system
      - The internal representation for a given agent
    - Question is: what makes an agent good or bad, intelligent or stupid?
  - The notion of an agent is meant to be a tool for analyzing systems, not a characterization that divides the world into agents and non-agents
  
- Good Behavior: The Concept of Rationality

  - Rational Agents
    - An agent that does the right thing
      - Every entry in the agent function table is filled out correctly

    - What does this mean?
      - We answer this by evaluating the consequences of the agent's behavior
      - If the sequence of actions generated by the percepts that the agent receives is desirable, then the agent has performed well
        - This notion of desirability is captured by a performance measure that evaluates any given sequence of environment states
          - Note the evaluation is based on the state of the environment, not the agent, as the agent may be able to deceive itself into thinking it did well

        - This performance measure must be devised according to the circumstances
          - As a general rule, it is better to design performance measures according to what one actually wants in the environment, not how one expects the agent to behave

  - Rationality
    - 4 main factors:
      - The performance measure that defines the criterion for success
      - The agent's prior knowledge of the enviromment
      - The actions that the agent can perform
      - The agent's percept sequence to date

    - Definition of a rational agent:
      - For each possible percept sequence, a rational agent should select an action that is expected to maximize its performance measure, given the evidence provided by the percept sequence and whatever built-in knowledge the agent has

  - Omniscience, Learning, and Autonomy
    - Omniscience
      - An omniscient agent knows the actual outcome of its actions and can act accordingly
        - Impossible in reality

      - Rationality is not the same as perfection
        - Rationality maximizes expected performance, while perfection maximizes actual performance
        - It is impossible to design an agent to achieve perfection
        - Rationality only depends on the percept sequence to date

    - Information Gathering
      - The act of doing actions in order to modify future percepts
      - Exploration in an unknown environment
      - It is irrational to act with an uninformative percept sequence

    - Learning
      - The rational agent must learn as much as possible from what it perceives
        - An agent's initial configuration may contain information about its environment that can be modified and altered by the agent's experience

      - In extreme cases, the agent's environment is know a priori, resulting in the agent not needing to perceive or learn, just act
        - Results in a fragile agent that is incapable of adaptation

    - Autonomy
      - To the extent that an agent relies on the prior knowledge of its designer rather than on its own percepts, we say that the agent lacks autonomy
      - Rational agents should be autonomous
        - Should learn what it can to compensate for missing information
        - Complete autonomy is not required from that start
          - When the agent has little to no experience, it has no information it can act on to achieve autonomy
          - We provide the agent with some initial knowledge and the ability to learn in hopes it can eventually achieve autonomy by becoming independent of its initial knowledge

      - Allows an agent to succeed in a vast variety of environments

- The Nature of Environments

  - Task Environments
    - Task environments are the problems to which rational agents are the solutions
    - The type of task environment directly affects the appropriate design for the agent program

  - Specifying the Task Environment
    - PEAS (Performance, Environment, Actuators, Sensors)
      - A description of the task environment

    - The first step to designing an agent must be to specify the task environment as fully as possible

  - Properties of Task Environments
    - Fully Observable vs. Partially Observable
      - If an agent's sensors give it access to the complete state of the environment at each point in time, then that task environment is fully observable
        - If the sensors detect all aspects that are relevant to the choice of action
          - Relevance depends on the performance measure

        - Convenient, as the agent doesn't need to maintain an internal state to keep track of the world

      - Otherwise, the environment is partially observable
        - May be due to noisy/inaccurate sensors, or because parts of the state are absent from sensor data

      - If the agent has no sensors at all, the environment is unobservable
        - The agent's goals may still be achievable, even with certainty

    - Single Agent vs. Multiagent
      - Careful determination between things that must be treated as agents
        - Key distinction is whether an object's behavior is best described as maximizing a performance measure whose value depends on an agent's behavior

      - A competitive multiagent environment is one in which the maximization of one agent's performance  measures leads to the minimization of another's
        - i.e., chess

      - A cooperative multiagent environment is one in which the maximization of one agent's performance measures leads to the maximization of another's
        - i.e., driving
        - Can be partially competitive as well (parking spaces)

      - Agent design problems in multiagent environments are often different from single-agent environments
        - Communication often emerges as rational behavior
        - In some situations, randomized behavior is rational because it avoids the pitfalls of predictability

    - Deterministic vs. Stochastic
      - If the next state of the environment is completely determined by the current state and the action executed by the agent, the environment is deterministic
        - Otherwise, it is stochastic
        - In principle, an agent doesn't need to worry about uncertainty in a fully observable, deterministic environment
          - This ignores the uncertainty that arises from actions of other agents in a multiagent environment
          - Partially observable environments may appear to be stochastic => reality
          - An environment is uncertain if it is not fully observable or not deterministic

      - Use of "stochastic" implies uncertainty about outcomes is quantified in terms of probabilities
        - A nondeterministic environment is one in which actions are characterized by possible outcomes, but with no probabilities attached
          - Usually associated with performance measures that require the agent to succeed for all possible outcomes of its actions

    - Episodic vs. Sequential
      - In an episodic task environment, the agent's experience is divided into atomic episodes
        - Each episode sees the agent receiving a percept and then performing a single action
        - The next episode doesn't depend on the actions taken in previous episodes
        - Applies to many classification tasks

      - In sequential environments, the current decision may affect all future decisions
        - Much more complex than episodic environments because the agent needs to be able to think ahead

    - Static vs. Dynamic
      - If the environment can change while an agent is deliberating, then the environment is dynamic for that agent
        - Otherwise, it is static
        - Environment is constantly asking the agent what to do
          - If it hasn't decided yet, it has decided to do nothing

      - Static environments are easy to deal with since the agent doesn't need to keep looking at the world while deciding on an action
        - It also doesn't need to worry about the passage of time

      - If the environment itself doesn't change with the passage of time, but the agent's performance score does, then the environment is semidynamic

    - Discrete vs. Continuous
      - Applies to the state of the environment, the way time is handled, and to the percepts and actions of the agent

    - Known vs. Unknown
      - Speaks to the agent/designer's state of knowledge about the "laws of physics" of the environment
        - Not strictly a property of the environment

      - In a known environment, the outcomes (or outcome probabilities) for all actions are given
      - In an unknown environment, the agent must learn how it works to make good decisions
      - Distinguish between known/unknown and fully/partially observable

    - Hardest case is partially observable, multiagent, stochastic, sequential, dynamic, continuous, and unknown
      - Classifications may depend on how the task environment is defined

- The Structure of Agents

  - `Agent = Architecture + Program`

    - The agent program must map from percepts to actions
    - The architecture is some sort of computing device with physical sensors and actuators that the program runs on

  - Agent Programs

    - General skeleton: take the current percept as input from the sensors and return an action to the actuators
    - Note the difference between the agent program and the agent function
      - The agent program takes the current percept as input, as nothing else is available from the environment
      - The agent function takes the entire percept history as input

    - The table-driven approach to agent construction is doomed to failure
      - Too many possible percepts creates too large of a lookup table
        - No physical agent in this universe will have the space to store the table
        - The designer wouldn't have time to create the table
        - No agent could ever learn all the right table entries from experience
        - The designer still has no guidance about how to fill in the table entries

      - Key challenge is to find out how to write programs that, to the extent possible, produce rational behavior from a smallish program rather than from a vast table

    - 4 basic kinds of agent programs:
      - Simple reflex agents
      - Model-based reflex agents
      - Goal-based agents
      - Utility-based agents

    - Each kind of agent program combines particular components in particular ways to generate actions
      - These should be converted into learning agents that can improve the performance of their components as to generate better actions

  - Simple Reflex Agents

    - These agents select actions on the basis of the current percept, ignoring the rest of the percept history

    - Example:

      - ```pseudocode
        function REFLEX_VACUUM_AGENT([location, status]) returns an action
        	if status = DIRTY then return SUCK
        	else if location = A then return RIGHT
        	else if location = B then return LEFT
        ```

    - Simple reflex behaviors occur even in more complex environments

      - These connections are called a condition-action rule (if-then)

    - Example above is specific to a particular environment

      - More general and flexible approach is first to build a general-purpose interpreter for condition-action rules, and then to create rule sets for specific task environments

      - ```pseudocode
        function SIMPLE_REFLEX_AGENT(percept) returns an action
        	persistent: rules, a set of condition-action rules
        	
        	state <- INTERPRET_INPUT(percept)
        	rule <- RULE_MATCH(state, rules)
        	action <- rule.ACTION
        	return action
        ```

    - Simple, but of limited intelligence

      - Improved example above only works if the environment is fully observable
      - Often results in unavoidable infinite loops given a partially observable environment
        - Escape is possible if the agent can randomize its actions (usually not rational)

  - Model-Based Reflex Agents

    - Most effective way to handle partial observability is for the agent to keep track of the parts of the world it can't see now

      - Should maintain some sort of commercial state that depends on the percept history, reflecting some of the unobserved aspects of the current state

    - Updates to this internal state require 2 types of knowledge:

      - Information about how the world evolves independently of that agent
      - Information about how the agent's own actions affect the world

    - Knowledge about how the world works is called a model of the world

      - Therefore, an agent that uses such a model is called a model-based agent

    - Example:

      - ```pseudocode
        function MODEL_BASED_REFLEX_AGENT(percept) returns an action
        	persistent: state, the agent's current perception of the world state
        				model, a description of how the next state depends on current
        				       state and action
        				rules, a set of condition-action rules
        				action, the most recent action, initially none
        				
        	state <- UPDATE_STATE(state, action, percept, model)
        	rule <- RULE_MATCH(state, rules)
        	action <- rule.ACTION
        	return action
        ```

    - Details about how models and states are represented vary widely depending on the type of environment and the particular technology used in the agent design

      - Seldom possible to determine the current state of a partially observable environment exactly, must use the agent's best guess(es)

    - The agent's internal state doesn't have to describe "what the world is like now" in a literal sense

  - Goal-Based Agents

    - Knowing something about the current state of the environment is not always enough to decide what to do
      - In addition to a current state description, the agent needs some sort of goal information that describes desirable situations
      - The agent program can combine this information with a model to choose actions that achieve the goal

    - Can be straightforward, like when goal satisfaction results immediately from a single action
      - Can be complex, like when the agent has to consider long sequences of twists and turns to find a way to achieve the goal
      - Search and planning are subfields of AI devoted to finding action sequences that achieve the agent's goals

    - Decision making of this kind is fundamentally different from the condition-action rules from earlier
      - Involves consideration of the future
        - "What will happen if I do this?"
        - "Will that make me happy?"

      - Not addressed in reflex agents, who map directly from percept to action

    - More flexible because the knowledge that supports its decisions is represented explicitly and can be modified
      - Automatically causes relevant behaviors to be altered to suit the new conditions

  - Utility-Based Agents

    - Goals simply serve to provide a crude binary distinction between happy and unhappy states
      - A more general purpose measure should allow a comparison of different states based on how happy they would make the agent => utility

    - An agent's utility function is an internalization of the performance measure
      - If the internal utility function on the external performance measure are in agreement, than an agent that chooses actions to maximize its utility will be rational according to the external performance measure

    - Utility-based agents have many advantages in flexibility and learning
    - In 2 situations, goals are inadequate, but utility-based agents can still behave rationally
      - When there are conflicting, mutually exclusive goals, the utility function specifies the appropriate tradeoff
      - When there are several goals that the agent can aim for, none of which are certain, utility provides a way in which the likelihood of success can be weighed against the goals' importance

    - Due to the ubiquitous nature of partial observability and stochasticity in the real world, decision making under uncertainty is common
      - Therefore, a rational utility-based agent behaves according to the expected utility of the action outcomes
        - The utility that the agent expects to derive, on average, given the probabilities and utilities of available options

      - A utility-based agent must model and keep track of its environment, choose the utility-maximizing course of action, and avoid computational complexity

  - Learning Agents

    - Learning allows the agent to operate in initially unknown environments and to become more competent than its initial knowledge alone might allow
    - 4 conceptual components:
      - The learning element is responsible for making improvements
        - Design depends on the design of the performance element
        - Simplest form of learning is observing directly from the percept sequence
          - "How the world evolves"
          - "What my actions do"

      - The performance element is responsible for selecting external actions
        - Takes in percepts and decides on actions

      - The critic provides feedback on how the agent is doing and decides how the performance element should be modified to do better in the future
        - Judges with respect to a fixed performance standard which must not be modified to fit the agent's behavior
        - Required since the percepts themselves provide no indication of the agent's success

      - The problem generator is responsible for suggesting actions that will lead to new and informative experiences
        - Suboptimal actions in the short run may lead to larger payoffs in the long run
        - Without the problem generator, the agent would continue behaving in the way it already thinks is optimal, never looking beyond those actions for potentially better solutions

    - Learning in intelligent agents can be summarized as a process of modification of each component of the agent to bring the components into closer agreement with the available feedback information, thereby improving the overall performance of the agent

  - How the Components of Agent Programs Work

    - Place representations along an axis of increasing complexity and expressive power:
      - Atomic
      - Factored
      - Structured

    - Atomic Representation
      - Each state of the world is indivisible
      - Two different atomic states have nothing in common
      - Algorithms underlying search and game-playing, Hidden Markov models, and Markov decision processes all work with atomic representations

    - Factored Representation
      - Splits up each state into a fixed set of variables or attributes, each of which can have a value
      - Two different factored states may share some attributes while differing in others
        - Makes it much easier to work out how to turn one state into another

      - Can represent uncertainty by leaving attributes blank
      - Algorithms underlying constraint satisfaction, propositional logic, planning, Bayesian networks, and machine learning use factored representations

    - Structured Representation
      - Objects and their various and varying relationships can be described explicitly
      - Algorithms underlying relational databases and first-order logic, first-order probability models, knowledge-based learning, and natural language understanding use structured representations

    - Expressiveness
      - A more expressive representation can capture, at least as concisely, everything a less expressive one can capture, plus some more
        - Often, the more expressive language is much more concise

      - Reasoning and learning become more complex as the expressive power of the representation increases
      - To gain the benefits of expressive representations while avoiding their drawbacks, intelligent systems may operate at all points along the axis simultaneously

- Summary

  - An agent is something that perceives and acts in an environment
    - The agent function for an agent specifies the action taken by the agent in response to any percept sequence
    - The performance measure evaluates the behavior of the agent in an environment
      - A rational agent acts so as to maximize the expected value of the performance measure, given the percept sequence it has seen so far

    - A task environment specification includes the performance measure, the external environment, the actuators, and the sensors
      - In designing an agent, the first step must always be to specify the task environment as fully as possible

    - Task environments vary along several significant dimensions
      - They can be fully or partially observable, single-agent or multiagent, deterministic or stochastic, episodic or sequential, static or dynamic, discrete or continuous, and known or unknown

    - The agent program implements the agent function
      - There exists a variety of basic agent-program designs reflecting the kind of information made explicit and used in the decision process
        - The designs vary in efficiency, compactness, and flexibility
        - The appropriate design of the agent program depends on the nature of the environment

    - Simple reflex agents respond directly to percepts, whereas model-based reflex agents maintain internal state to track aspects of the world that are not evident in the current percept
    - Goal-based agents act to achieve their goals, and utility-based agents try to maximize their own expected "happiness"
    - All agents can improve their performance through learning




## Reading 3: Solving Problems by Searching

- Problem-Solving Agents

  - Goal Formulation

    - The first step in problem solving, based on the current situation and the agent's performance measure
    - Goals help to organize behavior by limiting the objectives that the agent is trying to achieve and hence the actions it needs to consider
    - Find out how to act, now and in the future, to reach a goal state
      - Before this, it needs to decide what sorts of actions and states it should consider, given a goal => problem formulation

    - In general, an agent with several immediate options of unknown value can decide what to do by first examining future actions that eventually lead to states of known value
    - Assume the environment is observable, discrete, known, and deterministic
      - Under these conditions, the solution to any problem is a fixed sequence of actions

  - Search

    - The process of looking for a sequence of actions that reaches the goal
    - Search algorithms take in a problem as input, and returns a solution in the form of an action sequence

  - Execution

    - Once a solution is found, the actions it recommends can be carried out

    - Afterwards, the agent will formulate a new goal, repeating the "formulate, search, execute" design

      - ```pseudocode
        function SIMPLE_PROBLEM_SOLVING_AGENT(percept) returns an action
        	persistent: seq, an action sequence, initially empty
        				state, some description of the current world state
        				goal, a goal, initially null
        				problem, a problem formulation
        				
        	state <- UPDATE_STATE(state, percept)
        	if seq is empty then
        		goal <- FORMULATE_GOAL(state)
        		problem <- FORMULATE_PROBLEM(state, goal)
        		seq <- SEARCH(problem)
        		if seq = FAILURE then return a null action
        	action <- FIRST(seq)
        	seq <- REST(seq)
        	return action
        ```

    - During this phase, the agent ignores percepts when choosing actions because it knows in advance what they will be

      - Open-loop system: ignoring the percepts breaks the loop between agent and environment

  - Well-Defined Problems and Solutions

    - A problem can be defined in 5 components:
      - The initial state that the agent starts in
      - A description of the possible actions available to the agent
        - We say that the set of actions that can be executed in a particular state `s` are "applicable in `s`"

      - The transition model: a description of what each action does
        - Any state reachable from a given state by a single action is a successor to that state
        - The initial state, actions, and transition model implicitly define the state space of the problem
          - This is the set of all states reachable from the initial state by any sequence of actions
          - This forms a directed network/graph in which that nodes are states and the edges are actions
          - A path in the state space is a sequence of states connected by a sequence of actions

      - The goal test determines whether a given state is a goal state
        - Works for either an explicitly defined set of goal states or goal states defined by an abstract property

      - The path cost function assigns a numeric cost to each path
        - This cost function reflects the agent's performance measure

    - A solution to the problem is an action sequence that leads from the initial state to a goal state
      - Solution quality is measured by the path cost function
      - An optimal solution has the lowest path cost among all solutions

  - Formulating Problems

    - The process of removing detail from a representation is called abstraction
      - The process defined above is an abstraction of the state descriptions
      - We must abstract the actions themselves as well

    - An abstraction is valid if we can expand any abstract solution into a solution in the more detailed world
    - An abstraction is useful if carrying out each of the actions in the solution is easier than the original problem
      - Choice of a good abstraction thus involves removing as much detail as possible while retaining validity and ensuring that the abstract actions are easy to carry out

    - Without abstraction, intelligent agents would be completely swamped by the real world

- Searching for Solutions

  - The possible action sequences starting from the initial state form a search tree with the initial state at the root

  - General Tree Search Algorithm

    - The branches are actions and the nodes correspond to states in the state space of the problem

    - We start by testing if the initial state is the goal state

    - We then expand the current state by applying each legal action to the current state, generating a new set of states

      - This will add branches from the parent node to one child node for each new state
      - We then must choose which of the possibilities to consider further
        - Essence of search: following up one option now and putting the others aside for later, in case the first choice doesn't lead to a solution

    - These child nodes become leaf nodes, nodes with no children

      - The set of all leaf nodes available for expansion at any given point is called the frontier/open list
        - The process of expanding nodes on the frontier continues until either a solution is found or there are no more states to expand

    - ```pseudocode
      function TREE_SEARCH(problem) returns a solution, or failure
      	initialize the frontier using the initial state of problem
      	loop do
      		if the frontier is empty then return failure
      		choose a leaf node and remove it from the frontier
      		if the node contains a goal state then return the corresponding solution
      		expand the chosen node, adding the resulting nodes to the frontier
      ```

    - Search algorithms all share this basic structure

      - They vary on their search strategy: how they choose which state to expand next

  - Redundant Paths

    - Exist whenever there is more than one way to to get from one state to another

    - Loops

      - Loops can generate repeated states
      - Consideration of loops may cause complete search trees for problems to be infinite, making otherwise solvable problems unsolvable
      - There is no need to consider loops, since path costs are additive and non-negative, a loop to a given state is never better than the same path with the loop removed
      - Special case of redundant paths

    - If concerned with reaching the goal, there is never a reason to keep more than one path to a given state, as any goal state that is reachable by extending one path is reachable by extending the other

    - In some cases, redundant paths can be eliminated by redefining the problem

    - In other cases, redundant paths are unavoidable

      - Route-finding on a rectangular grid, reversible action problems, etc.

    - Redundant paths can cause a tractable problem to become intractable

    - Algorithms that forget their history are doomed to repeat it

      - Avoid exploring redundant paths by remembering where one has been

      - Augment the `TREE_SEARCH` algorithm with the explored set/closed list data structure, which remembers every expanded node

        - Newly generated nodes that match previously generated nodes can be discarded instead of added to the frontier

      - ```pseudocode
        function GRAPH_SEARCH(problem) returns a solution, or failure
        	initialize the frontier using the initial state of problem
        	initialize the explored set to be empty
        	loop do
        		if the frontier is empty then return failure
        		choose a leaf node and remove it from the frontier
        		if the node contains a goal state then return the corresponding solution
        		add the node to the explored set
        		expand the chosen node, adding the resulting nodes to the frontier
        			only if not in the frontier or explored set
        ```

        - Think of this as growing a tree directly on the state-space graph, as the tree now contains at most one copy of each state
        - The frontier separates the state-space graph into the explored region and the unexplored region
          - Every path from the initial state to an unexplored state now has to pass through the frontier

  - Infrastructure for Search Algorithms

    - For each node `n` of the tree, we have a structure that contains 4 components:

      - `n.STATE`: the state in the state space to which the node corresponds
      - `n.PARENT`: the node in the search tree that generated this node
        - Pointers string nodes together into a tree structure
        - Allow the solution to be extracted when a goal node is found

      - `n.ACTION`: the action that was applied to the parent to generate the node
      - `n.PATH_COST`: the cost, traditionally denoted by `g(n)`, of the path from the initial state to the node, as indicated by the parent pointers

    - Taking a parent node and action and returning the resultant child node:

      - ```pseudocode
        function CHILD_NODE(problem, parent, action) returns a node
        	return a node with
        		STATE = problem.RESULT(parent.STATE, action),
        		PARENT = parent, ACTION = action,
        		PATH_COST = parent.PATH_COST + problem.STEP_COST(parent.STATE, action)
        ```

    - Be careful to distinguish between nodes and states

      - A node is a bookkeeping data structure used to represent the search tree
        - On particular paths
        - Two different nodes can contain the same world state if that state is generated via two different search paths

      - A state corresponds to a configuration of the world

    - The Queue Data Structure

      - The queue stores the frontier so that the search algorithm can easily choose the next node to expand according to its preferred strategy
      - Operations:
        - `EMPTY?(queue)`: returns true only if there are no more elements in the queue
        - `POP(queue)`: removes the first element of the queue and returns it
        - `INSERT(element, queue)` inserts an element and returns the resulting queue

      - Characterized by the order in which they store the inserted nodes
        - FIFO queue: pops the oldest element of the queue
        - LIFO queue: pops the newest element of the queue
        - Priority queue: pops the element with the highest priority, according to some ordering function

    - The Explored Set

      - Can be implemented with a hash table to allow efficient checking for repeated states
        - Insertion and lookup can be done in roughly constant time, no matter how many states are stored
        - Take care to implement the hash table with the correct notion of equality for the given problem
          - Can be achieved by insisting that the data structures for states be in some canonical form
            - Logically equivalent states should map to the same data structure

  - Measuring Problem Solving Performance

    - Evaluation of an algorithm's performance in 4 ways:
      - Completeness: is the algorithm guaranteed to find a solution when there is one?
      - Optimality: does the strategy find the optimal solution?
      - Time complexity: how long does it take to find a solution?
      - Space complexity: how much memory is needed to perform the search?

    - Time and space complexity are always considered with some respect to the problem difficulty
      - The typical measure is the size of the state-space graph, `|V| + |E|`, where `V` is the set of vertices/nodes and `E` is the set of edges
        - Correct when the graph is an explicit data structure that is input to the search program

    - In AI, the graph is often represented implicitly by the initial state, actions, and transition model, and is frequently infinite
      - For this situation, we express complexity in terms of 3 quantities:
        - `b`: the branching factor (maximum number of successors of any node)
        - `d`: the depth of the shallowest goal node (the number of steps along the path from the root)
        - `m`: the maximum length of any path in the state space

      - Time measured in terms of the number of nodes generated during the search, and space in terms of the number of nodes stored in memory
      - Usually done for search on a tree
        - For a graph, the answer depends on how redundant the paths in the state space are

    - To assess the effectiveness of a search algorithm, we can use the search cost or the total cost
      - Search cost: depends on the time complexity but can also include a term for memory usage
      - Total cost: combines the search cost and the path cost of the found solution
        - Enables the agent to find an optimal tradeoff point at which further computation to find a shorter path becomes counterintuitive

- Uninformed Search Strategies

  - This section contains several search strategies that are considered uninformed/blind searches

    - These strategies have no additional information about states beyond that provided in the problem definition
    - They can only generate successors and distinguish a goal state from a non-goal state

  - Breadth-First Search

    - Strategy where the root node is expanded first, then all the successors of the root node are expanded next, then their successors, and so on

      - In general, all the nodes are expanded at a given depth in the search tree before any nodes at the next level are expanded

    - The shallowest node unexpanded node is selected for expansion

      - Done simply using a FIFO queue for the frontier
      - The goal test is applied to each node upon its generation
      - Always generates the shallowest path to each node by discarding paths to states already in the frontier or explored set

    - ```pseudocode
      function BREADTH_FIRST_SEARCH(problem) returns a solution, or failure
      	node <- a node with STATE = problem.INITIAL_STATE, PATH_COST = 0
      	if problem.GOAL_TEST(node.STATE) then return SOLUTION(node)
      	frontier <- a FIFO queue with node as the only element
      	explored <- an empty set
      	loop do
      		if EMPTY?(frontier) then return failure
      		node <- POP(frontier) // Chooses the shallowest node in frontier
      		add node.STATE to explored
      		for each action in problem.ACTIONS(node.STATE) do
      			child <- CHILD_NODE(problem, node, action)
      			if child.STATE is not in explored or frontier then
      				if problem.GOAL_TEST(child.STATE) then return SOLUTION(child)
      				frontier <- INSERT(child, frontier)
      ```

    - The 4 criteria:

      - We can see BFS is complete, as the shallowest goal node is at some finite depth `d`, therefore BFS will eventually find it after generating all shallower nodes (provided `b` is finite)

        - Note that when a goal node is generated, we know it's the shallowest (not most optimal) goal node since all shallower nodes have been generated and failed the goal test

      - Optimal when the path cost is a nondecreasing function of the depth of the node

        - Most common when all actions have the same cost

      - For time complexity, the algorithm searches a tree where each node has `b` successors:

        - $$
          b+b^2+b^3+...+b^d=O(b^d)
          $$

      - For the space complexity, we note that we store all nodes in the explored set and the frontier, leaving us with `O(b^d)`

    - Memory requirements are a bigger problem for BFS than the execution time

      - Time is still a major factor

    - In general, exponential-complexity search problems cannot be solved by uninformed methods for any but the smallest instances

  - Uniform-Cost Search

    - Instead of expanding the shallowest node, uniform-cost search expands the node `n` with the lowest path cost `g(n)`

      - Stores the frontier as a priority queue ordered by `g`

    - ```pseudocode
      function UNIFORM_COST_SEARCH(problem) returns a solution, or failure
      	node <- a node with STATE = problem.INITIAL_STATE, PATH_COST = 0
      	frontier <- a priority queue ordered by PATH_COST, with node as the only element
      	explored <- an empty set
      	loop do
      		if EMPTY?(frontier) then return failure
      		node <- POP(frontier) // Chooses the lowest-cost node in frontier
      		if problem.GOAL_TEST(node.STATE) then return SOLUTION(node)
      		add node.STATE to explored
      		for each action in problem.ACTIONS(node.STATE) do
      			child <- CHILD_NODE(problem, node, action)
      			if child.STATE is not in explored or frontier then
      				frontier <- INSERT(child, frontier)
      			else if child.STATE is in frontier with higher PATH_COST then
             replace that frontier node with child
      ```

    - 2 other significant differences from BFS

      - Goal test is applied to a node when it is selected for expansion rather than when it is first generated
        - Reason is that the first goal node generated may be on a suboptimal path

      - A test is added in case a batter path is found to a node currently on the frontier

    - Easy to see optimality

      - Observe that whenever uniform-cost search selects a node `n` for expansion, the optimal path to that node has been found
        - Were this not the case, there would have to be another frontier node `n'` on the optimal path from the start node to `n`
        - By definition, `n'` would have lower `g`-cost than `n` and would have been selected first

      - Since step costs are non-negative, paths never get shorter as nodes are added
      - These together imply that uniform-cost search expands nodes in order of their optimal path cost
        - Hence the first goal node selected for expansion must be the optimal solution

    - Doesn't care about the number of steps a path has, but only about their total costs

      - Can get stuck in an infinite loop if there is a path with an infinite sequence of zero-cost actions
      - Completeness is guaranteed provided the cost of every step exceeds some small positive constant `ϵ`

    - Complexity is not easily characterized in terms of `b` and `d`

      - Let `C*` be the cost of the optimal solution and assume every action costs at least `ϵ`

        - $$
          O(b^{1+\lfloor C^{{\star}/\varepsilon}\rfloor})
          $$

          - May be worse than BFS, as uniform-cost search may explore large trees of small steps before exploring paths involving large and perhaps useful steps

      - If all step costs are the same, the time complexity reduces down to `O(b^d+1)`

        - Like BFS, except uniform-cost search examines all the nodes at the goal's depth to see if one has a lower cost
          - Does strictly more work

  - Depth-First Search

    - Always expands the deepest node in the current frontier of the search tree
      - As these nodes are expanded, they are dropped from the frontier, so then the search "backs up" to the next deepest node that still has unexplored successors
      - Uses a LIFO queue, choosing the most recently generated node for expansion
        - Must be the deepest node because it is one deeper than its parent, which was the deepest unexpanded node when it was selected
    - Properties are dependent on the variant of DFS used
      - Graph-search avoids repeated states and redundant paths
        - Is complete because it will eventually expand every node
      - Tree-search is not complete
        - Victim to redundant paths

      - Both variants are incomplete in infinite state spaces
      - DFS is not optimal
      - Graph search time complexity bounded by the size of the state space
      - Tree search time complexity can generate all of the `O(b^m)` nodes of the search tree, where `m` is the maximum depth of any node
        - Can be much greater than the size of the state space

      - Space complexity is where DFS shines
        - Once a node has been expanded, it can be removed from memory once all of its descendants have been fully explored
        - For a state space with branching factor `b` and maximum depth `d`, DFS requires storage of only `O(bm)` nodes
        - Leads to adoption of DFS as the basic workhorse of many areas of AI
          - Constraint satisfaction, propositional satisfiability, logic programming, etc.
    - Variant of DFS called backtracking search uses even less memory
      - Only one successor is generated at a time rather than all successors
        - Each partially expanded node remembers which successor to generate next
        - Requires only `O(m)` memory
      - Uses idea of modifying the current state description rather than copying it first
        - Reduces memory requirements to just one state description and `O(m)` actions
        - We must be able to undo each modification when we go back to generate the next successor

  - Depth-Limited Search

    - Works to supply DFS with a predetermined depth limit `l` to alleviate failures in infinite state spaces

      - Nodes at depth `l` are treated as if they have no successors
      - Solves the infinite-path problem
      - Can still be incomplete if `l < d`, or if the selected depth limit is shallower than the shallowest goal
        - Is also nonoptimal if `l > d`, as time complexity is `O(b^l)` and space complexity is `O(bl)`
      - DFS can be viewed as a special case of depth-limited search where `l = ∞`

    - Depth limits can be based on the knowledge of the problem

      - For example, using the diameter of the state space can lead to a more efficient depth-limited search
      - For most problems, we will not know a good depth limit until we have solved the problem

    - ```pseudocode
      function DEPTH_LIMITED_SEARCH(problem, limit) returns a solution, or failure/cutoff
      	return RECURSIVE_DLS(MAKE_NODE(problem.INITIAL_STATE), problem, limit)
      	
      function RECURSIVE_DLS(node, problem, limit) returns a solution or failure/cutoff
      	if problem.GOAL_TEST(node.STATE) then return SOLUTION(node)
      	else if limit = 0 then return cutoff
      	else
      		cutoff_occurred? <- false
      		for each action in problem.ACTIONS(node.STATE) do
      			child <- CHILD_NODE(problem, node, action)
      			result <- RECURSIVE_DLS(child, problem, limit - 1)
      			if result = cutoff then cutoff_occurred? <- true
      			else if result != failure then return result
      		if cutoff_occurred? then return cutoff else return failure
      ```
      
      - Notice this can return 2 different failure conditions:
        - `failure` indicates no solution
        - `cutoff` indicates no solution within the depth limit
    
  - Iterative Deepening Depth-First Search

    - A general strategy, often used in combination with DFS, that finds the best depth limit

      - Gradually increases the limit until a goal is found, which occurs when the depth limit reachs `d`, the depth of the shallowest goal node

    - Combines the benefits of DFS and BFS

      - Modest memory requirements: `O(bd)`
      - Complete when the branching factor is finite and optimal when the path cost is a nondecreasing function of the depth of the node

    - ```pseudocode
      function ITERATIVE_DEEPENING_SEARCH(problem) returns a solution, or failure
      	for depth = 0 to INFINITY do
      		result <- DEPTH_LIMITED_SEARCH(problem, depth)
      		if result != cutoff then return result
      ```

    - Seems wasteful due to repeated generation of states

      - Ends up being not too costly, as most of the nodes are in the bottom level, so it doesn't matter that the upper nodes are generated multiple times

    - In general, this is the preferred uninformed search method when the search space is large and the depth of the solution is not known

    - Explores a complete layer of new nodes at each iteration before going on to the next layer, like BFS

  - Bidirectional Search

    - Run 2 simultaneous searches, one forward from the initial state and one backward from the goal
      - Hope the searches meet in themiddle
    - Replaces the goal test with a check to see whether the frontiers of the two searches interact
      - The first solution found may not be optimal, even if the searches are both breadth-first
      - Check done when each node is generated/selected with a hash table
    - Time/space complexity is `O(b^d/2)` if both are done with BFS
      - Can reduce space by changing one search to iterative deepening
        - One BFS is required so that we can check for overlapping frontiers
    - Difficult part is how to search backwards
      - Requires a method for computing predecessors, all the states that have a given state as a successor
        - Easy when all actions are reversible, difficult otherwise

  - Comparing Uninformed Search Strategies

    - | Criterion |   BFS    |  Uniform  |   DFS    |    DL    |    ID    |     Bi     |
      | --------- | :------: | :-------: | :------: | :------: | :------: | :--------: |
      | Complete? |    Y     |     Y     |    N     |    N     |    Y     |     Y      |
      | Time      | `O(b^d)` | See above | `O(b^m)` | `O(b^l)` | `O(b^d)` | `O(b^d/2)` |
      | Space     | `O(b^d)` | See above | `O(b^m)` | `O(bl)`  | `O(bd)`  | `O(b^d/2)` |
      | Optimal   |    Y     |     Y     |    N     |    N     |    Y     |     Y      |

- Informed (Heuristic) Search Strategies

  - An informed search strategy is one that uses problem-specific knowledge beyond the problem definition

    - Can find more efficient solutions than uninformed strategies

  - General approach: best-first search

    - Variant of `TREE_SEARCH`/`GRAPH_SEARCH` in which nodes are selected for expansion based on an evaluation function `f(n)`
    - `f(n)` is a cost estimate, so the node with the lowest evaluation is expanded first
    - Implementation is identical to uniform-cost search, but with `f(n)` instead of `g(n)` dictating the priority queue
    - Most best-first search algorithms include as a component of `f` a heuristic function, denoted `h(n)`
      - `h(n)` is the estimated cost of the the cheapest path from the state at node `n` to a goal state
      - Note that this function takes in a node as input, but the result only depends on the state that the node represents
      - Most common form in which additional knowledge of the problem is imparted to the search algorithm
      - Consider them to be arbitrary, nonnegative, problem-specific functions, where, if `n` is a goal state, then `h(n) = 0`

  - Greedy Best-First Search

    - Tries to expand the node that is closest to the goal, on the grounds that this is likely to lead to a solution quickly
      - i.e., `f(n) = h(n)`

    - Incomplete, even in finite spaces
      - Problem similar to that of DFS

    - Time and space complexity of `O(b^m)`
      - Can be reduced substantially with a good heuristic function

  - A* Search: Minimizing the Total Estimated Solution Cost

    - Evaluates nodes by combining `g(n)`, the cost to reach the node, and `h(n)`, the cost to get from the node to the goal

      - $$
        f(n)=g(n)+h(n)
        $$

      - i.e., `f(n)` is the estimated cost of the cheapest solution through `n`

    - Provided that `h(n)` satisfies certain conditions, A* search is both complete and optimal

      - `h(n)` must be an admissible heuristic

        - A heuristic that never overestimates the cost to reach the goal
          - This tells us that `f(n)` also never overestimates the true cost of a solution along the current path through `n`

        - Optimistic by nature, because that think the cost of solving a problem is less than it is
          - Ex) straight-line distance heuristic

      - `h(n)` must satisfy consistency/monotonicity

        - Only required for applications of A* to graph search

        - If for every node `n` and every successor `n'` of `n` generated by any action `a`, the estimated cost of reaching the goal from `n` is no greater than the step cost of getting to `n'` plus the estimated cost of reaching the goal from `n'`

          - $$
            h(n)\le c(n,a,n')+h(n')
            $$

          - Form of the general triangle inequality, which stipulates that each side of a triangle cannot be longer than the sum of the other two sides

            - Triangle here formed by `n`, `n'`, and the goal `G_n` closest to `n`

        - Every consistent heuristic is also admissible

    - Optimality of A*

      - As mentioned above, the tree-search version of A* is optimal if `h(n)` is admissible, while the graph-search version is optimal if `h(n)` is consistent

      - First step is to establish that if `h(n)` is consistent, then the values of `f(n)` along any path are nondecreasing

        - $$
          f(n')=g(n')+h(n')=g(n)+c(n,a,n')+h(n')\ge g(n)+h(n)=f(n)
          $$

      - Next, prove that wherever A* selects a node `n` for expansion, the optimal path to that node has been found

        - If this were not the case, there would have to be another frontier node `n'` on the optimal path from the start node to `n`
        - Since `f` is nondecreasing along any path, `n'` would have lower `f`-cost than `n` and would have been selected first

      - From these observations, it follows that the sequence of nodes expanded by A* using `GRAPH_SEARCH` is in nondecreasing order of `f(n)`

        - Therefore, the first goal node selected for expansion must be an optimal solution because `f` is the true cost for goal nodes (which have `h = 0`) and all later goal nodes will be at least as expensive
        - This allows us to draw contours in the state space, where everything within a given contour has a cost less than or equal to the contour value
          - Shows us that A* search fans out from the start node, adding nodes in concentric bands of increasing `f`-cost
          - Unlike uniform-cost search, these contours are not circular, and, with accurate heuristics, they will stretch towards the goal state and become more narrowly focused around the optimal path
          - Assuming `C*` is the cost of the optimal solution path, then we can say the following:
            - A* expands all nodes with `f(n) < C*`
            - A* might then expand some of the nodes right on the "goal contour" (where `f(n) = C*`) before selecting a goal node

    - Completeness requires that there be only finitely many nodes with cost less than or equal to `C*`, a condition that is true if all step costs exceed some finite `ϵ` and if `b` is finite

    - Notice that A* expands no nodes with `f(n) > C*`

      - We say the subtree below this point is pruned
      - These nodes can be safely eliminated while guaranteeing optimality due to the admissible nature of the heuristic
        - Important for many areas of AI to eliminate possibilities without having to consider them

    - Among optimal algorithms that extend search paths from the root and use the same heuristic information, A* is optimally efficient for any given consistent heuristic

      - No other optimal algorithm is guaranteed to expand fewer nodes than A*
      - Any algorithm that doesn't expand all nodes with `f(n) < C*` risks missing the optimal solution entirely

    - Catch is that most problems have a number of states within the goal contour that grows exponentially with the length of the solution

      - For problems with constant step costs, the growth in run time as a function of the optimal solution depth `d` is analyzed in terms of the error of the heuristic

        - Absolute error:

          - $$
            \Delta\equiv h^*-h
            $$

        - Relative error:

          - $$
            \epsilon\equiv\frac{h^*-h}{h^*}
            $$

    - Complexity results depend heavily on the assumptions made about the state space

      - Simplest model is a state space that has a single goal and is essentially a tree with reversible actions
        - Time complexity is `O(b^Δ)`, or `O(b^ϵd)` for constant step costs

      - When the state space has many goal states, particularly ones that are near-optimal, the search process can be led astray from the optimal path
        - There is an extra cost proportional to the number of goals whose cost is within a factor `ϵ` of the optimal cost

      - Often makes it impractical to insist on finding an optimal solution
        - Variants of the algorithm find suboptimal solutions quickly
        - Use of heuristics that are more accurate, but not strictly admissible

      - Usually runs out of space long before it runs out of time
        - Must keep all generated nodes in memory
        - Not practical for many large-scale problems

  - Memory-Bounded Heuristic Search

    - Simplest way to reduce memory constraints on A* is to adapt the idea of iterative deepening to the heuristic search context

      - Results in IDA*, or iterative-deepening A\*
      - Difference between IDA* and iterative deepening is that the cutoff used is the `f`-cost (`g + h`) rather than the depth
      - At each iteration, the cutoff value is the smallest `f`-cost of any node that exceeded the cutoff on the previous iteration

    - Recursive Best-First Search (RBFS)

      - Recursive algorithm that attempts to mimic the operation of standard best-first search, but using only linear space

      - ```pseudocode
        function RECURSIVE_BEST_FIRST_SEARCH(problem) returns a solution, or failure
        	return RBFS(problem, MAKE_NODE(problem.INITIAL_STATE), INF)
        	
        function RBFS(problem, node, f_limit) returns a solution, or failure and a new f-cost limit
        	if problem.GOAL_TEST(node.STATE) then return SOLUTION(node)
        	successors <- []
        	for each eaction in problem.ACTIONS(node.STATE) do
        		add CHILD_NODE(problem, node, action) into successors
        	if successors is empty then return failure, INF
        	for each s in successors do /* update f with value from previous search */
        		s.f <- max(s.g + s.h, node.f)
            loop do
            	best <- the lowest f-value node in successors
            	if best.f > f_limit then return failure, best.f
            	alternative <- the second lowest f-value among successors
            	result, best.f <- RBFS(problem, best, min(f_limit, alternative))
            	if result != failure then return result
        ```

        - Uses `f_limit` to keep track of the `f`-value of the best alternative path available from any ancestor of the current node
        - Rewinds back to the alternate path if the current node exceeds this limit
        - RBFS replaces the `f`-value of each node along the path with a backed-up value
          - The best `f`-value of its children
          - Remembers the best `f`-value leaf of the forgotten subtree, allowing it to decide if its worth re-expanding the subtree later

      - Somewhat more efficient than IDA*, but still suffers from excessive node regeneration

        - Happens because, when the current best path is extended, the `f`-value is likely to increase, as `h` is usually less optimistic for nodes closer to the goal
        - Results in need to backtrack to follow the second-best path

      - Optimal if the heuristic function `h(n)` is admissible

      - Space complexity is linear in the depth of the deepest optimal solution

        - Suffers from using too little memory, resulting in re-expansions of states

      - Time complexity depends on the accuracy of the heuristic function and how often the best path changes as nodes are expanded

    - MA* and SMA*

      - Memory-bounded A* and simplified MA*
      - SMA* proceeds like A* until the memory is full
        - To proceed, SMA* drops the leaf node with the highest `f`-value, backing up the value of the forgotten node to its parent
          - This allows the ancestor of a forgotten subtree to remember the quality of the best path in that subtree
          - If all the descendants of a node `n` are forgotten, then we don't know where to go from `n`, but we still have an idea of how worthwhile it is to go anywhere from `n`

        - SMA* will regenerate this forgotten path only if all other paths have been shown to be worse than it
        - In case of a tie of `f`-values, SMA* expands the newest best leaf and deletes the oldest worst leaf
        - Complete if there is a reachable solution (`d` is less than the memory size, expressed in nodes)
        - Optimal if the optimal solution is reachable
          - Otherwise returns the best reachable solution

        - Robust choice for finding optimal solutions, particularly when the state space is a graph, step costs are not uniform, and node generation is expensive compared to the overhead of maintaining the frontier and the explored set
        - On hard problems, likely that SMA* is forced to switch back and forth among many candidate solution paths
          - Resembles thrashing in disk paging systems
          - Extra repeated regeneration of the same nodes means that problems that would be practically solvable by A* become intractable for SMA*
            - Memory limitations can make a problem intractable from the point of view of computation time

  - Learning to Search Better

    - Could an agent learn how to search better?
      - Yes, involving the metalevel state space

    - Each state in a metalevel state space captures the internal state of a program that is searching in an object-level state space
      - Each action in the metalevel state space is a computation step that alters the internal state
      - A metalevel learning algorithm can learn from past experiences to avoid exploring unpromising subtrees
      - Goal of learning is to minimize the total cost of problem solving, trading off computational expense and path cost

- Heuristic Functions

  - The Effect of Heuristic Accuracy on Performance

    - One way to characterize the quality of a heuristic is the effective branching factor `b*`

      - If the total number of nodes generated by A* for a particular problem is `N` and the solution depth is `d`, then `b*` is the branching factor that a uniform tree of depth `d` would have to have in order to contain `N + 1` nodes

      - $$
        N+1=1+b^*+(b^*)^2+...+(b^*)^d
        $$

      - The effective branching factor can vary across problem instances, but usually is fairly consistent for sufficiently hard problems

      - Experimental measurements of `b*` on a small set of problems can provide a good guide to the heuristic's overall usefulness

        - Well-designed heuristics would have a value of `b*` close to `1`, allowing fairly large problems to be solved at a reasonable computational cost

    - We say that `h_2` dominates `h_1` if for any node `n`:

      - $$
        h_2(n)\ge h_1(n)
        $$

      - Domination translates directly into efficiency: it is better to use a heuristic function with higher values, provided it is consistent and that the computation time for the heuristic is not too long

  - Generating Admissible Heuristics from Relaxed Problems

    - A problem with fewer restrictions on the actions is called a relaxed problem
      - The state-space graph of the relaxed problem is a supergraph of the original state space because the removal of restrictions creates added edges in the graph
    - Since the relaxed problem adds edges to the state space, any optimal solution in the original problem is also a solution in the relaxed problem
      - The relaxed problem may have better solutions if the added edges provide shortcuts
      - The cost of an optimal solution to a relaxed problem is an admissible heuristic for the original problem
        - Since the derived heuristic is an exact cost for the relaxed problem, it must obey the triangle inequality and is therefore consistent
    - One problem with generating new heuristic functions is that one often fails to get a single "clearly best" heuristic
      - Can simply take the maximum all heuristics at each node, using the heuristic most accurate on the node in question

  - Generating Admissible Heuristics from Subproblems: Pattern Databases

    - Admissible heuristics can also be derived from the solution cost of a subproblem of a given problem
      - The cost of the optimal solution of a subproblem is a lower bound on the cost of the complete problem
    - Idea behind pattern databases is to store these exact solution costs for every possible subproblem instance
      - We then compute an admissible heuristic `h_DB` for each complete state encountered during a search simply by looking up the corresponding subproblem configuration in the database
      - Database is created by searching back from the goal and recording the cost of each new pattern encountered
        - Expense amortized over many subsequent problem instances
    - Idea behind disjoint pattern databases is to sum up the costs of non-overlapping problems and develop a heuristic accordingly

  - Learning Heuristics from Experience

    - How can an agent construct a heuristic function?
      - One solution is to devise relaxed problems for which an optimal solution can be found easily
      - Another solution is to learn from experience (many problem instances)
        - Each optimal solution provides examples from which `h(n)` can be learned
        - Use of neural nets, decision trees, etc.
        - Work best when supplied with features of a state that are relevant to predicting the state's value, rather than just the raw state description
          - Linear combination of these factors to develop the heuristic

- Summary

  - Before an agent can start searching for solutions, a goal, must be identified and a well-defined problem must be formulated
  - A problem consists of 5 parts: the initial state, a set of actions, a transition model describing the results of those actions, a goal test function, and a path cost function
    - The environment of the problem is represented by a state space
    - A path through the state space from the initial state to a goal state is a solution
  - Search algorithms treat states and actions as atomic: they do not consider any internal structure they might possess
  - A general `TREE_SEARCH` algorithm considers all possible paths to find a solution, whereas a `GRAPH_SEARCH` algorithm avoids consideration of redundant paths
  - Search algorithms are judged on the basis of completeness, optimality, time complexity, and space complexity
    - Complexity depends on `b`, the branching factor in the state space, and `d`, the depth of the shallowest solution
  - Uninformed search methods have access only to the problem definition
    - BFS expands the shallowest nodes first; it is complete, optimal for unit step costs, but has exponential space complexity
    - Uniform-cost search expands the node with lowest path cost, `g(n)`, and is optimal for general step costs
    - DFS expands the deepest unexpanded node first; it is neither complete nor optimal, but has linear space complexity
      - Depth-limited search adds a depth bound
    - Iterative deepening search calls DFS with increasing depth limits until a goal is found; it is complete, optimal for unit step costs, has time complexity comparable to BFS, and has linear space complexity
    - Bidirectional search can enormously reduce time complexity, but it is not always applicable and may require too much space
  - Informed search methods may have access to a heuristic function `h(n)` that estimates the cost of a solution from `n`
    - The generic best-first search algorithm selects a node for expansion according to an evaluation function
    - Greedy best-first search expands nodes with minimal `h(n)`; it is not optimal, but often efficient
    - A* search expands nodes with minimal `f(n) = g(n) + h(n)`; it is complete and optimal, provided that `h(n)` is admissible (for `TREE_SEARCH`) or consistent (for `GRAPH_SEARCH`)
      - The space complexity is still prohibitive
    - RBFS and SMA* are robust, optimal search algorithms that use limited amounts of memory; given enough time, they can solve problems that A* cannot solve because it runs out of memory
  - The performance of heuristic search algorithms depends on the quality of the heuristic function
    - One can sometimes construct good heuristics by relaxing the problem definition, by storing precomputed solution costs for subproblems in a pattern database, or by learning from experience with the problem class



## Reading 4: Beyond Classical Search

- Local Search Algorithms and Optimization Problems

  - In many problems, the path to the goal doesn't matter

    - In these cases, we consider a different class of algorithms that don't worry about paths

  - Local search algorithms operate using a single current node (rather than multiple paths)

    - Generally only move to neighbors of that node
    - Typically, the paths followed by the search are not retained
    - 2 key advantages:
      - Use very little memory, usually a constant amount
      - Can often find reasonable solutions in large or infinite (continuous) state space for which systematic algorithms are unsuitable
    - Useful for solving pure optimization problems
      - Aim is to find the best state according to an objective function

  - State-space landscape

    - Landscape has both "location" (defined by the state) and "elevation" (defined by the value of the heuristic cost function or objective function)
      - If elevation corresponds to cost, then the aim is to find the global minimum
      - If elevation corresponds to an objective function, then the aim is to find the global maximum
    - Local search algorithms explore this landscape
      - Complete local search algorithms always find a goal if one exists
      - Optimal local search algorithms always find a global minimum/maximum

  - Hill-Climbing Search

    - ```pseudocode
      function HILL_CLIMBING(problem) returns a state that is a local maximum
      	current <- MAKE_NODE(problem.INITIAL_STATE)
      	loop do
      		neighbor <- a highest-valued successor of current
      		if neighbor.VALUE <= current.VALUE then return current.STATE
      		current <- neighbor
      ```

      - Loop that continually moves in the direction of increasing value
      - Terminates when it reaches a peak where no neighbor has a higher value

    - Algorithm doesn't maintain a search tree or look beyond the immediate neighbors of the current state

    - Sometimes called greedy local search because it grabs a good neighbor state without thinking ahead about where to go next

      - Often makes rapid progress towards a solution because its usually easy to improve a bad state

    - Often gets stuck due to the following reasons:

      - Local maxima cause the algorithm to get stuck with nowhere to go
      - Ridges result in sequences of local maxima that are hard for the algorithm to navigate
      - Plateaus, which can be flat local maxima or a shoulder, might cause the algorithm to get lost
        - If sideways moves are allowed, flat local maxima will generate infinite loops

    - Various forms of this algorithm have been invented

      - Stochastic hill climbing chooses at random from among uphill moves, with the probability of selecting varying with the steepness of the uphill move
        - Converges slower, but may find better solutions
      - First-choice hill climbing generates successors randomly to implement stochastic hill climbing
        - Good strategy when a state has many successors
      - Random-restart hill climbing conducts searches from randomly generated initial states until a goal is found
        - Trivially complete
        - Expected number of restarts is `1/p`, where `p` is the probability of success

    - Success depends on the shape of the state-space landscape

      - Few local maxima and plateaus are conducive to success

  - Simulated Annealing

    - Hill-climbing algorithms that never make downhill moves are guaranteed to be incomplete, as they get stuck on local maxima

    - Random selection is complete, but inefficient

    - Simulated annealing combines randomness and hill climbing to attempt to yield efficiency and completeness

      - Shake hard and gradually reduce the intensity of the shaking

    - ```pseudocode
      function SIMULATED_ANNEALING(problem, schedule) returns a solution state
      	inputs: problem, a problem
      	        schedule, a mapping from time to "temperature"
        current <- MAKE_NODE(problem.INITIAL_STATE)
        for t = 1 to INF do
        	T <- schedule(t)
        	if T = 0 then return current
        	next <- a randomly selected successor of current
        	ΔE <- next.VALUE - current.VALUE
        	if ΔE > 0 then current <- next
        	else current <- next only with probability e^(ΔE/T)
      ```

  - Local Beam Search

    - Keeps track of `k` state rather than just one
      - Begins with `k` randomly generated states
      - All successors of `k` states are generated
        - If a goal is found, the algorithm halts
      - Select the `k` best successors from the complete list and repeat
    - Passes useful information among the parallel search threads
      - Allows the algorithm to quickly abandon unfruitful searches and move to where the most progress is being made
    - Can suffer from a lack of diversity in `k` states
      - Remedied by stochastic beam search, which picks successors at random, with probabilities weighted by the successors' values

  - Genetic Algorithms

    - A variant of stochastic beam search in which successor states are generated by combining 2 parent states rather than by modifying a single value

    - Begins with a set of `k` randomly generated states called the population

      - Each state/individual is represented as a string over a finite object
      - Each state is rated by the objective/fitness function, which returns higher values for better states
      - Pairs selected at random for reproduction
        - Crossover point selected from positions in the string
      - Children created from crossovers
      - Children subject to random mutations of varying probabilities

    - Combination of uphill tendencies with random exploration and exchange of information

    - Crossover combines large blocks of letters that have evolved independently to perform useful function, raising the level of granularity at which the search operates

      - Operates with schema and instances of schema

    - ```pseudocode
      function GENETIC_ALGORITHM(population, FITNESS_FN) returns an individual
      	inputs: population, a set of individuals
      	FITNESS_FN, a function that measures the fitness of an individual
      	
      	repeat
      		new_population <- empty set
      		for i = 1 to SIZE(population) do
      			x <- RANDOM_SELECTION(population, FITNESS_FN)
      			y <- RANDOM_SELECTION(population, FITNESS_FN)
      			child <- REPRODUCE(x, y)
      			if (small random probability) then child <- MUTATE(child)
      			add child to new_population
      		population <- new_population
        until some individual is fit enough, or enough time has elapsed
        return the best individual in population, according to FITNESS_FN
        
      function REPRODUCE(x, y) returns an individual
      	inputs: x, y, parent individuals
      	
      	n <- LENGTH(x); c <- some random number from 1 to n
      	return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c + 1, n))
      ```

- Local Search in Continuous Spaces

  - One way to avoid continuous problems is the discretize the neighborhood of each state

  - Many methods attempt to use the gradient of the landscape to find a maximum

    - Given a locally correct expression for the gradient, we can perform steepest-ascent hill climbing by updating the current state using:

      - $$
        x\leftarrow x+\alpha\nabla f(x)
        $$

      - `α` is a small constant called the step size

        - If it's too small, too many steps are needed 
        - If it's too large, the search may overshoot the maximum
        - Line search extends the current gradient direction until `f` starts to decrease again

  - If the objective function is not in a differentiable form, we can use an empirical gradient, which results from a response to small increments and decrements in each coordinate

  - For many problems, the most effective is the Newton-Raphson method, used to solve equations of the form `g(x) = 0`

    - Computes a new estimate for the root `x` according to Newton's formula:

      - $$
        x\leftarrow x-g(x)/g'(x)
        $$

    - To find a maximum or minimum of `f`, we need to find `x` such that the gradient is `0`

      - $$
        x\leftarrow x-H_f^{-1}(x)\nabla f(x)
        $$

        - `Hf(x)` is the Hessian matrix of second derivatives

          - $$
            H_{ij}=\partial^2f/\partial x_i\partial x_j
            $$

  - Methods still suffer from local maxima, ridges, and plataeus

    - Random restarts and simulated annealing can be used and are often helpful

  - Constrained optimization is an optimization problem where solutions must satisfy some hard constraints on the values of the variables

    - Linear programming problems have constraints that must be linear inequalities forming a convex set



## Reading 5: Constraint Satisfaction Problems

- Defining Constraint Satisfaction Problems

  - A constraint satisfaction problem consists of three components, `X`, `D`, and `C`
    - `X` is a set of variables, `{X_1, ..., X_n}`
    - `D` is a set of domains, `{D_1, ..., D_n}`, one for each variable
      - Each `D_i` consists of a set of allowable values, `{v_1, ..., v_k}` for variable `X_i`
    - `C` is a set of constraints that specify allowable combinations of values
      - Each constraint `C_i` consists of a pair `<scope, rel>`
        - `scope` is a tuple of variables that participate in the constraint
        - `rel` is a relation that defines the values that those variables can take on
          - Can be represented as an explicit list of all tuples of values that satisfy the constraint, or as an abstract relation that supports two operations:
            - Testing if the tuple is a member of the relation
            - Enumerating the members of the relation
  - To solve a CSP, we need to define a state space and the notion of a solution
    - Each state in a CSP is defined by an assignment of values to some or all of the variables
      - An assignment that doesn't violate any constraints is called a consistent/legal assignment
      - An assignment in which all of the variables are assigned is called a complete assignment
      - An assignment in which only some of the variables are assigned is called a partial assignment
    - A solution is a complete, consistent assignment
  - CSPs can be visualized as a constraint graph
    - The nodes of the graph correspond to variables of the problem
    - A link connects any two variables that participate in a constraint
  - Why formulate a problem as a CSP?
    - CSPs yield a natural representation for a wide variety of problems
      - If you already have a CSP-solving system, it is often easier to solve a problem using it than to design a custom solution using another search technique
    - CSP solvers can be faster than state-space searchers because the CSP solver can quickly eliminate large swatches of the search space
      - Regular state-space search can only ask if specific states are goals
      - CSPs can use the information that a partial assignment is not a solution to discard further refinements of the partial assignment
      - CSPs can see why certain assignments aren't solutions, allowing us to focus on the variables that break constraints
  - Variations on the CSP Formalism
    - Simplest kinds of CSPs involve variables that have discrete, finite domains
    - Infinite Domains
      - Discrete domains can be infinite
        - Ex) set of integers, set of strings, etc.
      - No longer possible to describe constraints by enumerating all allowed combinations of values
        - Must now use a constraint language that understands the constraints without having to enumerate the set of pairs of allowable values
        - Special solution algorithms exist for linear constraints on integer variables
          - It can be shown that no algorithm exists for solving general nonlinear constraints on integer variables
    - Continuous Domains
      - Best known category is that of linear programming problems
        - Constraints must be linear equalities or inequalities
        - Can be solved in time polynomial in the number of variables
    - Types of Constraints
      - Unary constraints restrict the value of a single variable
      - Binary constraints relate two variables
        - A binary CSP is one with only binary CSPs, and can be represented as a constraint graph
      - Higher-order constraints relate 3+ variables
      - Global constraints involve an arbitrary number of variables
        - Can be represented in a constraint hypergraph, consisting of ordinary nodes and hypernodes, which represent `n`-ary constraints
    - Every finite-domain constraint can be reduced to a set of binary constraints if enough auxiliary variables are introduced
      - This means we can transform any CSP into one with only binary constraints
      - Can be done through the dual graph transformation
        - Create a new graph in which there will be one variable for each constraint in the original graph
        - Create one binary constraint for each pair of constraints in the original graph that share variables
      - We may prefer global constraints to binary constraints for two reasons:
        - Easier and less error-prone to write the problem description in some instances
        - Possible to design special-purpose inference algorithms for global constraints that are not available for a set of more primitive constraints
    - Preference Constraints
      - Included in many real-world CSPs
      - Indicate which solutions are preferred
      - CSPs with preferences can be solved with optimization search methods
        - Called constraint optimization problems, or COPs

- Constraint Propagation: Inference in CSPs

  - In CSPs, algorithms have a choice:

    - To search by choosing a new variable assignment from several possibilities
    - To do a specific type of inference called constraint propagation
      - Uses the constraints to reduce the number of legal values for a variable, which can in turn reduce the number of legal values for another variable and so on
    - These steps can be intertwined, or, constraint propagation may be used as a preprocessing step for the the search

  - Key idea is local consistency

    - If each variable is treated as a node and each constraint as an arc, the process of enforcing local consistency in each part of the graph causes inconsistent values to be eliminated throughout the graph

  - Node Consistency

    - A single variable is node-consistent if all the values in the variable's domain satisfy the variable's unary constraints
      - A network is node-consistent if every variable in the network is node-consistent

  - Arc Consistency

    - A variable in a CSP is arc-consistent if every value in its domain satisfies the variable's binary constraints

      - `X_i` is arc-consistent with respect to another variable `X_j` if for every value in the current domain `D_i` there is some value in the domain `D_j` that satisfies the binary constraint on the arc `(X_i, X_j)`
      - A network is arc-consistent if every variable is arc-consistent with every other variable

    - The most popular algorithm for applying arc consistency is AC-3

      - ```pseudocode
        function AC-3(csp) returns false if an inconsistency is found and true otherwise
        	inputs: csp, a binary CSP with components (X, D, C)
        	local variables: queue, a queue of arcs, initially all the arcs in csp
        	
        	while queue is not empty do
        		(X_i, X_j) <- REMOVE_FIRST(queue)
        		if REVISE(csp, X_i, X_j) then
        			if size of D_i = 0 then return false
        			for each X_k in X_i.NEIGHBORS - {X_j} do
        				add (X_k, X_i) to queue
            return true
            
        function REVISE(csp, X_i, X_j) returns true iff we revise the domain of X_i
        	revised <- false
        	for each x in D_i do
        		if no value y in D_j allows (x, y) to satisfy the constraint between X_i 		 and X_j then
        			delete x from D_i
        			revised <- true
            return revised
        ```

      - Maintains a queue of arcs to consider

      - Pops off an arbitrary arc `(X_i, X_j)` and makes `X_i` arc-consistent with `X_j`

        - If this leaves `D_i` unchanged, the algorithm moves on to the next arc
        - If this revises `D_i`, then we add to the queue all arcs `(X_k, X_i)`, where `X_k` is a neighbor of `X_i`
          - This is because the change in `D_i` might enable further reductions in the domains of `D_k`
          - If `D_i` is revised down to nothing, then we know the whole CSP has no consistent solution

      - If the algorithm succeeds, we are left with a CSP that is equivalent to the original CSP, but the arc-consistent CSP will be faster to search because its variables have smaller domains

      - Complexity Analysis

        - Assume a CSP with `n` variables, each with domain size at most `d`, and with `c` binary constraints
        - Each arc can only be inserted in the queue `s` times because `X_i` has at most `d` values to delete
        - Checking consistency of an arc can be done in `O(d^2)` time
        - Worst-case time is therefore `O(cd^3)`

    - Possible to generalize arc consistency to handle `n`-ary constraints as well

      - Called generalized arc consistency or hyperarc consistency
      - A variable `X_i` is generalized arc consistent with respect to an `n`-ary constraint if for every value `v` in the domain of `X_i`, there exists a tuple of values that is a member of the constraint, has all its values taken from the domains of the corresponding variables, and have its `X_i` component equal to `v`

  - Path Consistency

    - Path consistency tightens the binary constraints by using implicit constraints that are inferred by looking at triples of values
    - A two variable set `{X_i, X_j}` is path-consistent with respect to a third variable `X_m` if, for every assignment `{X_i = a, X_j = b}` consistent with the constraints on `{X_i, X_j}`, there is an assignment to `X_m` that satisfies the constraints on `{X_i, X_m}` and `{X_m, X_j}`
      - One can think of this as looking at a path from `X_i` to `X_j` with `X_m` in the middle
    - The PC-2 algorithm achieves path consistency in the same way that AC-3 achieves arc consistency

  - `K`-Consistency

    - A CSP is `k`-consistent if, for any set of `k - 1` variables and for any consistent assignment to those variables, a consistent value can always be assigned to any `k`th variable
      - 1-consistency says that, given the empty set, we can make any set of one variable consistent, aka, node consistency
      - 2-consistency is the same as arc consistency
      - 3-consistency is the same as path consistency for binary constraint networks
    - A CSP is strongly `k`-consistent if it is `k` consistent and is also `(k - 1)`-consistent, `(k - 2)`-consistent, etc. all the way down to 1-consistent
      - Assume we have a CSP with `n` nodes and make it strongly `n`-consistent
      - We can then solve the problem as follows:
        - Choose a consistent value for `X_1`
        - We are guaranteed to be able to choose a value for `X_2` because the graph is 2-consistent
        - Repeat
      - For each variable `X_i`, we only need to search through the `d` values in the domain to find a value consistent with `X_1, ..., X_i-1`
        - Guaranteed to find a solution in time `O(n^2d)`
      - Algorithm for establishing `n`-consistency takes time and space exponential in `n`

  - Global Constraints

    - Global constraints occur frequently in real problems and can be handled by special-purpose algorithms that are more efficient than the general-purpose methods described so far
    - `Alldiff`
      - If `m` variables are involved in the constraint, and if they have `n` possible distinct values together, and `m > n`, then the constraint cannot be satisfied
      - Simple algorithm:
        - Remove any variable in the constraint that has a singleton domain, and delete that variable's value from the domains of the remaining variables
        - Repeat as long as there are singleton variables
        - If at any point an empty domain is produced or there are more variables than the domain values left, then an inconsistency has been detected
    - Resource Constraint
      - Called the `Atmost` constraint
    - Bounds Propagation
      - Used for resource-limited problems with integer values in which the domain of each variable cannot be represented as a large set of integers
        - Represent the domains as upper and lower bounds
      - A CSP is bounds consistent if for every variable `X`, and for both the lower-bound and upper-bound values of `X`, there exists some value of `Y` that satisfies the constraint between `X` and `Y` for every variable `Y`

- Backtracking Search for CSPs

  - CSPs have the property of commutativity, as the order of application of any given set of actions has no effect on the outcome

    - When assigning values to variables, the same partial assignment is reached, regardless of order
    - Build a search tree by considering a single variable at each node in the search tree

  - Backtracking search refers to a DFS 5that chooses values for one variable at a time and backtracks when a variable has no legal values left to assign

  - ```pseudocode
    function BACKTRACKING_SEARCH(csp) returns a solution, or failure
    	return BACKTRACK({}, csp)
    	
    function BACKTRACK(assignment, csp) returns a solution, or failure
    	if assignment is complete then return assignment
    	var <- SELECT_UNASSIGNED_VARIABLE(csp)
    	for each value in ORDER_DOMAIN_VALUES(var, assignment, csp) do
    		if value is consistent with assignment then
    			add {var = value} to assignment
    			inferences <- INFERENCE(csp, var, value)
    			if inferences != failure then
    				add inferences to assignment
    				result <- BACKTRACK(assignment, csp)
    				if result !=- failure then
    					return result
            remove {var = value} and inferences from assignment
        return failure
    ```

    - Repeatedly chooses an unassigned variable, and then tries all values in the domain of that variable in turn, trying to find a solution
    - If an inconsistency is detected, `BACKTRACK` returns failure, causing the previous call to try another value
    - Only keeps a single representation of a state and alters the representation rather than creating new ones

  - CSPs can be solved efficiently without domain-specific knowledge

    - Can improve algorithm by adding sophistication to the functions used in `BACKTRACKING_SEARCH`
      - Which variable should be assigned next, and in what order should its values be tried?
      - What inferences should be performed at each step in the search?
      - When the search arrives at an assignment that violates a constraint, can the search avoid repeating this failure?

  - Variable and Value Ordering

    - The simplest strategy for `SELECT_UNASSIGNED_VARIABLE` is to choose the next unassigned variable in order
    - The minimum-remaining-values heuristic tells us to choose the variable with the fewest "legal" values
      - Picks the variable that is most likely to cause a failure soon, pruning the search tree
      - Can detect failure much faster
    - The degree heuristic selects the variable that is involved in the largest number of constraints on other unassigned variables
      - Less powerful than MRV, but can be a useful tiebreaker
    - The least-constraining value heuristic prefers the value that rules out the fewest choices for the neighboring variables in the constraint graph
      - Tries to leave the maximum flexibility for subsequent variable assignments
      - Doesn't matter in the cases where we are trying to find all solutions to a problem or there isn't a solution
    - Variable selection is fail-first while value selection is fail-last
      - Variable ordering that chooses a variable with the minimum number of remaining values minimizes the number of nodes in the search tree
      - Value ordering only needs one solution, so it makes sense to look for the most likely values first

  - Interleaving Search and Inference

    - Every time we make a choice of a value for a variable, we have a brand-new opportunity to infer new domain reductions on the neighboring variables
    - Forward Checking
      - When a variable is assigned, the forward-checking process establishes arc-consistency for it
      - No reason to do this if arc consistency was already established in preprocessing
    - MAC (Maintaining Arc Consistency)
      - After a variable `X_i` is assigned a value, the `INFERENCE` procedure calls `AC-3`
        - This starts with only in the arcs `(X_j, X_i)` for all `X_j` that are unassigned variables that are neighbors of `X_i` in the queue
        - If any variable has its domain reduced to the empty set, the call to `AC-3` fails and we know to backtrack immediately

  - Intelligent Backtracking: Looking Backward

    - `BACKTRACKING_SEARCH` backs up to the preceding variable and tries a different value for it when the search fails

      - Called chronological backtracking because the most recent decision point is revisited

    - More intelligent approach involves backtracking to a variable that might fix the problem

      - Track the conflict set, the set of assignments that are in conflict with the failed value
        - Can be built using forward checking with no extra work
      - Backjumping backtracks to the most recent assignment in the conflict set
        - Redundant in a forward-checking search

    - Redefine conflict set as the set of preceding variables that caused a variable, along with any subsequent variables to have no consistent solution

      - Conflict-directed backjumping

      - Let `X_j` be the current variable and let `conf(X_j)` be its conflict set

      - If every possible value for `X_j` fails, backjump to the most recent variable `X_i` in `conf(X_i)` and set:

        - $$
          conf(X_i)\leftarrow conf(X_i)\cup conf(X_j)-\{X_i\}
          $$

      - When we reach a contradiction, backjumping can tell us how far to back up, so we don't waste time changing variables that won't fix the problem

    - When the search arrives at a contradiction, we know that some subset of the conflict set is responsible for the problem

      - Constraint learning is the idea of finding a minimum set of variables from the conflict set that causes the problem
        - This set of variables, along with their corresponding values is called a no-good

- Local Search for CSPs

  - Local search algorithms are effective at solving many CSPs

  - In choosing a new value for a variable, the most obvious heuristic is to select the value that results in the minimum number of conflicts with other variables

    - Min-conflicts heuristic

    - ```pseudocode
      function MIN_CONFLICTS(csp, max_steps) returns a solution or failure
      	inputs: csp, a constraint satisfaction problem
      	        max_steps, the number of steps allowed before giving up
      	        
      	current <- an initial complete assignment for csp
      	for i = 1 to max_steps do
      		if current is a solution for csp then return current
      		var <- a randomly chosen conflicted variable from csp.VARIABLES
      		value <- the value v for var that minimizes CONFLICTS(var, v, current, csp)
      		set var = value in current
          return failure
      ```

  - Constraint Weighting

    - Helps concentrate the search on important constraints
    - Each constraint is given a numeric weight `W`, initially all `1`
    - At each step of the search, the algorithm chooses a variable/value pair to change that will result in the lowest total weight of all violated constraints
    - Weights are then adjusted by incrementing the weight of each constraint that is violated by the current assignment
    - Two benefits:
      - Adds topography to plateaus, making sure it is possible to improve from the current state
      - Adds weight to the constraints that are proving difficult to solve over time

  - Can be used in an online setting when the problem changes

- The Structure of Problems

  - Independent subproblems are subproblems for which any solution for one subproblem can be combined with any solutions of the other subproblems to generate a solution for the overall problem

    - Independence can be ascertained by finding connected components of the constraint graph

  - Tree Structures

    - A constraint graph is a tree when any two variables are connected by only one path

    - Any tree-structured CSP can be solved in time linear in the number of variables

      - Key is directed arc consistency
      - A CSP is directed arc-consistent under an ordering of variables `X_1`, `X_2`, ..., `X_n` iff every `X_i` is arc-consistent with each `X_j` for `j > i`

    - Solving Tree-Structured CSPs

      - First, pick any variable to be the root of the tree, and choose an ordering of the variables such that each variable appears after its parent in the tree

        - This ordering is called a topological sort

      - Any tree with `n` nodes has `n - 1` arcs, so we can make this graph directed arc-consistent in `O(n)` steps, each of which must compare up to `d` possible domain values for two variables, for a total time of `O(nd^2)`

      - We can now march down the list of variables and choose any remaining value, guaranteed to be available due to the directed arc consistency

      - ```pseudocode
        function TREE_CSP_SOLVER(csp) returns a solution, or failure
        	inputs: csp, a CSP with components X, D, C
        	
        	n <- number of variables in X
        	assignment <- an empty assignment
        	root <- any variable in X
        	X <- TOPOLOGICAL_SORT(X, root)
        	for j = n down to 2 do
        		MAKE_ARC_CONSISTENT(PARENT(X_j), X_j)
        		if it cannot be made consistent then return failure
        	for i = 1 to n do
        		assignment[X_i] <- any consistent value from D_i
        		if there is no consistent value then return failure
        	return assignment
        ```

  - Knowing that we have an efficient algorithm for trees, the next goal is to find a way to convert more general constraint graphs to trees

    - Removing Nodes
      - Involves assigning values to some variables so that the remaining variables form a tree
      - General Algorithm:
        - Choose a subset `S` of the CSP's variables such that the constraint graph becomes a tree after removal of `S`
          - `S` is called a cycle cutset
        - For each possible assignment to the variables in `S` that satisfies all conditions on `S`:
          - Remove from the domains of the remaining variables any values that are inconsistent with the assignment for `S`
          - If the remaining CSP has a solution, return it together with the assignment for `S`
      - If the cycle cutset has size `c`, then the total runtime is `O(d^c(n - c)d^2)`
        - We have to try each of the `d^c` combinations of values for the variables ub `S`
        - For each combination, we must solve a tree problem of size `n - c `
      - Finding the smallest cycle cutset is NP-hard, but several efficient approximation algorithms are known
      - Overall approach is called cutset conditioning
    - Collapsing Nodes
      - Constructing a tree decomposition of the constraint graph into a set of connected subproblems
        - Each subproblem is solved independently, and the solutions are then combined
      - Tree decomposition must satisfy the following requirements:
        - Every variable in the original problem appears in at least one of the subproblems
        - If two variables are connected by a constraint in the original problem, they must appear together (along with the constraint) in at least one of the subproblems
        - If a variable appears in two subproblems in the tree, it must appear in every subproblem along the path connecting those subproblems
      - We solve each subproblem independently
        - If any one has no solution, we know the entire problem has no solution
          - If we can solve all the subproblems, we construct a global solution as follows:
            - View each subproblem as a "mega-variable" whose domain is the set of all solutions for the subproblem
            - Then solve the constraints connecting the subproblems using the algorithm for trees
            - Constraints between subproblems insist that the subproblem solutions agree on their shared variables
      - In choosing a decomposition, the aim is to make the subproblems as small as possible
        - The tree width of a tree decomposition of a graph is one less than the size of the largest subproblem
        - The tree width of the graph itself is defined to be the minimum tree width among all its tress decompositions
        - If a graph has tree width `w` and we are given the corresponding tree decomposition, then the problem can be solved in `O(nd^w+1)` time
          - Hence, CSPs with constraint graphs of bounded tree width are solvable in polynomial time

- Summary

  - Constraint satisfaction problems (CSPs) represent a state with a set of variable/value pairs and represent the codnitions for a solution by a set of constraints on the variables
    - Many important real-world problems can be described as CSPs
  - A number of inference techniques use the constraints to infer which variable/value pairs are consistent and which are not
    - These include node, arc, path, and `k`-consistency
  - Backtracking search, a form of DFS, is commonly used for solving CSPs
    - Inference can be interwoven with search
    - The minimum-remaining-values and degree heuristics are domain-independent methods for deciding which variable to choose next in a backtracking search
    - The least-constraining-value heuristic helps in deciding which value to try first for a given variable
    - Backtracking occurs when no legal assignment can be found for a variable
    - Conflict-directed backjumping backtracks directly to the source of the problem
  - Local search using the min-conflicts heuristic has also been applied to constraint satisfaction problems with great success
  - The complexity of solving a CSP is strongly related to the structure of its constraint graph
    - Tree-structured problems can be solved in linear time
    - Cutset conditioning can reduce a general CSP to a tree-structured one and is quite efficient if a small cutset can be found
    - Tree decomposition techniques transform the CSP into a tree of subproblems and are efficient if the tree width of the constraint graph is small



## Reading 6: Adversarial Search

- Games

  - The unpredictability of multiagent environments can introduce contingencies into the agent's problem solving process
  - In competitive environments, the agents' goals are in conflict, giving rise to adversarial search problems or games
  - Mathematical game theory views all multiagent environments as games, provided that the impact the agents have on each other is significant
    - Regardless of whether the agents are cooperative or competitive
  - Most common games in AI are deterministic, turn-taking, two-player, zero-sum games of perfect information
    - Deterministic, fully observable environments in which two agents act alternately and in which the utility values at the end of the game are always equal and opposite
    - Opposition between the agents' utility functions makes the situation adversarial
  - Abstract nature of games makes them an appealing subject for study
    - The state of the game is easy to represent
    - Agents are usually restricted to a small number of actions whose outcomes are defined by precise rules
    - Physical games, which have much more complex descriptions, a wider range of actions, and imprecise rules, have not attract much interest in regards to AI
  - Games require the ability to make some decision even when calculating the optimal decision is infeasible
    - Penalize efficiency severely
  - Start with a definition of the optimal move and an algorithm for finding it
    - Then look at techniques for choosing a good move when time is limited
    - Pruning allows us to ignore portions of the search tree that make no difference to the final choice
    - Heuristic evaluation functions allow for the approximation of the true utility of the state without doing a complete search
  - A game can be formally defined as a search problem with the following elements:
    - `S_0`: the initial state, which specifies how the game is set up at the start
    - `PLAYER(s)`: defines which player has the move in a state
    - `ACTIONS(s)`: returns the set of legal moves in a state
    - `RESULT(s, a)`: the transition model, which defines the result of a move
    - `TERMINAL-TEST(s)`: a terminal test, which is true when the game is over and false otherwise
      - States where the game has ended are called terminal states
    - `UTILITY(s, p)`: a utility function defines the final numeric value for a game that ends in terminal state `s` for player `p`
      - A zero-sum game is defined as one where the total payoff to all players is the same for every instance of the game
  - The initial state, `ACTIONS`, and `RESULT` define the game tree for the game
    - Nodes are game states, and the edges are moves
    - A search tree is a tree that is superimposed on the game tree, and examines enough nodes to allow a player to determine what move to make

- Optimal Decisions in Games

  - In adversarial search, `MAX` must find a contingent strategy which specifies its move in the initial state, then its moves in the states resulting from every possible response by `MIN`, and so on

    - An optimal strategy leads to outcomes at least as good as any other strategy when one is playing an infallible opponent

  - Given a game tree, the optimal strategy can be determined from the minimax value of each node, written as `MINIMAX(n)`

    - The minimax value of a node is the utility (for `MAX`) of being in the corresponding state, assuming that both players play optimally from there to the end of the game
    - The minimax value of a terminal state is just its utility
    - `MAX` prefers to move to a state of maximum value, while `MIN` prefers to move to a state of minimum value

  - The Minimax Algorithm

    - ```pseudocode
      function MINIMAX-DECISION(state) returns an action
      	return arg max(a ∈ ACTIONS(s)) MIN-VALUE(RESULT(state, a))
      	
      function MAX-VALUE(state) returns a utility value
      	if TERMINAL-TEST(state) then return UTILITY(state)
      	v <- -INF
      	for each a in ACTIONS(state) dp
      		v <- MAX(v, MIN-VALUE(RESULT(s, a)))
      	return v
      
      function MIN-VALUE(state) returns a utility value
      	if TERMINAL-TEST(state) then return UTILITY(state)
      	v <- INF
      	for each a in ACTIONS(state) do
      		v <- MIN(v, MAX-VALUE(RESULT(s, a)))
      	return v
      ```

    - Computes the minimax decision from the current state

    - Recursive computation of the minimax values of each successor state, proceeding all the way down the leaves of the tree, and then the minimax values are backed up through the tree as the recursion unwinds

    - Performs a depth-first exploration of the game tree

    - Assume the maximum depth of the tree is `m` and there are `b` legal moves at each point:

      - Time complexity is `O(b^m)`
      - Space complexity is `O(bm)` for an algorithm that generates all actions at once, or `O(m)` for an algorithm that generates actions one at a time

    - Serves as the basis for the mathematical analysis of games and for more practical algorithms

  - Optimal Decisions in Multiplayer Games

    - How do we extend the minimax idea to games with 2+ players?
      - Replace the single value for each node with a vector of values
        - For terminal states this vector gives the utility of the state from each player's viewpoint
        - Have `UTILITY` return a vector of utilities
      - The backed-up value of a node `n` is always the utility vector of the successor state with the highest value for the player choosing at `n`
    - Usually involve alliances, whether formal or informal among the players
      - Collaboration emerges from purely selfish behavior
    - Players will automatically cooperate to achieve a mutually desirable goal

- Alpha-Beta Pruning

  - Minimax search has too many game states to search

    - Alpha-beta pruning returns the same move minimax would, but prunes away branches that cannot possibly influence the final decision

  - Can be applied to trees of any depth

  - General principle:

    - Consider a node `n` somewhere in the tree, such that the player has a choice of moving to that node
    - If the player has a better choice `m` either at the parent node of `n` or at any choice point further up, then `n` will never be reached in actual play
    - Once we find out enough about `n` by examining some of its descendants to reach this conclusion, we can prune it
    - `α` is the value of the best (highest-value) choice we have found so far at any choice point along the path for `MAX`
    - `β` is the value of the best (lowest-value) choice we have found so far at any choice point along the path for `MIN`

  - Alpha-beta search updates the values of `α` and `β` as it goes along and prunes the remaining branches at a node as soon as the value of the current node is known to be worse than the current `α` or `β` value for `MAX` or `MIN`, respectively

  - ```pseudocode
    function ALPHA-BETA-SEARCH(state) returns an action
    	v <- MAX-VALUE(state, -INF, +INF)
    	return the action in ACTIONS(state) with value v
    	
    function MAX-VALUE(state, α, β) returns a utility value
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- -INF
    	for each a in ACTIONS(state) do
    		v <- MAX(v, MIN-VALUE(RESULT(s, a), α, β))
    		if v >= β then return v
    		α <- MAX(α, v)
    	return v
    
    function MIN-VALUE(state, α, β) returns a utility value
    	if TERMINAL-TEST(state) then return UTILITY(state)
    	v <- +INF
    	for each a in ACTIONS(state) do
    		v <- MIN(v, MAX-VALUE(RESULT(s, a), α, β))
    		if v <= α then return v
    		β <- MIN(β, v)
    	return v
    ```

  - Move Ordering

    - Effectiveness of alpha-beta pruning is highly dependent on the order in which the states are examined
      - Suggests that it might be worthwhile to try to examine first the successors that are likely to be the best
        - If this can be done, then it turns out that alpha-beta needs to examine only `O(b^m/2)` nodes to pick the best move
        - The effective branching factor becomes `sqrt(b)`
      - If successors are examined in random order, the number of nodes examined will be roughly `O(b^3m/4)`
    - Adding dynamic move-ordering schemes, such as trying first the moves that were found to be best in the past, brings us close to the theoretical limit
      - Past could be the previous move or previous exploration of the current move
      - One way to gain information from the current search is iterative deepening search
        - Search and record the best set of moves, using that recording to inform move ordering on subsequent searches
      - The best moves are called killer moves and trying them first is the killer move heuristic
    - Repeated states in the search tree can cause an exponential increase in search cost
      - Games have many repeated states due to transpositions, different permutations of the move sequence that end up in the same position
      - It is worthwhile to store the evaluation of the resulting position in a hash table the first time it's encountered so that we don't have to recompute it on subsequent occurrences
        - Traditionally called a transposition table
        - Essentially identical to the `explored` list in `GRAPH-SEARCH`
        - At some point it's not practical to keep all explored nodes in the transposition table, so we must choose which nodes to keep and which to discard

- Imperfect Real-Time Decisions

  - Even alpha-beta still has to search all the way to terminal states for at least a portion of the state space, which is usually not practical since moves must be made within a reasonable time
  
    - Proposal that programs should cut off the search earlier and apply a heuristic evaluation function to states in the search, turning nonterminal nodes into terminal leaves
  
  - Two changes:
  
    - Replace the utility function by a heuristic evaluation function `EVAL`
    - Replace the terminal test by a cutoff test that decides when to apply `EVAL`
  
  - Evaluation Function
  
    - Returns an estimate of the expected utility of the game from a given position
    - An inaccurate evaluation function will guide an agent toward positions that turn out to be lost
    - How do we design good evaluation functions?
      - The evaluation function should order the terminal states in the same way as the true utility function
        - Otherwise, an agent using the evaluation function might err even if it can see ahead all the way to the end of the game
      - The computation must not take too long
      - The evaluation function should be strongly correlated with the actual chances of winning for nonterminal states
        - Since the search is cutoff at states, the algorithm will necessarily be uncertain at these states
        - Uncertainty induced by computational limitations
    - Most evaluation functions work by calculating various features of the state
      - These features, taken together, define various categories or equivalence classes of states
        - The states in each category have the same values for all the features
      - Any given category will contains various states that lead to various outcomes
        - The evaluation function cannot know which states are which, but it can return a single value that reflects the proportion of states with each outcome
        - A reasonable evaluation for states in the category is the expected value
        - This analysis leads to too many categories/too much experience to estimate all the probabilities of winning
      - Most evaluation functions compute separate numerical contributions from each feature and then combine them to find the total value
        - Feature values can then be added up to obtain the evaluation of the position
          - Requires the strong assumption that the contribution of each feature is independent of the values of the other features
          - Nonlinear combinations of features are also used
        - Weights can be determined through experience or machine learning
  
  - Cutting Off Search
  
    - Modify `ALPHA-BETA-SEARCH` so that it will call the heuristic `EVAL` function when it is appropriate to cut off the search
  
      - ```pseudocode
        if CUTOFF-TEST(state, depth) then return EVAL(state)
        ```
  
    - The most straightforward approach for controlling the amount of search is to set a fixed depth limit so that `CUTOFF-TEST(state, depth)` returns `true` for all `depth` greater than some fixed depth `d`
  
      - Must also return `true` for all terminal states
      - `d` chosen so that a move is selected within the allotted time
  
    - Can also apply iterative deepening
  
      - When time runs out, the program returns the move selected by the deepest completed search
      - Also helps with move ordering
  
    - Simple approaches can lead to errors due to the approximate nature of the evaluation function => more sophistication needed
  
      - Evaluation function should only be applied to positions that are quiescent (unlikely to exhibit wild swings in value in the near future
      - Expand nonquiescent positions until quiescent positions are reached
        - Called quiescence search
  
    - Horizon effect is more difficult to eliminate
  
      - Arises when the program is facing an opponent's move that causes serious damage and is ultimately unavoidable, but can be temporarily avoided by delaying tactics
      - One strategy to avoid it is the singular extension, a move that is "clearly better" than all other moves in a given position
  
  - Forward Pruning
  
    - Some moves at a given node are pruned immediately without further consideration
    - One approach is beam search
      - On each ply, consider only a beam of the `n` best moves (according to the evaluation function)
      - No guarantee that the best move won't be pruned away
    - The `PROBCUT` or probabilistic cut algorithm uses statistics gained from prior experience to lessen the chance that the best move will be pruned
      - Prunes nodes that are probably outside the `(α, β)` window
      - Does a shallow search to compute the backed-up value `v` of a node and then uses past experience to estimate how likely it is that a score of `v` at depth `d` in the tree would be outside `(α, β)`
  
  - Search vs. Lookup
  
    - Many game-playing programs use table lookup rather than search for the opening and ending of games
      - Computers can rely on the expertise of humans and the previous games they've played to determine the probabilities that a given opening will win
        - Few choices leads to an abundance of expert commentary and past games on which to draw
      - Computers have the upper-hand when it comes to the endgame
        - Can completely solve the endgame by producing a policy (a mapping from every possible state to the best move in that state)
        - The policy allows us to lookup the move instead of recomputing it
  
- Stochastic Games

  - Many games mirror unpredictability by including a random element
    - i.e., throwing of a dice
    - These games are called stochastic games
  - Stochastic games' game trees must include chance nodes in addition to `MAX` and `MIN` nodes
    - Branches leading from chance nodes denote possibilities of the random action
  - We still want to pick a move that leads to the best position, but we can only calculate the expected value of a position
    - This is the weighted average over all possible outcomes of the chance nodes
    - Minimax value must be generalized to an expectiminimax value for games with chance nodes
  - Evaluation Functions for Games of Chance
    - The obvious approximation to make with expectiminimax is to cut the search off at some point and apply an evaluation function to each leaf
      - The presence of chance nodes means that we have to be more careful about what the evaluation values mean
      - The evaluation function must be a positive linear transformation of the probability of winning from a position
      - For a game with branching factor `b`, maximum depth `m`, and `n` distinct rolls, expectiminimax will take `O(b^mn^m)` 
    - Alpha-beta improves minimax by concentrating on likely occurrences
      - However, in games with random chance, there are no likely sequences of moves, as the random events may alter legal sequences
      - Forming details plans of action becomes pointless because the world probably will not play along
    - Monte Carlo simulation can be used to evaluate a position
      - Have a selected search algorithm play thousands of games against itself, using random chance
      - The resulting win percentage has been shown to be a good approximation of the value of the position

- Partially Observable Games

  - Kriegspiel: Partially Observable Chess

    - Use the notion of a belief state, the set of all logically possible game states given the complete history of percepts to date
    - Keeping track of the belief state as the game progresses is exactly the problem of state estimation
    - For a partially observable game, strategies don't specify a move to make for each possible move the opponent might make, we need a move for every possible percept sequence that might be received
    - An equilibrium specifies an optimal randomized strategy for each player

  - Card Games

    - Naive concept: consider all possible deals, solve each one as if it were an observable game, then choose the move that has the best outcome averaged over all the deals

      - Assuming each deal `s` occurs with probability `P(s)`, the move we want is:

        - $$
          \text{argmax}_a\sum_sP(s)\texttt{MINIMAX}(\texttt{RESULT}(s,a))
          $$

    - The amount of deals is far to high to feasibly solve, so we resort to Monte Carlo approximation

      - Take a random sample of `N` deals, where the probability of deal `s` appearing in the sample is proportional to `P(s)`:

        - $$
          \text{argmax}_a\frac{1}{N}\sum^N_{i=1}\texttt{MINIMAX}(\texttt{RESULT}(s_i,a))
          $$

      - Method gives a good approximation, even for fairly small `N`

      - Can also be applied to deterministic games given some reasonable estimate of `P(s)`

    - Averaging over clairvoyance fails because it assumes that the game will become observable to both players immediately after the first move

      - Does not consider the belief state that the agent will be in after acting
      - Belief state of total ignorance is not desirable
      - Since it assumes every future state is one of perfect knowledge, the approach never selects actions that gather information or hide information

- Alternative Approaches

  - The standard approach of minimax and alpha-beta searches doesn't provide much room for new insight into general questions of decision making
  - Introduce the idea of the utility of a node expansion
    - If there are no node expansions whose utility is higher than their cost, then the algorithm should stop searching and make a move
    - Works for clear-favorite situations and for symmetrical moves
    - This reasoning about what moves to do is called metareasoning
  - Goal-directed reasoning can eliminate combinatorial search

- Summary

  - A game can be defined by the initial state (how the board is set up), the legal actions in each state, the result of each action, a terminal test (which says when the game is over), and a utility function that applies to terminal states
  - In two-player zero-sum games with perfect information, the minimax algorithm can select optimal moves by a depth-first enumeration of the game tree
  - The alpha-beta search algorithm computes the same optimal move as minimax, but achieves much greater efficiency by eliminating subtrees that are provably irrelevant
  - Usually, it is not feasible to consider the whole game tree (even with alpha-beta), so we need to cut the search off at some point and apply a heuristic evaluation function that estimates the utility of a state
  - Many game programs precompute tables of best moves in the opening and endgame so that they can look up a move rather than search
  - Games of chance can be handled by an extension to the minimax algorithm that evaluates a chance node by taking the average utility of all its children, weighted by the probability of each child
  - Optimal play in games of imperfect information requires reasoning about the current and future belief states of each player
    - A simple approximation can be obtained by averaging the value of an action over each possible configuration of missing information
  - Programs have bested even champion human players at games such as chess, checkers, and Othello
    - Humans retain the edge in several games of imperfect information, such as poker, bridge, etc. and in games with very large branching factors and little good heuristic knowledge



## Reading 7: Logical Agents

- Knowledge-Based Agents

  - The central component of a knowledge-based agent is its knowledge base, or KB

    - This is a set of sentences
    - Each sentence is expressed in a language called a knowledge representation language and represents some assertion about the world
    - A sentence is called an axiom when the sentence is taken as given without being derived from other sentences
    - `TELL` is a way to add new sentences to the knowledge base
    - `ASK` is a way to query the knowledge base for what is known
      - Both operations involve inference, the process of deriving new sentences from old ones
      - Inference must obey the requirement that when one `ASK`s a question of the KB, the answer should follow from what has been `TELL`ed to the KB previously
        - Inference shouldn't make things up as it goes along

  - ```pseudocode
    function KB-AGENT(percept) returns an action
    	persistent: KB, a knowledge base
    	            t, a counter, initially 0, indicating time
        
        TELL(KB, MAKE-PERCEPT-SENTENCE(percept, t))
        action <- ASK(KB, MAKE-ACTION-QUERY(t))
        TELL(KB, MAKE-ACTION-SEQUENCE(action, t))
        t <- t + 1
        return action
    ```

    - The KB may initially contain some background knowledge
    - Each time the agent program is called, it does 3 things:
      - `TELL`s the KB what it perceives
      - `ASK`s the KB what action it should perform
        - In the process of answering this query, extensive reasoning may be done about the current state of the world, outcomes of possible action sequences, etc.
      - The agent program `TELL`s the KB which action was chosen, and the agent executes the action
    - The details of the representation language are hidden inside 3 functions that implement the interface between the sensors/actuators on one side and the core representation and reasoning system on the other:
      - `MAKE-PERCEPT-SEQUENCE` constructs a sentence asserting that the agent perceived the given percept at the given time
      - `MAKE-ACTION-QUERY` constructs a sentence that asks what action should be done at the current time
      - `MAKE-ACTION-SENTENCE` constructs a sentence asserting that the chose action was executed

  - The knowledge-based agent is not an arbitrary program for calculating actions

    - It is amenable to a description at the knowledge levels
    - We need specify only what the agent knows and what its goals are in order to fix its behavior
    - Independent of how the agent works at the implementation level

  - The declarative approach to system building involves `TELL`ing the agent what it needs to know until it can operate in its environment

  - The procedural approach encodes desired behaviors directly as program code

  - A successful agent often combines both the declarative and procedural approach

    - Declarative knowledge can often be compiled into more efficient procedural code

- The Wumpus World

  - The wumpus world is an environment in which knowledge-based agents can show their worth
    - Is a cave consisting of rooms connected by passageways
    - The wumpus lurks somewhere in the cave, and eats anyone who enters its room
    - The agent has one arrow, with which they can attempt to shoot the wumpus with
    - Some rooms contain bottomless pits that will trap the agent, but not the wumpus
    - There is a possibility of finding a heap of gold
    - PEAS description:
      - Performance measure: `+1000` for climbing out of the cave with the gold, `-1000` for falling into a pit or being eaten by the wumpus, `-1` for each action taken, `-10` for using up the arrow
        - The game ends when either the agent dies or when the agent climbs out of the cave
      - Environment: a 4 x 4 grid of rooms
        - The agent always starts in the square labeled `[1, 1]`, facing to the right
        - The locations of the gold and the wumpus are chosen randomly, with a uniform distribution, from the squares other than the start square
        - Each square other than the start can be a pit, with probability 0.2
      - Actuators: the agent can move `Forward`, `TurnLeft` by 90°, or `TurnRight` by 90°
        - The agent dies if they enter a room with a pit or a living wumpus
        - If the agent tries to move forward into a wall, they don't move
        - `Grab` can be used to pick up gold if it is in the same room as the agent
        - `Shoot` can be used to fire an arrow in a straight line in the direction the agent is facing
          - This arrow continues until it hits the wumpus or a wall
          - Only the first `Shoot` action has any effect
        - `Climb` can be used to climb out of the cave from square `[1, 1]`
      - Sensors: the agent has 5 sensors, each of which gives a single bit of information:
        - In the square containing the wumpus and in the directly adjacent squares the agent will perceive a `Stench`
        - In the squares directly adjacent to a pit, the agent will perceive a `Breeze`
        - In the square where the gold is, the agent will perceive a `Glitter`
        - When an agent walks into a wall, it will perceive a `Bump`
        - When the wumpus is killed, it emits a `Scream` that can be perceived anywhere in the cave
        - The percepts will be given to the agent program in the form of a list of 5 symbols
  - The wumpus environment is discrete, static, and single-agent
    - It's sequential because rewards may come only after actions are taken
    - It's partially observable, as some aspects of the state are not directly perceivable
      - The agent's location, the wumpus' status, and the availability of an arrow
  - The main challenge for an agent in the environment is the initial ignorance of the configuration of the environment
    - This would seem to require logical reasoning
  - Note that when the agent draws a conclusion from the available information, that conclusion is guaranteed to be correct if the available information was correct

- Logic

  - The sentences that make up KBs are expressed according to the syntax of the representation language

    - This syntax specifies all sentences that are well formed

  - Logic must also define the semantics/meaning of sentences

    - The semantics define the truth of each sentence with respect to each possible world
    - In standard logics, every sentence must be either true or false in each possible world, it cannot be in between

  - When precision is required, the term "possible world" is replaced with model

    - Possible worlds are thought of as (potentially) real environments that the agent may or may not be in
    - Models are mathematical abstractions, each of which fixes the truth or falsehood of every relevant sentence
    - If a sentence `α` is true in model `m`, we sat that `m` satisfies `α`, or that `m` is a model of `α`
      - The notation `M(α)` is used to represent the set of all models of `α`

  - Logical reasoning involves the relation of logical entailment between sentences

    - This refers to the idea that a sentence follows logically from another sentence

    - If `α` entails `β`, we write that `α |= β`

    - The formal definition of entailment is that `α |= β` iff in every model in which `α` is true, `β` is also true

      - $$
        \alpha\models\beta\text{ if and only if }M(\alpha)\subseteq M(\beta)
        $$

      - Note that this means `α` is a stronger assertion than `β`, meaning `α` rules out more worlds

  - The KB can be thought of as a set of sentences or as a single sentence that asserts all the individual sentences

    - The KB is false in models that contradict what the agent knows

  - Using the definition of entailment to derive conclusions is called carrying out logical inference

    - Analogy:

      - Things of the set of all consequences of KB as a haystack and of `α` as a needle
      - Entailment is like the needle being in the haystack
      - Inferences is like finding the needle in the haystack

    - If an inference algorithm `i` can derive `α` from KB, we write:

      - $$
        KB\vdash_i\alpha
        $$

      - `α` is derived from KB by `i` or `i` derives `α` from KB

    - An inference algorithm that derives only entailed sentences is called sound or truth-preserving

      - Unsound inference algorithms essentially make things up as they go along

    - For an inference algorithm to be complete, it must be able to derive any sentence that is entailed

  - If KB is true in the real world, then any sentence `α` derived from KB by a sound inference procedure is also true in the real world

    - Inference process operates on syntax, but corresponds to the real-world relationship whereby some aspect of the real world is the case by virtue of other aspects of the real world being the case

  - Grounding is the connection between logical reasoning processes and the real environment in which the agent exists

    - How do we know that KB is true in the real world?
    - The agent's sensors connect its KB with the real world
      - The meaning of true and percept sequences are defined by the processes of sensing and sentence construction that produce them
    - The agent also has knowledge of general rules
      - Possibly derived from perceptual experience, but not a direct statement of that experience
      - Produced by a sentence construction process called learning
        - Not always 100% accurate, but good learning procedures give reason for optimism

- Propositional Logic: A Very Simple Logic

  - Syntax

    - The syntax of propositional logic defines the allowable sentences
    - The atomic sentences consist of a single proposition symbol
      - Each symbol stands for a proposition that can be true or false
      - These symbols start with an uppercase letter and may contain other letters or subscripts
        - These names are arbitrary, but are often chosen to have some mnemonic value

      - There exist 2 proposition symbols with fixed meanings:
        - `True` is the always-true proposition
        - `False` is the always-false proposition

    - Complex sentences are constructed from simpler sentences using parentheses and logical connectives
      - There are 5 connectives in common use:
        - `¬`: not
          - A sentence such as `¬X` is called the negation of `X`
          - A literal is either an atomic sentence (positive literal) or a negated atomic sentence (negative literal)

        - `∧`: and
          - A sentence whose main conjunctive is `∧` is called a conjunction
          - The parts of the conjunction are the conjuncts

        - `∨`: or
          - A sentence using `∨` is a disjunction
          - The parts of the disjunction are the disjuncts

        - `⇒`: implies
          - A sentence using `⇒` is called an implication
          - The LHS is called the premise or antecedent
          - The RHS is called the conclusion or consequent
          - Implications are also known as rules or if-then statements

        - `⇔`: iff
          - A sentence using `⇔` is a biconditional

      - Precedence: `¬`, `∧`, `∨`, `⇒`, `⇔`

  - Semantics

    - The semantics defines the rules for determining the truth of a sentence with respect to a particular model
      - In propositional logic, a model simply fixes the truth value (`true` or `false`) for every proposition symbol

    - The semantics for propositional logic must specify how to compute the truth value of any sentence, given a model
      - Done recursively
      - Atomic sentences:
        - `True` is true in every model and `False` is false in every model
        - The truth value of every other proposition symbol must be specified directly in the model
      - Complex sentences:
        - `¬P` is true iff `P` is false in `m`
        - `P ∧ Q` is true iff both `P` and `Q` are true in `m`
        - `P ∨ Q` is true iff either `P` or `Q` is true in `m`
        - `P ⇒ Q` is true unless `P` is true and `Q` is false in `m`
        - `P ⇔ Q` is true iff `P` and `Q` are both true or both false in `m`

  - A Simple Inference Procedure

    - ```pseudocode
      function TT-ENTAILS?(KB, α) returns true or false
      	inputs: KB, the knowledge base, a sentence in propositional logic
      	        α, the query, a sentence in propositional logic
      	        
      	symbols <- a list of the proposition symbols in KB and α
      	return TT-CHECK-ALL(KB, α, symbols, {})
      	
      function TT-CHECK-ALL(KB, α, symbols, model) returns true or false
      	if EMPTY?(symbols) then
      		if PL-TRUE?(KB, model) then return PL-TRUE?(α, model)
      		else return true // When KB is false, always return true
      	else do
      		P <- FIRST(symbols)
      		rest <- REST(symbols)
      		return (TT-CHECK-ALL(KB, α, rest, model ∪ { P = true })
      		        and
      		        TT-CHECK-ALL(KB, α, rest, model ∪ { P = false }))
      ```

      - Performs a recursive enumeration of a finite space of assignments to symbols
      - Sound because it implements directly the definition of entailment
      - Complete because it works for any KB and `α` and always terminates
        - `2^n` models, resulting in a `O(2^n)` time complexity

    - Propositional entailment is co-NP-complete, so every known inference algorithm for propositional logic has a worst-case complexity that is exponential in the size of the input

- Propositional Theorem Proving

  - Thus far, we have determined entailment through the process of model checking

    - Enumerating models and showing that the sentence must hold in all models

  - Entailment can also be done by theorem proving

    - Applying rules of inference directly to the sentences in our KB to construct a proof of the desired sentence without consulting models
    - If the number of models is large, but the length of the proof is short, this method can be more efficient than model checking

  - Two sentences `α` and `β` are logically equivalent if they are true in the same set of models

    - $$
      \alpha\equiv\beta
      $$

    - Equivalences play much the same role in logic as arithmetic identities do in ordinary mathematics

    - Any two sentences `α` and `β` are logically equivalent iff each of them entails the other:

      - $$
        \alpha\equiv\beta\text{ iff }\alpha\models\beta\text{ and }\beta\models\alpha
        $$

  - A sentence is valid if it is true in all models

    - Also known as tautologies, which are necessarily true
    - Every valid sentence is equal to `True`, since `True` is true in all models
    - Using the concept of validity and the definition of entailment, we can derive the deduction theorem:
      - For any sentences `α` and `β`, `α ⊨ β` iff the sentence `(α ⇒ β)` is valid
      - States that every valid implication sentence describes a legitimate inference

  - A sentence is satisfiable if it is true in, or satisfied by, some model

    - Satisfiability can be checked by enumerating the possible models until one is found that satisfies the sentence
    - The SAT problem is the problem of determining the satisfiability of sentences in propositional logic
      - First problem proved to be NP-complete

    - Connected with validity
      - `α` is valid iff `¬α` is unsatisfiable
      - `α` is satisfiable iff `¬α` is not valid
      - `α ⊨ β` iff the sentence `(α ∧ ¬β)` is unsatisfiable
        - Proof by contradiction

  - One property of logical systems is monotonicity, which says that the set of entailed sentences can only increase as information is added to the KB

    - $$
      \text{if }KB\models\alpha\text{ then }KB\and\beta\models\alpha
      $$

    - Inference rules can be applied whenever suitable premises are found in the KB
      - The conclusion of the rule must follow regardless of what else is in the KB

  - Inference and Proofs

    - Inference Rules

      - Modus Ponens

        - $$
          \frac{\alpha\implies\beta,\quad\alpha}{\beta}
          $$

        - Whenever any sentences of the form `α ⇒ β` and `α` are given, then the sentence `β` can be inferred

      - And-Elimination

        - $$
          \frac{\alpha\and\beta}{\alpha}
          $$

        - From a conjunction, any of the conjuncts can be inferred

    - We define a proof problem as follows:

      - Initial State: the initial KB
      - Actions: the set of actions consists of all the inference rules applied to all the sentences that match the top half of the inference rule
      - Result: the result of an action is to add the sentence in the bottom half of the inference rule
      - Goal: the goal is a state that contains the sentence we are trying to prove

    - In many practical cases, finding a proof can be more efficient because the proof can ignore irrelevant propositions, no matter how many of them there are

  - Proof by Resolution

    - Resolution is an inference rule that yields a complete inference algorithm when couple with any complete search algorithm

    - The unit resolution rule takes a clause and a literal and produces a new clause:

      - $$
        \frac{l_1\ \or...\or\ l_k,\quad m}{l_1\ \or...\or\ l_{i-1}\or l_{i+1}\ \or...\or\ l_k}
        $$

      - Each `l` is a literal and `l_i` and `m` are complementary literals

      - A clause is a disjunction of literals

      - Note that a single literal can be viewed as a disjunction of one literal, also known as a unit clause

    - The unit resolution rule can be generalized to the full resolution rule

      - $$
        \frac{l_1\ \or...\or\ l_k,\quad m_1\ \or...\or\ m_n}{l_1\ \or...\or\ l_{i-1}\or l_{i+1}\ \or...\or\ l_k\or m_1\or...\or\ m_{j-1}\ \or...\or\ m_n }
        $$

      - `l_i` and `m_j` are complementary literals

      - Says that resolution takes 2 clauses and produces a new clause containing all of the literals of the original 2 clauses except the 2 complementary literals

      - The resulting clause should contain only one copy of each literal

        - The removal of multiple copies of literals is called factoring

      - Forms the basis for a family of complete inference procedures

        - A resolution-based theorem prover can, for any sentences `α` and `β` in propositional logic, decide whether `α |= β`

    - Conjunctive Normal Form

      - Every sentence of propositional logic is logically equivalent to a conjunction of clauses
        - A sentence expressed as a conjunction of clauses is said to be in conjunctive normal form, or CNF

      - Procedure:
        - Eliminate `⇔`, replacing `α ⇔ β` with `(α ⇒ β) ∧ (β ⇒ α)`
        - Eliminate `⇒`, replacing `α ⇒ β` with `¬α ∨ β`
        - CNF requires `¬` to appear only in literals, so we move `¬` inwards by repeated application
          of the following equivalences:
          - `¬(¬α) ≡ α`
          - `¬(α ∧ β) ≡ (¬α ∨ ¬β)`
          - `¬(α ∨ β) ≡ (¬α ∧ ¬β)`

        - Now we have a sentence containing nested `∧` and `∨` operators applied to literals. We
          apply the distributivity law, distributing `∨` over `∧` wherever possible.

    - A Resolution Algorithm

      - ```pseudocode
        function PL-RESOLUTION(KB, α) returns true or false
        	inputs: KB, the knowledge base, a sentence in propositional logic
        	        α, the query, a sentence in propositional logic
        	        
            clauses <- the set of clauses in the CNF representation of KB ∧ ¬α
            new <- {}
            loop do
            	for each pair of clauses C_i, C_j in clauses do
            		resolvents <- PL-RESOLVE(C_i, C_j)
            		if resolvents contains the empty clause then return true
            		new <- new ∪ resolvents
                if new ⊆ clauses then return false
                clauses <- clauses ∪ new
        ```

        - Starts by converting `(KB ∧ ¬α)` into CNF
        - Then applies the resolution rule to the resulting clauses
          - Each pair that contains complementary literals is resolved to produce a new clause, which is added to the set if it is not already present
        - Continues until one of two things happen:
          - There are no new clauses that can be added, in which case KB does not entail `α`
          - Two clauses resolve to yield the empty clause, in which case KB entails `α`
            - Represents a contradiction, as it arises only from resolving two complementary unit clauses

    - Completeness of Resolution

      - The resolution closure `RC(S)` of a set of clauses `S` is the set of all clauses derivable by repeated application of the resolution rule to clauses in `S` or their derivatives
        - Computed by `PL-RESOLUTION` as the final value of `clauses`

      - The ground resolution theorem states that if a set of clauses is unsatisfiable, then the resolution closure of those clauses contains the empty clause

  - Horn Clauses and Definite Clauses

    - In many practical situations, the full power of resolution is not needed
      - Some real-world KBs satisfy certain restrictions on the form of sentences they contain, enabling them to use a more restricted and efficient inference algorithm

    - One restricted form is the definite clause, which is a disjunction of literals of which exactly one is positive
      - A more general version is the Horn clause, which is a disjunction of literals of which at most one is positive
        - All definite clauses are Horn clauses, as are clauses with no positive literals, called goal clauses

      - Horn clauses are closed under resolution
        - Resolving two Horn clauses results in another Horn clause

    - KBs containing only definite clauses are interesting because:
      - Every definite clause can be written as an implication whose premise is a conjunction of positive literals and whose conclusion is a single positive literal
        - In Horn form, the premise is called the body and the conclusion is called the head
        - A sentence consisting of a single positive literal is called a fact

      - Inference with Horn clauses can be done through the forward-chaining and backward-chaining algorithms
        - This type of inference is the basis for logic programming

      - Deciding entailment with Horn clauses can be done in time that is linear in the size of the KB

  - Forward and Backward Chaining

    - ```pseudocode
      function PL-FC-ENTAILS?(KB, q) returns true or false
      	inputs: KB, the knowledge base, a set of propositional definite clauses
      	        q, the query, a proposition symbol
          count <- a table, where count[c] is the number of symbols in c's premise
          inferred <- a table, where inferred[s] is initially false for all symbols
          agenda <- a queue of symbols, initially symbols known to be true in KB
          
          while agenda is not empty do
          	p <- POP(agenda)
          	if p = q then return true
          	if inferred[p] = false then
          		inferred[p] <- true
          		for each clause c in KB where p is in c.PREMISE do
          			decrement count[c]
          			if count[c] = 0 then add c.CONCLUSION to agenda
          return false
      ```

      - Determines if `q` is entailed by a KB of definite clauses
      - Begins from positive literals in the KB
      - Adds conclusion to the set of known facts if all the premises of an implication are known
        - Continues until the query `q` is added or no further inferences can be made
      - Runs in linear time

    - Is sound, as every inference is essentially an application of Modus Ponens

    - Complete, as every entailed atomic sequence will be derived once the algorithm reaches a fixed state

    - Example of the general concept of data-driven reasoning

      - Reasoning in which the focus of attention starts with the known data

    - Backward-chaining works backward from the query

      - Finds implications in the KB whose conclusion is `q`
      - If all the premises of one of these implications can be proved true through repeated backwards chaining, then `q` is true
      - Form of goal-directed reasoning
      - Cost is often much less than linear in the size of the KB, as the process only touches relevant facts

- Effective Propositional Model Checking

  - Section deals with 2 algorithms that work to solve the SAT problem

  - A Complete Backtracking Algorithm

    - Often called the Davis-Putnam Algorithm

    - Essentially a recursive, depth-first enumeration of possible models, but with various improvements:

      - Early termination

        - Detects whether the sentence must be true or false, even with a partially completed model
        - Avoids examination of entire subtrees in the search space

      - Pure symbol heuristic

        - A pure symbol is a symbol that always appears with the same sign in all clauses
        - If a sentence has a model, then it has a model with the pure symbols assigned so as to make their literals true
        - In determining the purity of a symbol, the algorithm can ignore clauses that are already known to be true in the model constructed so far

      - Unit clause heuristic

        - A unit clause is defined as a clause with just one literal
          - In the context of this algorithm, it also refers to clauses in which all literals but one are already assigned `false` by the model

        - This heuristic assigns all unit clauses before branching on the remainder
        - Unit propagation is when assigning one unit clause leads to another being assignable
          - Resembles forward chaining

      - ```pseudocode
        function DPLL-SATISFIABLE?(s) returns true or false
        	inputs: s, a sentence in propositional logic
        	
        	clauses <- the set of clauses in the CNF representation of s
        	symbols <- a list of the proposition symbols in s
        	return DPLL(clauses, symbols, {})
        	
        function DPLL(clauses, symbols, model) returns true or false
        	
        	if every clause in clauses is true in model then return true
        	if some clause in clauses is false in model then return false
        	P, value <- FIND-PURE-SYMBOL(symbols, clauses, model)
        	if P is non-null then return DPLL(clauses, symbols - P, model ∪ {P=value})
        	P, value <- FIND-UNIT-CLAUSE(clauses, model)
        	if P is non-null then return DPLL(clauses, symbols - P, model ∪ {P=value})
        	P <- FIRST(symbols); rest <- REST(symbols)
        	return DPLL(clauses, rest, model ∪ {P=true}) or
        	       DPLL(clauses, rest, model ∪ {P=false})
        ```

      - Various tricks exist to help SAT solvers scale up to large problems

        - Component analysis
          - As DPLL assigns truth values to variables, the set of clauses may become separated into disjoint subsets called components
          - A solver can gain significant speed by working on these components individually

        - Variable and value ordering
          - The current implementation uses an arbitrary value ordering and always tries `true` before `false`
          - The degree heuristic suggests choosing the variable that appears most frequently over the remaining clauses

        - Intelligent backtracking
          - Backtrack up to the relevant point of conflict
          - SAT solvers use some form of conflict clause learning to record conflicts so that they won't be repeated later in the search
            - Usually a limited-size set of conflicts is kept, while rarely-used ones are dropped

        - Random restarts
          - When a run appears to not be making progress, we can restart at the top of the search tree, picking different random choices in variable and value selection
          - Clauses learned in the initial run can be retained to help prune the search space
          - Reduces the variance on the time to solution

        - Clever indexing
          - Indexing structures must be updated dynamically as the computation proceeds

  - Local Search Algorithms

    - One of the simplest and most effective algorithms is `WALKSAT`

      - Picks an unsatisfied clause and picks a symbol in the clause to flip

      - Chooses randomly between 2 ways to flip the symbol:

        - A "min-conflicts" step that minimizes the number of unsatisfied clauses in the new state
        - A "random walk" step that picks the symbol randomly

      - When this algorithm returns a model, the input sentence is satisfiable

        - When the algorithm returns a failure, the sentence may be unsatisfiable, or the algorithm may have just run out of time
        - Most useful when we expect a solution to exist

      - ```pseudocode
        function WALKSAT(clauses, p, max_flips) returns a satisfying model or failure
        	inputs: clauses, a set of clauses in propositional logic
        	        p, the probability of choosing to do a random walk move, ~0.5
        	        max_flips, number of flips allowed before giving up
            
            model <- a random assignment of true/false to the symbols in clauses
            for i = 1 to max_flips do
            	if model satisfies clauses then return model
            	clause <- a randomly selected clause from clauses that is false in model
            	with probability p flip the value in model of a randomly selected symbol
            	else flip the symbol in clause maximizes the number of satisfied clauses
            return failure
        ```

      - Cannot always detect unsatisfiability, which is required to decide entailment

  - The Landscape of Random SAT Problems

    - Underconstrained problems are those in which there are few clauses relative to the number of variables, resulting in an easy SAT problem
    - Overconstrained problems are the opposite, and are likely to have no solutions

- Agents Based on Propositional Logic

  - The Current State of the World

    - A logical agent operates by deducing what to know from a KB of sentences about the world
      - KB is composed of axioms and percept sentences obtained from the agent's experience in a particular world

    - Note that a percept only asserts something about the current time
      - This idea extends to any aspect of the world that changes over time
      - These aspects are called fluent, essentially synonymous with state variable
      - Must be dictated by a transition model
      - By convention, the percept for a given time step happens first, followed by the action for that time step, followed by the transition to the next time step

    - Some aspects are time-independent, and are called atemporal variables
    - To describe how the world changes, we can write effect axioms that specify the outcome of an action at the next time step
      - The need for the effect axiom to state what remains unchanged as the result of an action is the frame problem
        - We could solve this issue by adding frame axioms that explicitly assert all the propositions that remain the same
        - Inefficient
        - In the real world, there are very many fluents, but each action typically only changes a small number of those fluents
          - The world exhibits locality

      - Change focus from writing axioms about actions to writing axioms about fluents
        - Called a successor-state axiom

    - Problem remains: confirm that all the necessary preconditions of an action hold for it to have its intended effect
      - Qualification problem
      - No complete solution in logic, system designers have to use good judgement to decide how detailed the model should be

  - A Hybrid Agent

    - The ability to deduce various aspects of the world can be combined with condition-action rules and with problem-solving algorithms to produce a hybrid agent

    - ```pseudocode
      function HYBRID-WUMPUS-AGENT(percept) returns an action
      	inputs: percept, a list, [stench, breeze, glitter, bump, scream]
      	persistent: KB, a knowledge base, initially the atemporal wumpus physics
      	            t, a counter, initially 0, indicating time
      	            plan, an action sequence, initially empty
          
          TELL(KB, MAKE-PERCEPT-SEQUENCE(percept, t))
          TELL the KB the temporal physics sentences for time t
          safe <- {[x, y]: ASK(KB, OK_x,y^t) = true}
          if ASK(KB, Glitter^t) = true then
          	plan <- [Grab] + PLAN-ROUTE(current, {[1, 1]}, safe) + [Climb]
          if plan is empty then
          	unvisited <- {[x, y]: ASK(KB, L_x,y^T) = false for all T <= t}
          	plan <- PLAN-ROUTE(current, unvisited ∩ safe, safe)
          if plan is empty and ASK(KB, HaveArrow^t) = true then
          	possible_wumpus <- {[x, y]: ASK(KB, ¬W_x,y) = false}
          	plan <- PLAN-SHOT(current, possible_wumpus, safe)
          if plan is empty then // no choice but to take a risk
          	not_unsafe <- {[x, y]: ASK(KB, ¬OK_x,y^t) = false}
          	plan <- PLAN-ROUTE(current, unvisited ∩ not_unsafe, safe)
          if plan is empty then
          	plan <- PLAN-ROUTE(current, {[1, 1]}, safe) + [Climb]
          action <- POP(plan)
          TELL(KB, MAKE-ACTION-SENTENCE(action, t))
          t <- t + 1
          return action
      
      function PLAN-ROUTE(current, goals, allowed) returns an action sequence
      	inputs: current, the agent's current position
      			goals, a set of squares; try to plan a route to one of them
      			allowed, a set of squares that can form part of the route
      			
          problem <- ROUTE-PROBLEM(current, goals, allowed)
          return A*-GRAPH-SEARCH(problem)
      ```

      - Agent program maintains and updates a KB as well as a current plan
      - Initial knowledge contains the atemporal axioms
      - At each time step, the new percept sentence is added along with all the axioms that depend on `t`
      - The agent then uses logical inference by `ASK`ing questions of the KB to work out safe/unexplored squares
      - Constructs a plan based on the priority of the agent's goals

  - Logical State Estimation

    - The program above has a major flaw: the computational expense involved in calls to `ASK` goes up over time
      - Obvious solution is to cache the results of inference so that the inference process at the next time step can build on the results of previous steps instead of building from scratch

    - Past history of percepts and all their ramifications can be replaced by the belief state
      - The process of updating the belief state as new percepts arrive is called state estimation
      - Maintaining this belief state as a logical formula would require a size exponential in the number of symbols

    - A very common natural scheme for approximate state estimation is to represent belief states as conjunctions of literals (1-CNF formulas)
      - Agent tries to prove `X^t` and `¬X^t` for each symbol given the belief state at `t - 1`
      - The conjunction of provable literals becomes the new belief state and the previous one is discarded
      - The set of possible states represented by the 1-CNF belief state includes all states that are in fact possible given the full percept history
        - Acts as a conservative approximation around the exact belief state

  - Making Plans by Propositional Inference

    - Basic idea:

      - Construct a sentence that includes:
        - `Init^0`: a collection of assertions about the initial state
        - `Transition^1, ... , Transition^t`: the successor-state axioms for all possible actions at each time up to some maximum time `t`
        - The assertion that the goal is achieved at time `t`

      - Present the whole sentence to a SAT solver
        - If the solver finds a satisfying model, then the goal is achievable
        - If the sentence is unsatisfiable, then the planning problem is impossible

      - Assuming a model is found, extract from the model those variables that represent actions and are assigned `true`
        - These form a plan to achieve the goals

    - ```pseudocode
      function SATPLAN(init, transition, goal, T_max) returns a solution or failure
      	inputs: init, transition, goal, constitute a description of the problem
      			T_max, an upper limit for plan length
          
          for t = 0 to T_max do
          	cnf <- TRANSLATE-TO-SAT(init, transition, goal, t)
          	model <- SAT-SOLVER(cnf)
          	if model is not null then
          		return EXTRACT-SOLUTION(model)
          return failure
      ```

      - This variation tries each possible number of steps `t` up to some value `T_max`
        - Guaranteed to find the shortest possible solution if one exists
      - Approach cannot be used in a partially observable environment

    - `SATPLAN` is a good debugging tool for KBs since it tells us where knowledge may be missing

      - We must add precondition axioms to avoid illegal actions
      - We must carefully distinguish between entailment and satisfiability
      - We must introduce action exclusion axioms to prevent multiple simultaneous actions

    - In summary, `SATPLAN` finds models for a sentence containing the initial state, the goal, the successor state axioms, the precondition axioms, and the action exclusion axioms

      - Any model satisfying the propositional sentence will be a valid plan for the original problem

- Summary

  - Intelligent agents need knowledge about the world in order to reach good decisions
  - Knowledge is contained in agents in the form of sentences in a knowledge representation language that are stored in a knowledge base
  - A knowledge-based agent is composed of a knowledge base and an inference mechanism
    - It operates by storing sentences about the world in its knowledge base, using the inference mechanism to infer new sentences, and using these sentences to decide what action to take

  - A representation language is defined by its syntax, which specifies the structure of sentences, and its semantics, which defines the truth of each sentence in each possible world or model
  - The relationship of entailment between sentences is crucial to our understanding of reasoning
    - A sentence `α` entails another sentence `β` if `β` is true in all worlds where `α` is true
    - Equivalent definitions include the validity of the sentence `α ⇒ β` and the unsatisfiability of the sentence `α ∧ ¬β`

  - Inference is the process of deriving new sentences from old ones
    - Sound inference algorithms derive only sentences that are entailed
    - Complete algorithms derive all sentences that are entailed

  - Propositional logic is a simple language consisting of proposition symbols and logical connectives
    - It can handle propositions that are known true, known false, or completely unknown

  - The set of possible models, given a fixed propositional vocabulary, is finite, so entailment can be checked by enumerating models
    - Efficient model-checking inference algorithms for propositional logic include backtracking and local search methods and can often solve large problems quickly

  - Inference rules are patters of sound inference that can be used to find proofs
    - The resolution rule yields a complete inference algorithm for KBs that are expressed in conjunctive normal form
    - Forward chaining and backward chaining are very natural reasoning algorithms for KBs in Horn form

  - Local search methods such as `WALKSAT` can be used to find solutions
    - Such algorithms are sound but not complete

  - Logical state estimation involves maintaining a logical sentence that describes the set of possible states consistent with the observation history
    - Each update step requires inference using the transition model of the environment, which is built from successor-state axioms that specify how each fluent changes

  - Decisions within a logical agent can be made by SAT solving: finding possible models specifying future actions sequences that reach the goal
    - This approach works only for fully observable or sensorless environments

  - Propositional logic does not scale to environments of unbounded size because it lacks the expressive power to deal concisely with time, space, and universal patters of relationships among objects



## Reading 8: First-Order Logic

- Representation Revisited
  - Programming languages lack a general mechanism for deriving facts from other facts
    - Each update to a data structure is done by a domain-specific procedure whose details are derived by the programmer from their own knowledge of the domain
    - Contrasts with the declarative nature of propositional logic, in which knowledge and inference are separate and inference is entirely domain independent
  - Data structures in programming languages also lack the expressiveness required to handle partial information
  - Propositional logic is a declarative language
    - Its semantics is based on a truth relation between sentences and possible worlds
    - Has sufficient expressive power to deal with partial information using disjunction and negation
    - Has the property of compositionality
      - The meaning of a sentence is a function of the meaning of its parts
      - Non-compositionality makes life a lot more difficult for the reasoning system
    - Lacks the expressive power to concisely describe an environment
      - However, the semantics and syntax of English make this possible
  - The Language of Thought
    - Natural languages are very expressive
      - There is a long tradition in linguistics and the philosophy of language to refer to natural language as a declarative knowledge representation language
    - Natural language serves as a medium for communication rather than pure representation
      - The meaning of a sentence depends both on the sentence itself and the context in which the sentence was spoken
    - Natural languages suffer from ambiguity
    - The Sapir-Whorf hypothesis claims that our understanding of the world is strongly influenced by the language we speak
      - Leads to the suggestion that people process words to form some sort of nonverbal representation
      - Words can serve as anchor points that affect how we perceive the world
    - From the viewpoint of formal logic, representing the same knowledge in two different wats makes absolutely no difference
      - The same facts will be derivable from either representation
      - In practice, one representation might require fewer steps to derive a conclusion
      - For non-deductive tasks such as learning from experience, outcomes are necessarily dependent on the form of the representations used
        - The influence of language on thought is unavoidable for any agent that does learning
  - Combining the Best of Formal and Natural Languages
    - We can adopt the declarative, compositional semantics that are context-independent and unambiguous of propositional logic and build a more expressive logic on that foundation by borrowing from natural language while avoiding its drawbacks
    - Natural language elements:
      - Nouns and noun phrases that refer to objects
      - Verbs and verb phrases that refer to relations among objects
        - Some of these relations are functions: relations in which there is only one value for a given input
    - Almost any assertion can be thought of as referring to objects and proportions or relations
    - The language of first-order logic is built around objects and relations
      - Can express facts about some or all of the objects in the universe, allowing one to represent general laws or rules
    - The primary difference between propositional and first-order logic lies in the ontological commitment made by each language
      - What it assumes about the nature of reality
      - This commitment is expressed mathematically through the nature of the formal models with respect to which the truth of sentences is defined
        - Propositional logic assumes that there are facts that either hold or do not hold in the world
        - First-order logic assumes that the world consists of objects with certain relations among them that do or do not hold
      - Special-purpose logics make further ontological commitments
        - Temporal logic assumes that facts hold at particular times and that those times are ordered
        - Higher-order logic views the relations and functions referred to by first-order logic as objects in themselves
          - Allows one to make assertions about all relations
          - Strictly more expressive than first-order logic
      - Logic can also be characterized by its epistemological commitments
        - The possible states of knowledge that it allows with respect to each fact
        - In propositional and first-order logic, a sentence represents a fact and the agent either believes the sentence to be true, false, or unknown => 3 possible states
        - In systems using probability theory, they can have any degree of belief ranging from `0` to `1`
- Syntax and Semantics of First-Order Logic
  - Models for First-Order Logic
    - Models of a logical language are the formal structures that constitute the possible worlds under consideration
      - Each model links the vocabulary of the logical sentences to elements of the possible world, so that the truth of any sentence can be determined
      - Models for propositional logic link proposition symbols to predefined truth values

    - Models for first-order logic are more interesting
      - The domain of a model is the set of objects or domain elements it contains
        - The domain is required to be nonempty, every possible world must contain at least one object
        - Mathematically speaking, it doesn't matter what these objects are, it just matters how many there are in each particular model

      - A relation is the set of tuples or objects that are related
        - Relations may be binary or unary

      - Models require total functions, which means there must be a value for every input tuple
  - Syntax and Interpretations
    - The basic syntactic elements of first-order logic are the symbols that stand for objects, relations, and functions
      - Constant symbols stand for objects
      - Predicate symbols stand for relations
      - Function symbols stand for functions
      - Symbols begin with uppercase letters by convention
      - Naming of the symbols is decided by the user
      - Predicate and function symbols come with an arity that fixes the number of arguments

    - Each model also includes an interpretation that specifies exactly which objects, relations, and functions are referred to by the constant, predicate, and function symbols
      - Under an interpretation, objects can have no name or multiple names

    - Entailment, validity, etc. are defined in terms of all possible models
      - Models vary in the how many objects they contain and in the way the constant symbols map to objects
      - Checking entailment by the enumeration of all possible models is not feasible for first-order logic, as the number of possible models is unbounded
  - Terms
    - A term is a logical expression that refers to an object
    - It is not always convenient to have a distinct symbol to name every object => this is what function symbols are for
      - A complex term is formed by a function symbol followed by a parenthesized list of terms as arguments to the function symbol
      - Remember that a complex term is just a complicated kind of name
      - We can reason about these complex terms without ever providing a definition of the function symbol
  - Atomic Sentences
    - We can put terms that refer to objects and predicate symbols that refer to relations together to make atomic sentences that state facts
    - An atom is formed from a predicate symbol optionally followed by a parenthesized list of terms
      - These can have complex terms as arguments
    - An atom is true in a given model if the relation referred to by the predicate symbol holds among the objects referred to by the arguments
  - Complex Sentences
    - We can use logical connectives to construct more complex sentences
      - Uses the same syntax and semantics as propositional calculus
  - Quantifiers
    - Since our logic allows objects, it is natural to want to express properties of entire collections of objects instead of enumerating the objects by name => quantifiers let us do this
    - (Universal quantification (`∀`)
      - `∀` is usually pronounced "for all"
        - Ex) `∀x King(x) ⇒ Person(x)` means "for all `x`, if `x` is a king, then `x` is a person"
        - The symbol `x` is called a variable, which, by convention, are all lowercase letters
          - A variable is a term by itself, and can serve as the argument of a function
          - A term with no variables is called a ground term
      - The sentence `∀x P`, where `P` is any logical expression, says that `P` is true for every object `x`
        - More precisely, `∀x P` is true in a given model if `P` is true in all possible extended interpretations constructed from the interpretation given in the model, where each extended interpretation specifies a domain element to which `x` refers
      - By asserting the universally quantified sentence, which is equivalent to asserting a whole list of individual implications, we end up asserting the conclusion of the rule just for those objects for whom the premise is true, while saying nothing about the individuals for whom the premise is false
        - Thus, the truth-table definition of `⇒` is perfect for writing general rules with universal quantifiers
      - A common mistake is to use conjunction instead of implication
    - Existential quantification (`∃`)
      - Existential quantifiers allow us to make a statement about some object in the universe without naming it
      - `∃x` is pronounced "there exists an `x` such that"
        - Ex) `∃x Crown(x) ∧ OnHead(x, John)` means "King John has a crown on his head"
      - Intuitively, `∃x P` says that `P` is true for at least one object `x`
        - More precisely, `∃x P` is true in a given model if `P` is true in at least one extended interpretation that assigns `x` to a domain element
      - `∧` is the natural connective to use with `∃`
    - Nested Quantifiers
      - Simplest case is where quantifiers are of the same type
        - Ex) `∀x ∀y Brother (x, y) ⇒ Sibling(x, y)` means "brothers are siblings"
          - Can be written as `∀x, y Brother (x, y) ⇒ Sibling(x, y)`
      - Some cases have mixtures of quantifiers
        - Ex) `∀x ∃y Loves(x, y)` means "everybody loves somebody"
        - Ex) `∃y ∀x Loves(x, y)` means "there is someone who is loved by everyone"
        - Order of quantification is very important
    - Connections between `∀` and `∃`
      - `∀x ¬P` is equivalent to `¬∃x P`
      - `∀x P` is equivalent to `¬∃x ¬P`
    - Equality
      - We can use the equality symbol to signify that two terms refer to the same object
        - Ex) `Father(John) = Henry` means "the object referred to by `Father(John)` and `Henry` are the same"
      - Can be used to state facts about a given function
      - Can also be used with negation to insist that two terms are not the same object
    - An alternative semantics?
      - Some sentences are difficult to translate from natural language into first-order semantics
      - One popular proposal to solve this is used in database systems
        - Insist that every constant symbol refer to a distinct object, the unique-names assumption
        - Assume that atomic sentences not known to be true are in fact false, the closed-world assumption
        - Invoke domain closure, meaning that each model contains no more domain elements than those named by the constant symbols
        - These are database semantics
        - Requires definite knowledge of what the world contains
      - There is no one correct semantics for logic
        - The usefulness of any proposed semantics depends on how concise and intuitive it makes the expression of the kinds of knowledge we want to write down
- Using First-Order Logic
  - Sentences are added to a KB using `TELL`, exactly as in propositional logic
    - These sentences are called assertions

  - We can ask questions of the KB using `ASK`
    - These questions are called queries or goals
    - Any query that is logically entailed by the KB should be answered affirmatively
    - If we want to know what value of `x` makes a sentence true, we use `ASKVARS`, which yields a stream of answers
      - These answers are called substitutions or binding lists
      - Usually reserved for KBs consisting solely of Horn clauses, as in these KBs, every way of making the query true will bind the variables to specific values

  - Axioms provide the basic factual information from which useful conclusions can be derived
    - They are commonly associated with purely mathematical domains
    - Can be definitions, plain facts, or may provide more general information about certain predicates

  - Definitions define functions and predicates in terms of other predicates
    - Definitions eventually reach a basic set of predicates in terms of which the others are ultimately defined
      - This basic set is not clearly identifiable in all domains

  - Theorems are entailed by the axioms
    - From a purely logical point of view, a KB doesn't need to contain theorems, as they don't increase the set of conclusions that follow from the KB
    - From a practical point of view, theorems are essential to reducing the computational cost of deriving new sentences
      - Without them, a reasoning system has to start from first principles every time

- Knowledge Engineering in First-Order Logic
  - Knowledge engineering is the general process of knowledge base construction
  - The Knowledge-Engineering Process
    - Knowledge engineering projects vary widely in content, scope, and difficulty, but all include the following steps:
      - Identify the task
        - Delineate the range of questions that the KB will support and the kinds of facts that will be available for each specific problem instance
        - Determine what knowledge must be represented in order to connect problem instances to answers
        - Analogous to the PEAS process for designing agents

      - Assemble the relevant knowledge
        - Engage in the process of knowledge acquisition
        - Idea is to understand the scope of the KB, as determined by the task, and to understand how the domain actually works
          - Knowledge is not represented formally at this stage

        - For real domains, the issue of relevance can be difficult

      - Decide on a vocabulary of predicates, functions, and constants
        - Translate the important domain-level concepts into logic-level names
        - Involves many questions of knowledge-engineering style, which can have a significant impact on the project's success
        - Once the design choices have been made, the result is a vocabulary that is known as the ontology of the domain
          - Determines what kinds of things exist, but does not determine their specific properties and interrelationships

      - Encode general knowledge about the domain
        - Write down the axioms for all the vocabulary terms
        - Pins down the meaning of the terms, enabling the expert to check the content
        - Reveals misconceptions or gaps in the vocabulary that must be fixed

      - Encode a description of the specific problem instance
        - Involves writing simple atomic sentences about instances of concepts that are already part of the ontology
        - For a logical agent, problem instances are supplied by the sensors
        - A disembodied KB is supplied with additional sentences in the same way that traditional programs are supplied with input data

      - Pose queries to the inference procedure and get answers
        - Let the inference procedure operate on the axioms and problem-specific facts to derive the facts we're interested in knowing
        - Avoids the need for writing an application-specific solution algorithm

      - Debug the KB
        - The answers will be correct for the KB as written, but may not be the answers the user was expecting
        - Missing axioms or axioms that are too weak can be easily identified by noticing places where the chain of reasoning stops unexpectedly
        - Incorrect axioms can be identified because they are false statements about the world

- Summary
  - Knowledge representation languages should be declarative, compositional, expressive, context independent, and unambiguous
  - Logics differ in their ontological commitments and epistemological commitments
    - While propositional logic commits only to the existence of facts, first-order logic commits to the existence of objects and relations and thereby gains expressive power

  - The syntax of first-order logic builds on that of propositional logic
    - It adds terms to represent objects, and has universal and existential quantifiers to construct assertions about all or some of the possible values of the quantified variables

  - A possible world, or model, for first-order logic includes a set of objects and an interpretation that maps constant symbols to objects, predicate symbols to relations among objects, and function symbols to functions on objects
  - An atomic sentence is true just when the relation named by the predicate holds between the objects named by the terms
    - Extended interpretations, which map quantifier variables to objects in the model, define the truth of quantified sentences

  - Developing a KB in first-order logic requires a careful process of analyzing the domain, choosing the vocabulary, and encoding the axioms required to support the desired inferences


​	

## Reading 9: Inference in First-Order Logic

- Propositional vs. First-Order Inference

  - First-order inference can be done by converting the KB to propositional logic and using propositional inference

  - Inference Rules for Quantifiers

    - The rule of Universal Instantiation (UI) says that we can infer any sentence obtained by substituting a ground term (a term without variables) for the variable in a universal quantifier

      - $$
        \frac{\forall v\quad\alpha}{\texttt{SUBST}(\{v/g\},\alpha)}
        $$

      - Can be applied many times to produce many different consequences

    - The rule of Existential Instantiation says that we can infer any sentence obtained by replacing the variable with a new constant symbol that doesn't appear anywhere else in the KB

      - $$
        \frac{\exists v\quad\alpha}{\texttt{SUBST}(\{v/k\},\alpha)}
        $$

      - This new name is called a Skolem constant

      - EI is a special case of a more general process called skolemization

      - Can be applied once, and then the existentially quantified sentence can be discarded

        - New KB is inferentially equivalent in the sense that it is satisfiable exactly when the original KB is satisfiable

  - Reduction to Propositional Inference

    - Just as an existentially quantified sentence can be replaced by one instantiation, a universally quantified sentence can be replaced by the set of all possible instantiations
      - Once complete, the KB is essentially propositional if we view the ground atomic sentences as proposition symbols

    - This technique of propositionalization can be made completely general
      - Every first-order KB and query can be propositionalized in such a way that entailment is preserved

    - Issue with function symbols
      - If the KB includes a function symbol, then infinitely many nested terms can be constructed
      - If a sentence is entailed by the original, first-order KB, then there exists a proof involving just a finite subset of the propositionalized KB
        - Any such subset has a maximum depth of nesting among its ground terms
        - Find the subset by first generating all the instantiations with constant symbols, then all terms of depth 1, depth 2, etc. until we are able to construct a propositional proof of the entailed sentence

    - This approach is complete - any entailed sentence can be proved
      - Proof procedure continues until this is proved
      - If the sentence is not entailed, we cannot know, the procedure will just infinitely generate nested terms
      - The question of entailment for first-order logic is semidecidable, algorithms exist that say yes to every entailed sentence, but no algorithm exists that also says no to every nonentailed sentence

- Unification and Lifting

  - A First-Order Inference Rule

    - The inference process can be captured as a single inference rule that we call Generalized Modus Ponens

      - For atomic sentences `p_i`, `p_i'`, and `q`, where there is a substitution `θ` such that `SUBST(θ, p_i') = SUBST(θ, p_i)` for all `i`:

        - $$
          \frac{p_1',p_2',...,p_n',\quad(p_1\land p_2\land\ ...\ \land p_n\Rightarrow q)}{\texttt{SUBST}(\theta,q)}
          $$

      - Lifted version of Modus Ponens

        - Raises Modus Ponens from ground (variable-free) propositional logic to first-order logic
        - Key advantage is that lifted inference rules make only substitutions that are required to allow particular inferences to proceed

  - Unification

    - Lifted inference rules require finding substitutions that make different logical expressions look identical

      - Process is called unification

    - The `UNIFY` algorithm takes two sentences and returns a unifier for them if one exists:

      - $$
        \texttt{UNIFY}(p,q)=\theta\text{ where }\texttt{SUBST}(\theta,p)=\texttt{SUBST}(\theta,q)
        $$

      - Requires standardizing apart one of the sentences being unified

        - Renaming variables to avoid name clashes

      - For every pair of expressions, there is a single most general unifier that is unique up to renaming and substitution of variables

        - A unifier is more general if it places fewer restrictions on the values of the variables

    - ```pseudocode
      function UNIFY(x, y, θ) returns a substitution to make x and y identical
      	inputs: x, a variable, constant, list, or compound expression
      			y, a variable, constant, list, or compound expression
      			θ, the substitution built up so far (optional, defaults to empty)
      			
          if θ = failure then return failure
          else if x = y then return θ
          else if VARIABLE?(x) then return UNIFY-VAR(x, y, θ)
          else if VARIABLE?(y) then return UNIFY-VAR(y, x, θ)
          else if COMPOUND?(x) and COMPOUND?(y) then
          	return UNIFY(x.ARGS, y.ARGS, UNIFY(x.OP, y.OP, θ))
          else if LIST?(x) and LIST?(y) then
          	return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, θ))
          else return failure
          
      function UNIFY-VAR(var, x, θ) returns a substitution
      	if { var/val } ∈ θ then return UNIFY(val, x, θ)
      	else if { x/val } ∈ θ then return UNIFY(var, val, θ)
      	else if OCCUR-CHECK?(var, x) then return failure
      	else return add { var/x } to θ
      ```

      - Recursively explore the two expressions simultaneously, building up a unifier along the way and failing if two corresponding points in the structure don't match
      - When matching a variable against a complex term, one must check whether the variable itself occurs inside the term
        - If it does, match fails because no consistent unifier can be constructed
        - Called the occur check
        - Makes the algorithm quadratic in the size of the expressions being unified

  - Storage and Retrieval

    - `STORE(s)` stores a sentence `s` into the KB
    - `FETCH(q)` returns all unifiers such that the query `q` unifies with some sentence in the KB
    - Simplest implementation of `STORE` and `FETCH` involves keeping all the facts in one long list and unifying each query against every element of the list

- Forward Chaining

  - Idea: start with the atomic sentences in the KB and apply Modus Ponens in the forward direction, adding new atomic sentences, until no further inferences can be made

  - First-Order Definite Clauses

    - Closely resemble propositional definite clauses
    - Disjunctions of literals of which exactly one is positive
      - Either atomic or an implication whose antecedent is a conjunction of positive literals and whose consequent is a single positive literal

    - Examples:
      - `King(x) ∧ Greedy(x) ⇒ Evil(x)`
      - `King(John)`
      - `Greedy(y)`

    - Can include variables, in which case those variables are assumed to be universally quantified
    - Not every KB can be converted into a set of definite clauses because of the single-positive-literal restriction, but many can

  - A Simple Forward-Chaining Algorithm

    - ```pseudocode
      function FOL-FC-ASK(KB, α) returns a substitution or false
      	inputs: KB, the knowledge base, a set of first-order definite clauses
      			α, the query, an atomic sentence
          local variables: new, the new sentences inferred on each iteration
          
          repeat until new is empty
          	new <- {}
          	for each rule in KB do
          		(p_1 ∧ ... ∧ p_n ⇒ q) <- STANDARDIZE-VARIABLES(rule)
          		for each θ such that SUBST(θ, p_1 ∧ ... ∧ p_n) =
          				SUBST(θ, p_1’ ∧ ... ∧ p_n’) for some p_1’, ... , p_n’ in KB
                      q’ <- SUBST(θ, q)
                      if q’ does not unify with some sentence already in KB or new then
                      	add q’ to new
                      	φ <- UNIFY(q’, α)
                      	if φ is not fail then return φ
              add new to KB
          return false
      ```

      - Starts from the known facts
      - Triggers all the rules whose premises are satisfied, adding their conclusions to the known facts
      - Repeats until the query is answered or no new facts are added
      - Note that a fact is not "new" if it is just a renaming of an known fact
        - One sentence is a renaming of another if they are identical except for the names of the variables
      - Once finished, no new inferences are possible because every sentence that could be concluded by forward chaining is already contained explicitly in the KB
        - Called a fixed point of the inference process
        - First-order fixed points can include universally quantified atomic sentences
      - Sound because every inference is just an application of Generalized Modus Ponens, which is sound
      - Complete for definite clause KBs
        - For general definite clauses with function symbols, `FOL-FC-ASK` can generate infinitely many new facts
        - Semidecidable

  - Efficient Forward Chaining

    - Prior algorithm is inefficient for the following reasons:
      - Inner loop involves finding all possible unifiers such that the premise of a rule unifies with a suitable set of facts in the KB
        - Called pattern matching and can be very expensive

      - Rechecks every rule on every iteration to see whether its premises are satisfied, even if very few additions are made to the KB on each iteration
      - Might generate many facts that are irrelevant to the goal

    - Matching rules against known facts
      - The conjunct ordering problem is to find an ordering to solve the conjuncts of the rule premise so that the total cost is minimized
        - Optimal ordering is NP-hard, but good heuristics are available
          - Ex) Minimum-remaining-values heuristic

      - Connection with constraint satisfaction is close
        - We can express every finite domain CSP as a single definite clause together with some associated ground facts
        - Allows us to conclude that matching a definite clause against a set of facts is NP-hard

      - Next steps:
        - Common in the database world to assume that both the sizes of rules and the arities of predicates are bounded by a constant and to worry only about data complexity
          - The complexity of inference as a function of the number of ground facts in the KB
          - Easy to show that the data complexity of forward chaining is polynomial

        - Consider subclasses of rules for which matching is efficient
        - Eliminate redundant rule-matching attempts in the forward-chaining algorithm

    - Incremental forward chaining
      - Every fact inferred on iteration `t` must be derived from at least one new fact inferred on iteration `t - 1`
        - Allows for the avoiding of redundant rule matching

      - Observation leads to an incremental forward-chaining algorithm where, at iteration `t`, we check a rule only if its premise includes a conjunct `p_i` that unifies with a fact `p_i'` newly inferred at iteration `t - 1`
        - Then fixes `p_i` to match with `p_i'`, but allows the other conjuncts of the rule to match with facts from any previous iteration
        - Generates exactly the same facts at each iteration, but is much more efficient

      - With suitable indexing, it is easy to identify all the rules that can be triggered by any given fact
        - Inferences cascade through the set of rules until the fixed point is reach, then begin the process again for the next new fact

      - A great deal of redundant work is done in repeatedly constructing partial matches that have some unsatisfied premises
        - Would be better to retain and gradually complete the partial matches as new facts arrive
        - Addressed by the rete algorithm
          - Preprocesses the set of rules in the KB to construct a dataflow network in which each node is a literal from a rule premise
          - Variable bindings flow through the network and are filtered out when they fail to match a literal
          - At any given point, the state of a rete network captures all the partial matches of the rules, avoiding a great deal of recomputation

    - Irrelevant facts
      - Problem arises from the fact that forward chaining makes all allowable inferences based on the known facts, even if they are irrelevant to the goal at hand
      - One solution is to use backward chaining
      - Another solution is to restrict forward chaining to a selected subset of rules
      - A third approach emerged from the field of deductive databases
        - Rewrite the rule set, using information from the goal, so that only relevant variable bindings are considered during forward chaining

- Backward Chaining

  - Algorithms work backward from the goal, chaining through rules to find known facts that support the proof

  - A Backward-Chaining Algorithm

    - ```pseudocode
      function FOL-BC-ASK(KB, query) returns a generator of substitutions
      	return FOL-BC-OR(KB, query, {})
      	
      generator FOL-BC-OR(KB, goal, θ) yields a substitution
      	for each rule (lhs ⇒ rhs) in FETCH-RULES-FOR-GOAL(KB, goal) do
      		(lhs, rhs) <- STANDARDIZE-VARIABLES((lhs, rhs))
      		for each θ’ in FOL-BC-AND(KB, lhs, UNIFY(rhs, goal, θ)) do
      			yield θ’
      			
      generator FOL-BC-AND(KB, goals, θ) yields a substitution
      	if θ = failure then return
      	else if LENGTH(goals) = 0 then yield θ
      	else do
      		first, rest <- FIRST(goals), REST(goals)
      		for each θ’ in FOL-BC-OR(KB, SUBST(θ, first), θ) do
      			for each θ’’ in FOL-BC-AND(KB, rest, θ’) do
      				yield θ’’
      ```

      - A generator is a function that returns multiple times, each time giving one possible result
      - `FOL-BC-OR` fetches all clauses that might unify with the goal, standardizes the variables in the clause to be brand-new variables, and then, if the `rhs` of the clause unifies with the goal, proves all the conjuncts in the `lhs` using `FOL-BC-AND`
      - `FOL-BC-AND` works by proving each of the conjuncts in turn, keeping track of the accumulated substitution as it goes

    - Backward chaining is a kind of and/or search

      - The or part because the goal query can be proved by any rule in the KB
      - The and part because all the conjuncts in the `lhs` of a clause must be proved
      - Clearly a DFS algorithm
        - Space requirements are linear in the size of the proof
        - Suffers from problems of repeated states and incompleteness

- Resolution

  - Conjunctive Normal Form for First-Order Logic

    - First-order resolution requires that sentences be in conjunctive normal form
      - A conjunction of clauses, where each clause is a disjunction of literals
      - Literals can contain variables, which are assumed to be universally quantified

    - Every sentence of first-order logic can be converted into an inferentially equivalent CNF sentnece
      - The CNF sentence will be unsatisfiable when the original sentence is unsatisfiable

    - Conversion process:
      - Eliminate implications
      - Move `¬` inwards
        - `¬∀x p` becomes `∃x ¬p`
        - `¬∃x p` becomes `∀x ¬p`

      - Standardize variables
        - For sentences which use the same variable name twice, change the name of one of the variables

      - Skolemize
        - Skolemization is the process of removing existential quantifiers by elimination
        - General rule is that the arguments of the Skolem function are all the universally quantified variables in whose scope the existential quantifier appears

      - Drop universal quantifiers
      - Distribute `∨` over `∧`

  - The Resolution Inference Rule

    - While propositional literals are complementary if one is the negation of the other, first-order literals are complementary if one unifies with the negation of the other

    - $$
      \frac{l_1\lor\ ...\ \lor l_k,\quad m_1\lor\ ...\ \lor m_n}{\texttt{SUBST}(\theta,l_1\lor\ ...\ \lor l_{i-1}\lor l_{i+1}\lor\ ...\ \lor l_k\lor m_1\lor\ ...\ \lor m_{j-1}\lor m_{j+1}\lor\ ...\ \lor m_n)}
      $$

      - `UNIFY(l_i, ¬m_j) = θ`

    - Resolution is complete

  - Equality

    - None of the approaches discusses thus far can handle the notion of equality

    - One solution is to axiomize equality

      - To write down sentences about the equality relation in the KB
      - Need to say equality is reflexive, symmetric, and transitive
      - This method leads to the generation of a lot of conclusions, most of them not helpful to a proof

    - An alternative is to add inference rules rather than axioms

      - The simplest rule, demodulation, takes a unit clause `x = y` and some clause `α` that contains the term `x`, and yields a new clause formed by substituting `y` for `x` within `α`

        - Works if the term within `α` unifies with `x`; it need not be exactly equal to `x`

        - Note that this process is direction; given `x = y`, the `x` always gets replaced with `y`

        - For any terms `x`, `y`, and `z`, where `z` appears somewhere in literal `m` and where `UNIFY(x, z) = θ`:

          - $$
            \frac{x=y,\quad m_1\lor\ ...\ \lor m_n}{\texttt{SUB}(\texttt{SUBST}(\theta,x),\texttt{SUBST}(\theta,y),m_1\lor\ ...\ \lor m_n)}
            $$

            - `SUBST` is the usual substitution of a binding list and `SUB(x, y, m)` means to replace `x` with `y` everywhere that `x` occurs within `m`

      - Demodulation can be extended to handle non-unit clauses in which an equality literal occurs in a process called paramodulation

        - For any terms `x`, `y`, and `z`, where `z` appears somewhere in literal `m_i`, and where `UNIFY(x, z) = θ`:

          - $$
            \frac{l_1\lor\ ...\ \lor l_k\lor x=y,\quad m_1\lor\ ...\ \lor m_n}{\texttt{SUB}(\texttt{SUBST}(\theta,x),\texttt{SUBST}(\theta,y),\texttt{SUBST}(\theta,l_1\lor\ ...\ \lor l_k\lor m_1\lor\ ...\ \lor m_n))}
            $$

        - Yields a complete inference procedure for first-order logic with equality

    - A third approach handles equality reasoning entirely within an extended unification algorithm

      - Terms are unifiable if they are provably equal under some substitution, where "provably" allows for equality reasoning
      - Equational unification of this kind can be done with efficient algorithms designed for the particular axioms used rather than through explicit inference with those axioms

  - Resolution Strategies

    - While repeated applications of the resolution inference rule will eventually find a proof if one exists, we want to examine strategies that help find proofs efficiently
    - Unit preference
      - Prefer to do resolutions where one of the sentences is a single literal (also known as a unit clause)
      - Since we're trying to produce an empty clause, it might be a good idea to prefer inferences that produce shorter clauses
      - Unit resolution is a restricted form of resolution in which every resolution step must involve a unit clause
        - Incomplete in general, but complete for Horn clauses

    - Set of support
      - May be more effective to try and eliminate some potential resolutions altogether
      - Insist that every resolution step involve at least one element of a special set of clauses, called the set of support
        - The resolvent is then added to the set of support

      - If the set of support is small relative to the whole KB, then the search space will be reduced dramatically
      - Bad choice for the set of support will make the algorithm incomplete

    - Input resolution
      - Every resolution combines one of the input sentences (from the KB or query) with some other sentence
      - In Horn KBs, Modus Ponens is a kind of input resolution strategy since it combines an implication from the original KB with some other sentences
      - Complete for KBs in Horn form, but incomplete in the general case
      - Linear resolution is a slight generalization that allows `P` and `Q` to be resolved together either if `P` is in the original KB or if `P` is an ancestor of `Q` in the proof tree
        - Complete

    - Subsumption
      - Eliminates all sentences that are subsumed by (more specific than) an existing sentence in the KB
      - Helps keep the KB small and thus helps keep the search space small

- Summary

  - A first approach uses inference rules (universal instantiation and existential instantiation) to propositionalize the inference problem
    - Typically, this approach is slow, unless the domain is small

  - The use of unification to identify appropriate substitutions for variables eliminates the instantiation step in first-order proofs, making the process more efficient in many cases
  - A lifted version of Modus Ponens uses unification to provide a natural and powerful inference rule, generalized Modus Ponens
    - The forward-chaining and backward-chaining algorithms apply this rule to sets of definite clauses

  - Generalized Modus Ponens is complete for definite clauses, although the entailment problem is semidecidable
    - For Datalog KBs consisting of function-free definite clauses, entailment is decidable

  - Forward chaining is used in deductive databases, where it can be combined with relational database operations
    - It is also used in production systems, which perform efficient updates with very large rule sets
    - Forward chaining is complete for Datalog and runs in polynomial time

  - Backward chaining is used in logic programming systems, which employ sophisticated compiler technology to provide very fast inference
    - Backward chaining suffers from redundant inferences and infinite loops; these can be alleviated with memoization

  - Prolog, unlike first-order logic, uses a closed world with the unique names assumption and negation as failure
    - These make Prolog a more practical programming language, but bring it further from pure logic

  - The generalized resolution inference rule provides a complete proof system for first-order logic, using KBs in conjunctive normal form
  - Several strategies exist for reducing the search space of a resolution system without compromising completeness
    - One of the most important issues is dealing with equality; we showed how demodulation and paramodulation can be used

  - Efficient resolution-based theorem provers have been used to prove interesting mathematical theorems and to verify and synthesize software and hardware




## Reading 10: Planning and Acting in the Real World

- Time, Schedules, and Resources

  - The classical planning representation talks about what to do, and in what order

    - It cannot talk about time

  - Need new methods to handle scheduling and resource constraints

    - Approach is to "plan first, schedule later"
    - Divide problem into a planning phase in which actions are selected, followed by a scheduling phase in which temporal information is added to the plan to ensure it meets resource/deadline constraints

  - Representing Temporal and Resource Constraints

    - A typical job-shop scheduling problem consists of a set of jobs, each of which consists of a collection of actions with ordering constraints among them
      - Each action has a duration and a set of resource constraints required by the action
      - Each constraint specifies a type of resource, the number of that resource required, and whether that resource is reusable or consumable
      - Resources can be produced by actions with negative consumption

    - A solution to a job-shop scheduling problem must specify the start times for each action and satisfy all the temporal ordering constraints and resource constraints
      - Solutions can be evaluated by a cost function, which can be quite complicated
      - For simplicity, we assume that the cost function is just the total duration of the plan, called the makespan

    - Resources are represented as numerical quantities rather than as named entities, in a technique called aggregation
      - Central idea is to group individual objects into quantities when the objects are all indistinguishable with respect to the purpose at hand
      - Essential for reducing complexity

  - Solving Scheduling Problems

    - Begin by considering just the temporal scheduling problem, ignoring resource constraints

      - To minimize makespan, we must find the earliest start times for all the actions consistent with the ordering constraints supplied with the problem

        - View these ordering constraints as a directed graph relating the actions

      - Apply the critical path method to the graph to determine the possible start and end times of each action

        - A path through a graph representing a partial-order plan is a linearly ordered sequence of actions

        - The critical path is the path whose total duration is longest

          - Determines the duration of the entire plan
          - Delaying the start of any action on the critical path slows down the whole plan

        - Actions that are off the critical path have the window of time in which they can be executed

          - Specified in terms of earliest start time and latest start time
            - `LS - ES` is known as the slack of an action
            - Together, the `ES` and `LS` times for all the actions constitute a schedule for the problem

        - ```pseudocode
          ES(Start) = 0
          ES(B) = max_A≺B ES(A) + Duration(A)
          LS(Finish) = ES(Finish)
          LS(A) = min_B≻A LS(B) − Duration(A)
          ```

          - `A ≺ B` means that `A` comes before `B`
          - Start by assigning `ES(Start)` to be `0`
          - As soon as we get an action `B` such that all the actions that come immediately before `B` have `ES` values assigned, we set `ES(B)` to be the maximum of the earliest finish times
            - The earliest finish time of an action is defined by its earliest start time plus its duration
            - `LS` values are computed in a similar manner, working backwards from the `Finish` action
          - Complexity is `O(Nb)`
            - `N` is the number of actions
            - `b` is the maximum branching factor into or out of an action

        - Easy to solve because they are defined as a conjunction of linear inequalities on the start and end times

          - Resource constraints introduce disjunctions, which makes the problem NP-hard

    - Complexity of scheduling with resource constraints is often seen in practice and theory

      - A simple heuristic is the minimum slack algorithm
        - On each iteration, schedule for the earliest possible start whichever unscheduled action has all its predecessors scheduled and has the least slack
        - Then update the `ES` and `LS` times for each affected action and repeat
        - Resembled the MRV heuristic in CSPs

    - We have assumed that the set of actions and ordering constraints is fixed

      - Under these assumptions, every scheduling problem can be solved by a nonoverlapping sequence that avoids all resource conflicts, provided that each action is feasible by itself
      - May be better to reconsider the actions and constraints, in case that leads to a much easier scheduling problem
        - Makes sense to integrate planning and scheduling by taking into account durations and overlaps during the construction of a partial-order plan

- Hierarchical Planning

  - Hierarchical decomposition is an idea that manages complexity

  - High-Level Actions

    - The basic formalism we adopt to understand hierarchical decomposition comes from the area of hierarchical task networks
    - We assume full observability and determinism and the availability of a set of actions, now called primitive actions
      - The key additional concept is the high-level action
    - Each HLA has one or more possible refinements into a sequence of actions, each of which may be an HLA or a primitive action
    - High-level actions and their refinements embody knowledge about how to do things
    - An HLA refinement that contains only primitive actions is called an implementation of the HLA
      - An implementation of a high-level plan (a sequence of HLAs) is the concatenation of implementations of each HLA in the sequence
      - A high-level plan achieves the goal from a given state if at least one of its implementations achieves the goal from that state
    - Simplest case is an HLA that has exactly one implementation
      - In this case, we can compute the preconditions and effects of the HLA from those of the implementation and then treat the HLA exactly as if it were a primitive action itself
    - When HLAs have multiple possible implementations, there are two options:
      - One is to search among the implementations for one that works
      - The other is to reason directly about the HLAs
        - Enables the derivation of provably correct abstract plans, without the need to consider their implementations

  - Searching for Primitive Solutions

    - HTN planning is often formulated with a single "top level" action called `Act`, where the aim is to find an implementation of `Act` that achieves the goal

    - ```pseudocode
      function HIERARCHICAL_SEARCH(problem, hierarchy) returns a solution, or failure
      	frontier <- a FIFO queue with [Act] as the only element
      	loop do
      		if EMPTY?(frontier) then return failure
      		plan <- POP(frontier) // chooses the shallowest plan in frontier
      		hla <- the first HLA in plan, or null if none
      		prefix, suffix <- the action subsequences before and after hla in plan
      		outcome <- RESULT(problem.INITIAL-STATE, prefix)
      		if hla is null then // so plan is primitive and outcome is its result
      			if outcome satisfies problem.GOAL then return plan
              else for each sequence in REFINEMENTS(hla, outcome, hierarchy) do
              	frontier <- INSERT(APPEND(prefix, sequence, suffix), frontier)
      ```

      - Repeatedly choose an HLA in the current plan and replace it with one of its refinements, until the plan achieves the goal

    - Explores the space of sequences that conform to the knowledge contained in the HLA library about how things are to be done

      - A great deal of knowledge can be encoded in the action sequences specified in each refinement, as well as in the preconditions for the refinements

    - The key to HTN planning is the construction of a plan library containing known methods for implementing complex, high-level actions

      - One method to constructing the library is to learn the methods from problem-solving experience
        - The agent can become more and more competent over time as new methods are built on top of old methods
        - One important aspect of this learning process is the ability to generalize the methods that are constructed, eliminating detail that is specific to the problem instance and just keeping the key elements of the plan

  - Searching for Abstract Solutions

    - The algorithm in the previous section forces us to refine HLAs all the way down to primitive action sequences, which seems like overkill

      - The solution seems to be to write precondition-effect descriptions of the HLAs and then use those descriptions to prove that the high-level plan achieves the goal
      - For this method to work, it must be the case that every high-level plan that claims to achieve the goal does in fact achieve the goal
        - Property is called the downward refinement property for HLA descriptions
        - If every description is true, then this requirement is satisfied
        - Problem arises when an HLA has multiple implementations

    - One way to describe HLAs with multiple implementations is to include only the positive effects that are achieved by every implementation and the negative effects of any implementation

      - Assumes that an adversary would be the one choosing the implementation => demonic nondeterminism

    - Angelic nondeterminism is when the agent itself makes the choices

      - The basic concept required for understanding angelic semantics is the reachable set of an HLA

        - Given a state `s`, the reachable set for an HLA `h`, written as `REACH(s, h)` is the set of states reachable by any of the HLA's implementations

      - Key idea is that the agent can choose which element of the reachable set it ends up in when it executes the HLA

        - An HLA with more refinements is more powerful than an HLA with fewer refinements

      - A high-level plan achieves the goal if its reachable set intersects with the set of goal states

        - If the reachable set doesn't intersect the goal, then the plan definitely works

      - In many cases, we can only approximate the effects of variables on the effects of an HLA

        - An HLA may have infinitely many implementations and may produce arbitrarily wiggly reachable sets
        - Use two kinds of approximations:
          - An optimistic description of an HLA may overstate the reachable set
          - A pessimistic description of an HLA may understate the reachable set

      - ```pseudocode
        function ANGELIC-SEARCH(problem, hierarchy, initialPlan) returns solution or fail
        	frontier <- a FIFO queue with initialPlan as the only element
        	loop do
        		if EMPTY?(frontier) then return fail
        		plan <- POP(frontier) // chooses the shallowest node in frontier
        		if REACH+(problem.INITIAL-STATE, plan) intersects problem.GOAL then
        			if plan is primitive then return plan
        			guaranteed <- REACH-(problem.INITIAL-STATE, plan) ∩ problem.GOAL
        			if guaranteed != {} and MAKING-PROGRESS(plan, initialPlan) then
        				finalState <- any element of guaranteed
        				return DECOMP(hierarchy, problem.INITIAL-STATE, plan, finalState)
        			hla <- some HLA in plan
        			prefix,suffix <- the action subsequences before and after hla in plan
        			for each sequence in REFINEMENTS(hla, outcome, hierarchy) do
        				frontier <- INSERT(APPEND(prefix, sequence, suffix), frontier)
        				
        function DECOMP(hierarchy, s_o, plan, s_f) returns a solution
        	solution <- an empty plan
        	while plan is not empty do
        		action <- REMOVE-LAST(plan)
        		s_i <- a state in REACH-(s_o, plan) such that s_f ∈ REACH-(s_i, action)
        		problem <- a problem with INITIAL-STATE = s_i and GOAL = s_f
        		solution <- APPEND(ANGELIC-SEARCH(problem, hierarchy, action), solution)
        		s_f <- s_i
            return solution
        ```

        - Can detect plans that will and won't work by checking the intersections of the optimistic and pessimistic reachable sets with the goal
        - When a workable abstract plan is found, the algorithm decomposes the original problem into subproblems, one for each step of the plan
          - The initial state and goal of each subproblem are obtained by regressing a guaranteed-reachable goal state through the action schemas for each step of the plan
        - The ability to commit to or reject high-level plans can give `ANGELIC-SEARCH` a significant computation advantage over `HIERARCHICAL-SEARCH`

      - Can be extended to find least-cost solutions by generalizing the notion of reachable set

        - Instead of a state being reachable or not, it has a cost for the most efficient way to get there
          - `INF` for unreachable states

        - Allows angelic search to find provably optimal abstract plans without considering their implementations
        - Same approach can be used to obtain effective hierarchical lookahead algorithms for online search

  - Planning and Acting in Nondeterministic Domains

    - To solve a partially observable problem, the agent will have to reason about the percepts it will obtain when it is executing the plan

      - The percept will be supplied by the agent's sensors when it is actually acting, but when it is planning it will need a model of its sensors

    - Sensorless Planning

      - Main idea is the conversion of a sensorless planning problem to a belief-state planning problem

        - Underlying physical transition model is represented by a collection of action schemas
        - The belief state can be represented by a logical formula instead of an explicitly enumerated set of states
        - Assume that the underlying planning problem is deterministic

      - In classical planning, where the closed-world assumption is made, we assume that any fluent not mentioned in the state is false

        - In sensorless planning, we have to switch to an open-world assumption in which states contain both positive and negative fluents
          - If a fluent doesn't appear, its value is unknown
        - The belief state corresponds directly to the set of possible worlds that satisfy the formula

      - Note that in a given belief state `b`, the agent can consider any action whose preconditions are satisfied by `b`

        - The general formula for updating the belief state `b` given an applicable actions `a` in a deterministic world is as follows:

          - $$
            b'=\texttt{RESULT}(b,a)=\{s':s'=\texttt{RESULT}_P(s,a)\text{ and }s\in b\}
            $$

          - `RESULT_P` defines the physical transition model

        - Assume that the initial belief state is always a conjunction of literals (a 1-CNF formula)

          - To construct a new belief state `b'`, we must consider what happens to each literal `l` in each physical state `s` in `b` when action `a` is applied

          - For literals whose truth value is already known in `b`, the truth value in `b'` is computed from the current value and the add list and delete list of the action

          - For literals whose truth value is unknown in `b`, there are three cases:

            - If the action adds `l`, then `l` will be true in `b'` regardless of initial state

            - If the action deletes `l`, then `l` will be false in `b'` regardless of its initial state

            - If the action doesn't affect `l`, then `l` will retain its initial value (which is unknown) and will not appear in `b'`

            - The calculation of `b'` is almost identical to the observable case:

              - $$
                b'=\texttt{RESULT}(b,a)=(b-\texttt{DEL}(a))\cup\texttt{ADD}(a)
                $$

      - The family of belief states defined as conjunctions of literals is closed under updates defined by PDDL action schemas

        - If the belief state starts as a conjunction of literals, then any update will yield a conjunction of literals
        - In a world with `n` fluents any belief state can be represented by a conjunction of size `O(n)`
          - We can compactly represent all the subsets of the possible `2^n` states we will ever need

      - Issue with this approach is that it only works for action schemas that have the same effects for all states in which their preconditions are satisfied

        - Once the effect can depend on the state, dependencies are introduced between fluents and the 1-CNF property is lost
        - For such actions, out action schemas need the addition of a conditional effect
          - `when condition: effect`
            - `condition` is a logical formula to be compared against the current state
            - `effect` is a formula describing the resulting state
        - Can introduce arbitrary dependencies among the fluents in a belief state, leading to belief states of exponential size in the worst case
        - Important to understand the difference between preconditions and conditional effects
          - All conditional effects whose conditions are satisfied have their effects applied to generate the resulting state
            - If none are satisfied, then the resulting state is unchanged
          - If a precondition is unsatisfied, then the action is inapplicable and the resulting state is undefined

      - Seems inevitable that nontrivial problems will involve wiggly belief states

        - One possible solution is to use a conservative approximation to the exact belief state
          - Approach is sound in that it never generates an incorrect plan
          - Incomplete because it may be unable to find solutions
        - A possibly better solution is to look for action sequences that keep the belief state as simple as possible
          - Keep performing little actions to eliminate uncertainty and keep our belief state manageable (in 1-CNF preferably)
        - Another approach is to not bother computing the wiggly belief state at all
          - Suffers from the drawback of determining whether the goal is satisfied or if an action is applicable

      - Final piece of the sensorless planning puzzle is to introduce a heuristic function to guide the search

        - An estimate of the cost of achieving the goal from the given belief state

        - Solving any subset of a belief state is necessarily easier than solving the belief state:

          - $$
            \text{if }b_1\subseteq b_2\text{ then }h^*(b_1)\le h^*(b_2)
            $$

    - Contingent Planning

      - The generation of plans with conditional branching base on percepts

        - Appropriate for environments with partial observability, nondeterminism, or both

      - Variables in such a plan should be considered existentially quantified

      - When executing such a plan, a contingent-planning agent can maintain its belief state as a logical formula and evaluate each branch condition by determining if the belief state entails the condition formula or its negation

        - Calculating the new belief state after an action and subsequent percept is done in two stages:

          - First stage calculates the belief state after the action:

            - $$
              \hat{b}=(b-\texttt{DEL}(a))\cup\texttt{ADD}(a)
              $$

          - Second stage adds percept axioms to the belief state for received percept literals

            - If a percept `p` has exactly one percept axiom, `Percept(p, PRECOND: c)`, where `c` is a conjunction of literals, then those literals can be thrown into the belief state along with `p`
            - If `p` has more than one percept axiom whose preconditions might hold according to the predicted belief state `b_hat`, then we have to add in the disjunction of the preconditions
              - Takes the belief state outside 1-CNF

      - Given a mechanism for computing exact or approximate belief states, we can generate contingent plans with an extension of the `AND-OR` forward search over belief states

        - Actions with nondeterministic effects can be accommodated with minor changes to the belief-state update calculation and no change to the search algorithm

    - Online Replanning

      - Replanning presupposes some form of execution monitoring to determine the need for a new plan
      - One such need arises when a contingent planning agent gets tired of planning for every little contingency => some branches of a partially constructed contingent plan can simply say `Replan`
      - The decision as to how much of the problem to solve in advance and how much to leave to replanning is one that involves tradeoffs among possible events with different costs and probabilities of occurring
      - May also be needed if the agent's model of the world is incorrect
        - The model for an action may have a missing precondition, missing effect, or missing state variable
        - May also lack provision for exogenous events, possibly ones that change the agent's goal
        - Agent's behavior is likely to be extremely fragile if it relies on absolute correctness of the world
      - The online agent has three levels of choice in how to carefully monitor the environment
        - Action monitoring: before executing an action, the agent verifies that all the preconditions still hold
          - May lead to unintelligent behavior
          - Easy to enable
        - Plan monitoring: before executing an action, the agent verifies that the plan will still succeed
          - Cuts off execution of a doomed plan as soon as possible, rather than continuing until the failure actually occurs
          - Allows for serendipity (accidental success)
          - More complex to enable
        - Goal monitoring: before executing an action, the agent checks to see if there is a better set of goals it could be trying to achieve
      - Trouble occurs when an action is actually not nondeterministic, but actually relies on some precondition that the agent doesn't know about
        - One solution is to choose randomly from the set of possible repair plans
        - A better solution is to learn a better model
          - Every prediction failure is an opportunity for learning
          - An agent should be able to modify its model of the world to accord with its percepts
          - Allows the replanner to come up with a repair that gets at the root problem

  - Multiagent Planning

    - In a multiagent planning problem, each agent tries to achieve its own goals with the help or hinderance of others
    - Wide spectrum of problems
      - An agent with multiple effectors that can operate concurrently needs to do multieffector planning to manage each effector while handling their negative and positive interactions
      - When effectors are physically decoupled into detached units, multieffector planning becomes multibody planning
        - Still a standard single-agent problem if the relevant sensor information collected by each body can be pooled
      - When communication constraints make pooling of information impossible, we have a decentralized planning problem
        - The subplan constructed for each body may need to include explicit communicative actions with other bodies
      - When a single entity is doing the planning, there is really only one goal, which all the bodies necessarily share
        - When the bodies are distinct agents that do their own planning, they may still share the same goal
        - In a multiagent case, each agent decides what to do, resulting in a need for corrdination
      - Clearest case of of multiagent problem is when the agents have different goals
      - Some systems are a mixture of centralized and multiagent planning
        - Goals can be brought into alignment by incentives
    - Planning with Multiple Simultaneous Actions
      - Treat multieffector, multibody, and multiagent settings in the same way, labelling them as multiactor settings
        - The generic term of "actor" covers effectors, bodies, and agents
      - Work out how to define transition models, correct plans, and efficient planning algorithms for the multiactor setting
        - A correct plan is one that, if executed by the actors, achieves the goal
        - Assume perfect synchronization: each action takes the same amount of time and actions at each point in the joint plan are simultaneous
      - In the multiactor setting's transition model, a single action `a` is replaced by a joint action, which combines the actions of all actors
        - Results in a joint planning problem with a branching factor of `b^n`
        - Principal focus is to decouple the actors so that the problem comes closer to growing linearly
          - Actors that are completely independent can be treated as separate problems
          - Loosely couple actors can achieve close to exponential improvement?
      - Standard approach to loosely coupled problems is to pretend the problems are completely decoupled and then fix the interactions after
        - Difficulty is that preconditions constrain the state in which an action can be executed successfully, but do not constrain other actions that might mess it up
        - Augment actions schemas with a new feature: a concurrent action list stating which actions must/must not be executed concurrently
    - Planning with Multiple Agents: Cooperation and Coordination
      - How can agents coordinate together to ensure they execute the plan?
      - One option is to adopt a convention before engaging in joint activity
        - A convention is any constraint on the selection of joint plans
        - When conventions are widespread, they are called social laws
      - In the absences of convention, agents can use communication to achieve common knowledge of a feasible joint plan
        - Possible to engage in communication by simply executing part of the plan, called plan recognition
          - Only works when a single action or short sequence of actions is enough to determine a joint plan unambiguously
        - Can work with competitive agents as well as with cooperative ones
      - The most difficult multiagent problems involve both cooperation with members of one's own team and competition against members of opposing teams, all without centralized control

  - Summary

    - Many actions consume resources, such as money, gas, or raw materials
      - It is convenient to treat these resources as numeric measures in a pool rather than try to reason about them individually
      - Actions can generate and consume resources, and it is usually cheap and effective to check partial plans for satisfaction of resource constraints before attempting further refinements
    - Time is one of the most important resources
      - It can be handled by specialized scheduling algorithms, or scheduling can be integrated with planning
    - Hierarchical task network (HTN) planning allows the agent to take advice from the domain designer in the form of high-level actions (HLAs) that can be implemented in various ways by lower-level action sequences
      - The effects of HLAs can be defined with angelic semantics, allowing provably correct high-level plans to be derived without consideration of lower-level implementations
      - HTN methods can create the very large plans required by many real-world applications
    - Standard planning algorithms assume complete and correct information and deterministic, fully observable environments
      - Many domains violate this assumption
    - Contingent plans allow the agent to sense the world during execution to decide what branch of the plan to follow
      - In some cases, sensorless or conformant planning can be used to construct a plan that works without the need for perception
      - Both conformant and contingent plans can be constructed by search in the space of belief states
        - Efficient representation or computation of belief states is a key problem
    - An online planning agent uses execution monitoring and splices in repairs as needed to recover from unexpected situations, which can be due to nondeterministic actions, exogenous events, or incorrect models of the environment
    - Multiagent planning is necessary when there are other agents in the environment with which to cooperate or compete
      - Join plans can be constructed, but must be augmented with some form of coordination if two agents are to agree on which joint plan to execute




## Reading 11: Quantifying Uncertainty

- Acting under Uncertainty

  - Agents may need to handle uncertainty, whether due to partial observability, nondeterminism, or a combination of the two

  - Previously, we've handled this through the use of belief states, but these have significant drawbacks:

    - When interpreting partial sensor information, a logical agent must consider every logically possible explanation for the observations, no matter how unlikely
      - Leads to impossibly large and complex belief-state representations

    - A correct contingent plan that handles every eventuality can grow arbitrarily large and must consider arbitrarily unlikely contingencies
    - Sometimes there is no plan that is guaranteed to achieve the goal - yet the agent must act
      - It must have some way to compare the merits of plans that are not guaranteed

  - The qualification problem describes when a plan's success cannot be inferred due to an inability to deduce a number of conditions

  - The right thing to do - the rational decision - depends on both the relative importance of various goals and the likelihood that, and degree to which, they will be achieved

  - Summarizing Uncertainty

    - Trying to use logic to cope with large-scale domains fails for three main reasons:
      - Laziness: it is too much work to list the complete set of antecedents or consequents needed to ensure an exceptionless rule and too hard to use such rules
      - Theoretical ignorance: there is no complete theory for the domain
      - Practical ignorance: even if we know all the rules, we might be uncertain about a particular situation because not all the necessary tests have been or can be run

    - The agent's knowledge can at best provide only a degree of belief in the relevant sentences
      - Our main tool for dealing with these degrees of belief is probability theory
        - Same ontological commitments as logic: the world is composed of facts that do or do not hold in any particular case
        - Different epistemological commitments: a probabilistic agent may have a numerical degree of belief between `0` and `1`

    - Probability provides a way of summarizing the uncertainty that comes from our laziness and ignorance, thereby solving the qualification problem
      - Probability statements are made with respect to a knowledge state, not with respect to the real world

  - Uncertainty and Rational Decisions

    - To make choices, an agent must first have preferences between the different possible outcomes of the various plans

      - An outcome is a completely specified state
      - We use utility theory to represent and reason with preferences
        - Utility means the quality of being useful
        - Says that every state has a degree of usefulness, or utility, to an agent and that the agent will prefer states with higher utility

    - Preferences, as expressed by utilities, are combined with probabilities in the general theory of rational decisions called decision theory

      - Decision theory = probability theory + utility theory
      - Fundamental idea is that an agent is rational if and only if it chooses the action that yields the highest expected utility, averaged over all the possible outcomes of the action
        - Called the principle of maximum expected utility

    - ```pseudocode
      function DT-AGENT(percept) returns an action
      	persistent: belief_state, probabilistic beliefs about the current state of the 					world
      				action, the agent's action
          
          update belief_state based on action and percept
          calculate outcome probabilties for actions, given action descriptions and
          	current belief state
          select action with highest expected utility given probabilities of outcomes
          	and utility information
          return action
      ```

- Basic Probability Notation

  - What Probabilities are About

    - Probabilistic assertions are about possible worlds

      - Talk about how probable the various worlds are
      - The set of all possible worlds is called the sample space, referred to by `Ω`
        - Particular possible worlds, or elements of the space, are referred to by `ω`

      - Possible worlds are mutually exclusive and exhaustive
        - Two possible worlds cannot both be the case
        - One must be the case

    - A fully specified probability model associates a numerical probability `P(ω)` with each possible world

      - The basic axioms of probability theory say that every possible world has a probability between `0` and `1` and that the total probability of the set of possible worlds is `1`:

        - $$
          0\le P(\omega)\le 1\text{ for every }\omega\text{ and }\sum_{\omega\in\Omega}P(\omega)=1
          $$

    - Probabilistic assertions and queries are not usually about particular possible worlds, but about sets of them

      - These sets are called events

      - Sets are always described by propositions in a formal language

        - For each proposition, the corresponding set contains just those possible worlds in which the proposition holds

        - The probability associated with a proposition is defined to be the sum of the probabilities of the worlds in which it holds:

          - $$
            \text{For any proposition }\phi,P(\phi)=\sum_{\omega\in\Omega}P(\omega)
            $$

    - Probabilities that refer to degrees of belief in propositions in the absence of other information are called unconditional or prior probabilities

    - Most of the time, we have some information, usually called evidence, that has already been revealed

      - In this case, we are interested in the conditional or posterior probability
      - Ex) `P(doubles | Die_1 = 5)`
        - Where `|` is pronounced "given"

      - When making decisions, the agent needs to condition on all the evidence it has observed
      - Important to note that conditioning means whenever something is true and we have no further information

    - Conditional probabilities are defined in terms of unconditional probabilities as follows:

      - For any propositions `a` and `b`, we have:

        - $$
          P(a|b)=\frac{P(a\land b)}{P(b)}
          $$

        - Holds whenever `P(b) > 0`

      - Can be written in a different form called the product rule:

        - $$
          P(a\land b)=P(a|b)P(b)
          $$

        - For `a` and `b` to be true, we need `b` to be true, and we also need `a` to be true given `b`

  - The Language of Propositions in Probability Assertions

    - Propositions describing sets of possible worlds are written in a notation that combines elements of propositional logic and constraint satisfaction notation
    - If is a factored representation, in which a possible worlds is represented by a set of variable/value pairs
    - Variables in probability theory are called random variables and their names begin with an uppercase letter
      - Every random variable has a domain - the set of possible values it can take on
        - Domains can be sets of arbitrary tokens
        - When no ambiguity is possible, it is common to use a value by itself to stand for the proposition that a particular variable has that value

      - A Boolean random variable has the domain `{ true, false }`
      - By convention, propositions of the form `A = true` are abbreviated as `a`, while `A = false` is abbreviated as `¬a`

    - A bold `P` indicates that the result is a vector of numbers, and where we assume a predefined ordering on the domain
      - Defines a probability distribution for the random variable
      - Also used for conditional distributions
        - `P(X | Y)` gives the values of `P(X = x_i | Y = y_i)` for each possible `i, j` pair

    - For continuous variables, it is not possible to write out the entire distribution as a vector, because there are infinitely many values
      - Instead, we define the probability that a random variable takes on some value `x` as a parameterized function of `x`
      - We call this a probability density function

    - We also need notation for distributions on multiple variables
      - `P(X, Y)`
      - Generates an m x n table of probabilities called the joint probability distribution of `X` and `Y`
      - Can also mix variables with and without values

    - A possible world is defined to be an assignment of values to all of the random variables under consideration
      - A probability model is completely determined by the joint distribution for all the random variables
        - Called the full joint probability distribution
        - Suffices for calculating the probability of any proposition

  - Probability Axioms and their Reasonableness

    - $$
      P(\neg a)=1-P(a)
      $$

    - $$
      P(a\lor b)=P(a)+P(b)-P(a\land b)
      $$

    - If Agent 1 expresses a set of degrees of belief that violate the axioms of probability theory then there is a combination of bets by Agent 2 that guarantees that Agent 1 will lost money every time

- Inference Using Full Joint Distributions

  - Probabilistic inference is the computation of posterior probabilities for query propositions given observed evidence

    - Use the full joint distribution as the knowledge base from which answers to all questions may be derived
    - The probabilities in the join distribution sum to `1`, as required by the axioms of probability

  - One particularly common task is to extract the distribution over some subset of variables or a single variable

    - The process of marginalization or summing out involves calculating the marginal probability of a random variable

    - The general marginalization rule for any sets of variables `Y` and `Z` is as follows:

      - $$
        \textbf{P}(\textbf{Y})=\sum_{\textbf{z}\in\textbf{Z}}\textbf{P}(\textbf{Y},\textbf{z})
        $$

      - A variant of this rule involves conditional probabilities instead of joint probabilities, using the product rule:

        - $$
          \textbf{P}(\textbf{Y})=\sum_\textbf{z}\textbf{P}(\textbf{Y}|\textbf{z})P(\textbf{z})
          $$

        - Called conditioning

  - In most cases, we are interested in computing conditional probabilities for some variables, given evidence about others

    - Utilizes a normalization constant for each distribution, which ensures that it sums up to `1`
      - Represented by `α`

  - A general inference procedure is as follows:

    - Begin with the case in which the query involves a single variable, `X`

    - Let `E` be the list of evidence variables, let `e` be the list of observed values for them, and let `Y` be the remaining unobserved variables

    - The query is `P(X | e)` and can be evaluated as:

      - $$
        \textbf{P}(X|\textbf{e})=\alpha\textbf{P}(X,\textbf{e})=\alpha\sum_\textbf{y}\textbf{P}(X,\textbf{e},\textbf{y})
        $$

      - Together, the variables `X`, `E`, and `Y` constitute the complete set of variables for the domain, so `P(X, e, y)` is simply a subset of probabilities from the full joint distribution

    - Does not scale well

      - For a domain described by `n` Boolean variables, it requires an input table of size `O(2^n)` and takes `O(2^n)` time to process the table

- Independence

  

