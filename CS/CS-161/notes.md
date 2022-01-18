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
        - If solutions are dense, may be much fast than BFS
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

  - Uniform Cost Sesrch

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



## Lecture 5:

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



## Reading 4:

- 

