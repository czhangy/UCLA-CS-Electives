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

    - Sumper simple, super elegant

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



## Lecture 3:

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

- Good Behavior: The Concept of Rationality
- The Nature of Environments
- The Structure of Agents
- Summary



## Reading 3: Solving Problems by Searching

- 

