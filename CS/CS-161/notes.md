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
      	
      ```

      

- Informed (Heuristic) Search Strategies

- Heuristic Functions

- Summary



## Reading 4:

- 

