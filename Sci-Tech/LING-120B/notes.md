# LING 120B - Spring '22 - Loccioni

[TOC]

## Lecture 1: Parts of Speech

- What is Syntax?
  - Two notions of syntax:
    - As a system of grammar: the part of language that allows speakers to create and undersatand phrases and sentences
    - As a linguistic discipline: the subfield of linguistics that studies the rules and properties of phrases and sentences of human languages
  - What do we mean by rule?
    - We do not mean prescriptive rules (rules that prescribe how people should speak)
    - We are interested in how people really taken (descriptive rules)
  - The syntax of what?
    - The syntax of human language:
      - All and only humans have language
        - There are no primitive or simple languages
        - Even stigmatized languages and varieties of languages are complex, rule-governed systems
      - Superficially very different languages share deep similarities
      - Many logically possible grammatical structures do not exist in any language
      - Children acquire language similarly across different languages
      - Languages shares properties with innate instinctual behaviors
        - The innateness hypothesis: the ability to use language is part of the human biological makeup
  - What does a theory of syntax have to explain?
    - Creativity: speakers of a language can create and understand new sentences
    - Order sensitivity: the order of words affects:
      - Whether a sentence is possible in a language
      - What a sentence means
    - Ambiguity: a string of words can have multiple meanings
    - Recursion: sentence structures embed inside one another, allowing infinitely many sentence structures
    - Restrictions: not every arrangement of words is a sentence
  - Grammaticality
    - A sentence that is part of a language is called grammatical, a sentence that is not is called ungrammatical
    - Our data = grammaticality judgements
    - Meaningless != ungrammatical
    - Ungrammatical != hard to understand
  - Summary
    - Goals of syntax: discover the rules and processes that:
      - Allow the production and comprehension of infinitely many sentences
      - Capture the relationship between syntactic structure and meaning
      - Capture syntactic ambiguity
      - Characterize ungrammaticality, explain why some expressions with plausible meanings and structures are unacceptable
    - How will we do this?
      - Revisit the issues and phenomena already that we saw in Ling 20, but deeper
      - Flesh out concepts that we only touched briefly before
      - Look at new phenomena
      - Look at a variety of languages
    - Consequence: our understanding of syntactic structure will change as we move along
- Parts of Speech
  - The Traditional/Semantic Approach
    - How do we classify words?
    - The semantic approach: parts of speech are classified by their meanings
      - Noun: person, place, or thing
      - Verb: action, occurrence, or state of being
    - Some words can change its part of speech depending on where it appears
    - How do we know that a particular word belongs to a particular category?
      - We look at their distributions
  - Distributional Criteria
    - Distributional definition: parts of speech are classified by their distribution
      - Morphological distribution: affixes appear only on certain kinds of words
      - Syntactic distribution: position relative to other words
    - Words that have the same or strongly overlapping distributions have the same part of speech
    - Words whose distribution does not overlap have different parts of speech
    - Since distribution is language specific, the criteria for identifying parts of speech and the part of speech categories themselves can be language specific
  - Morphemes
    - Morpheme: meaningful atom = the smallest meaning-bearing unit in language
      - They can be free or bound
      - The can be an affix or a root
      - The can belong to an open or to a closed class
      - There are inflectional and derivational affixes
        - Derivational:
          - Plays a lexical (meaningful) role
          - Can change part of speech/category
          - Never required by grammatical rules
          - Typically indicate semantic relations within the word
          - Typically occur before inflectional morphology
        - Inflectional:
          - Plays only a grammatical role
          - Never changes base's category
          - Usually required by grammatical rules
          - Typically indicate relations between different words in a sentence
          - Occur at the end
    - A word can be atomic (if it contains only one morpheme) or complex (if it contains more than one)
  - Nouns
    - Derivational suffixes: -ment, -ness, -ity
    - Inflectional suffixes: -s, -es, -a
    - Syntactic distribution: nouns can appear:
      - As the only word between determiners and verbs
      - As the only word after determiners, adjectives, verbs, prepositions
      - Negated by no
  - Verbs
    - Derivational suffixes: -ify, -ize
    - Inflectional suffices: -ing, -en, -s
    - Syntactic distribution: verbs can appear:
      - As the first word in a (non-negative) command
      - As negated by not/n't
      - Modified by adverbs, but not adjectives
  - Adjectives
    - Derivational suffixes: -able, -ing, -al
    - Inflectional suffixes: comparative -er, superlative -est
    - Syntactic distribution: adjectives can appear:
      - As the only word between determiners and nouns
      - After comparative more and superlative most
      - Modified by degree adverbs like very
  - Compositionality
    - Morphemes combine in a regular way
    - Claim: only one ordering of the five morphemes in de-nation-al-iz-ation produces an English word
    - According to what we said so far, the speaker must know:
      - How each morpheme is pronounced
      - What kind of morpheme it is (free, prefix, affix, etc.)
      - If an affix, what it combines with (what it c-selects)
      - If an affix, that the c-selected element must be adjacent to the affix
        - Only thing not encoded in the lexical entry, it is not specific to any one lexical entry
      - If an affix, what kind of things result after c-selection
  - Lexical Entries - First Version
    - Lexical entry of a morpheme (= the implicit knowledge that a speaker has about it):
      - -er: c-selects `V` to form `N`
    - C-selection stands for category selection
    - The collection of lexical entries is called the lexicon



## Lecture 2: Constituency

- Compositionality
  - Locality of Selection Hypothesis: selection is local in the sense that an item can only select properties of its sister(s)
  - Tree Vocabulary:
    - Branch: a line connecting two parts of a tree
    - Node: each point that is labeled with a word or category, located at the end of a branch
    - Label: the name given to a node
    - Leaf (or terminal node): the nodes along the bottom of the tree
    - Mother: node `A` is a mother of `B` iff `A` is higher up in the tree than `B` and they are separated by a single branch (no intervening nodes)
    - Sister: two nodes are sisters if they share the same mother
  - Compounds and the RHHR
    - The English Right-Hand Head Rule: the rightmost element of a compound is the head of the word
    - What do we mean by head?
      - The head is the part of a linguistic structure that determines the distributions, the main meaning and the syntactic category of the whole structure
  - Affixes and the RHHR
    - What about affixes? Does the RHHR apply to them as well?
    - Yes, and as a result, suffixes and prefixes have a different status
      - Suffixes can change the category of a word
      - Prefixes cannot change the category of the stem they attach to
    - We can therefore extend the RHHR to all words:
      - The rightmost element of a word is the head of the word
    - Also we can assume that both free and bound morphemes belong to categories
      - Given the RHHR, we can then simplify our lexical entries
- Constituency
  - Constituency and Constituency Tests
    - One of the fundamental discoveries about the syntax of natural languages is that languages are chunky: words are organized into chunks or blocks of units that "rules" can manipulate as blocks
      - Languages don't just consist of strings of words, they have syntactic structure
    - Constituent: a group of words that function together as a unit
      - String that speakers can manipulate as a single chunk
    - How do we identify the parts of sentences that form units?
      - We use constituency tests
    - To correctly use a constituency test, we must follow rules
      - Constituency tests manipulate a string, and then we check whether the result is:
        - Grammatical
        - Consistent in meaning
      - Passing the test means the string is a constituent
      - Not passing the test means nothing
  - Stand Alone Test
    - If a group of words can stand alone, for example, as an answer to a question, they form a constituent
    - How it works:
      - Start with a string that you are interested in
      - Create a question that can be answered with the kind of string
      - If you can find a question that can be answered with this string, it is probably a constituent
  - Substitution
    - How it works:
      - Given a well-formed string `S` that we are trying to analyze:
        - Select a substring `SUB`
        - Replace `SUB` in kind by (what looks like) a monomorphic word (a word with no internal structure)
        - If the result `R` is well formed, we conclude that `SUB` is a constituent
        - If the result is ill formed, we conclude nothing
    - Logic of the test:
      - The smallest possible constituent consists of a single word
      - Any string of words that can be replaced by a single word while maintaining the same meaning is a constituent
    - Remember:
      - Original string and replacement should be minimal pairs
      - `S` and `R` have to be very close synonym
    - What can we infer from a successful substitution?
      - The initial phrase is a constituent
      - The initial phrase and its substitution share a distributional property (i.e., they belong to the same category)
      - In the tree structure, `SUB` is a subtree of `S`
    - What kinds of constituents can a pronoun replace?
      - It replaces `DP`s, it can't replace bare nominal phrases, `NP`s
    - What kinds of constituents can then/there replace?
      - `PP`s
    - What kinds of constituents can do so replace?
      - `VP`s
  - Summary
    - When carefully used, replacement can tell us about the constituency of sentence
    - A successful replacement shows that the replace string is a syntactic unit, i.e., constituent
    - In terms of trees:
      - A constituent is a node in the tree
      - Constituency tests reveal the major shape of the tree



## Lecture 3: Coordination, Movement, and Structural Relations

- Coordination
  - Coordination Test
    - Take two acceptable sentences of the form `[A B D]` and `[A C D]`, where `A`, `B`, `C`, and `D` represent (possibly null) substrings
    - If the string `[A B and C D]` is acceptable with the same meaning as `[A B D and A C D]`, this is evidence that `B` and `C` are both constituents, and constituents of the same type
  - Coordination Rule
    - `α -> α and α`
- Movement Tests
  - Topicalization/Fronting
    - Allows one to move a `DP`, `PP`, or `VP` constituent to the front of a sentence
      - The moved constituent is called the (contrastive) topic
    - Thus, topicalization can only affect constituents (but not all of them)
      - Negative results of this tests mean nothing
  - Clefting
    - The affected constituent is called the focus
    - `A B C -> It's B that A C`
    - If the result is acceptable, we have evidence that the string of words in focus form a constituent
    - While `VP`s can be topicalized, they cannot be clefted
  - Pseudoclefts
    - Very similar to clefting in what it does to the information structure of the sentence
    - `A B C -> What A C is B`
    - Pseudoclefts using a wh- word other than "what" are judged marginal by most speakers
    - In most cases `PP`s simply cannot be pseudoclefted
    - In addition, something we probably want to call a `VP` pseudocleft is possible (`A B C -> What A do C is B`)
- Structural Relations
  - Branch: a line connecting two parts of a tree
  - Node: each point that is labeled with a word or a category is called a node
    - It is the end of a branch
  - Label: the name given to the node
  - Dominance: a node `α` dominates a node `β` iff there exists a chain of two or more nodes `α`, `γi`, ..., `γj`, `β` such that each node is the mother of the next one
    - Roughly, `α` is an ancestor of `β`
  - Root: the node that dominates all other nodes in a tree, and is itself dominated by none
  - Leaf (or terminal node): the nodes along the bottom of the tree
    - They dominate nothing
  - Non-terminal node: a node that dominates something
    - A node that is a mother
  - Immediate dominance: a node `α` immediately dominates a node `β` iff `α` dominates `β` and there is no node `γ != α` such that `γ` dominates `β`
    - `α` is `β`'s mother
  - Mother: `A` is the mother of `B` if `A` immediately dominates `B`
  - Daughter: `B` is the daughter of `A` if `B` is immediately dominated by `A`
  - Sisters: two nodes that share the same mother
  - Sister precedence: node `A` sister-precedes node `B` iff both are immediately dominated by the same node, and `A` appears to the left of `B`
  - Precedence: node `A` precedes node `B` iff neither `A` dominates `B` nor `B` dominates `A` and `A ` (or some node dominating `A`) sister precedes `B` (or some node dominating `B`)
  - C-command: node `A` c-commands node `B` if every node dominating `A` also dominates `B` and `A` does not itself dominate `B`
    - Left-right doesn't matter, sisterhood does
    - Every node c-commands its sister and everything below its sister
  - Asymmetric c-command: `A` asymmetrically c-commands `B` if `A` c-commands `B` by `B` does not c-command `A`
  - Exhaustive dominance: a node `α` exhaustively dominates a set of terminal nodes `N` iff `α` dominates every node in `N` (so that there is no member of the set `N` that is not dominated by `α`) and `α` does not dominate any terminal node not in `N`
    - Exhaustive dominance is useful to talk about the leaves of a tree (which are the lexical terms)
    - Leaves form a constituent precisely when there is a node in the tree to exhaustively dominate those leaves
    - Immediate domination is not a necessary condition for exhaustive dominance



## Lecture 4: Ambiguity and X-Bar Theory

- Syntactic Ambiguity
  - Ambiguity
    - A sentence can have more than one meaning => ambiguity
    - Ambiguity comes in 2 flavors:
      - Lexical ambiguity uses words with more than one meaning
      - Structural ambiguity ambiguous due to the structure or constituency of the sentence
    - The relationship between structure and meaning is systematic
  - The Principle of Modification
    - Determines the meanings of the different structures
    - An adjunct contributes its meaning to the category it combines with syntactically
      - We can use constituency tests to manipulate the structure and show that:
        - The ambiguities are structural
        - The meanings are as presided by the Principle of Modification
- Where We Are
  - We know that sentences exist
  - Based on constituency tests, we've been able to identify:
    - Verb phrases (`VP`s): do so replacement test, topicalization, pseudoclefting, coordination, etc.
    - Determiner phrases (`DP`s): replacement using a pronoun, topicalization, clefting, pseudoclefting, coordination, etc.
    - Noun phrases (`NP`s): substitution by one, coordination, etc.
    - Prepositional phrases (`PP`s): replacement test (there, then, etc.), topicalization, clefting, coordination, etc.
  - We also came across other parts of speech (adjectives, adverbs) and modal verbs (should, will, must, can, etc.)
- X-Bar Theory
  - Behind the range of diverse constructions that English and other languages allow, we find surprising uniformity and regularity
    - The idea that the rule system that underlies our phrases is very simple
    - Every phrase looks the same
  - X-Bar theory is a theory that tries to eliminate phrase structure idiosyncrasies: every phrase looks the same
    - Every phrase is the projection of some lexical category: `N`, `V`, `A`, `Adv`, etc.
    - The system of projected constituency is common to all categories
  - We need to distinguish between 4 types of elements:
    - Heads
    - Complements
    - Specifiers
    - Adjuncts
  - We'll start by justifying the distinction between complements and adjuncts
  - Head
    - Every phrase contains exactly one head of some category (`N`, `V`, `Adj`, `Adv`, `D`, etc.)
    - Every head is contained within a phrase of the same category
    - The head is present in all cases
      - Whether a complement or a specifier is needed is a case-by-case thing
    - The head of a constituent projects its label (lexical category) to that constituent
    - The head of a constituent tells us the distribution of that constituent
    - The head selects its sister constituent
  - Complements
    - Complements are selected by the head as their sisters
    - They are merged into the structure as a result of the complement rule
      - `X' -> X (Complement)`
      - For head-final languages: `X' -> (Complement) X`
  - Adjuncts
    - Adjuncts are modifiers which modify the phrase
    - They are sisters to phrases (`XP`s, not `X'` or `X`)
    - They tend to be optional
    - In English, they can both precede and follow their sister
    - Adjunct Rule:
      - `XP -> Adjunct XP`
      - `XP -> XP Adjunct`
  - Specifiers
    - Specifiers are merged into the structure as the sister of `X'`
    - Specifier Rule:
      - `XP -> (Specifier) X'`
  - How to Write an X-Bar Compliant Tree
    - For every head you want to have 3 layers: `XP -> X' -> X`
    - If there is no specifier, you can omit the X-bar level: `XP -> X`
    - Both the specifier and the complement are phrases
    - Complements are sisters of the head that selects them, don't attach complements to higher projections of the head
- Sentences
  - Tense Phrases (`TP`s)
    - We've been using `S` for sentences, which doesn't seem to follow the X-bar schema
    - We are going to adopt a new system where Tense is the head of sentences
    - The `X'` schema will apply to `TP`s as well"
      - The head: `T`
      - Complement: `VP`
      - Specifier: the subject (`DP` or `CP`)
      - `TP -> YP T'`
      - `T' -> T VP`
    - Realization of `T`
      - `T` can be free
        - The non-finite `T` head "to"
        - Modals
      - `T` can be bound
        - `[+pres]`
        - `[-pres]` or -ed
    - Bound morphemes in tree structures
      - The bound morpheme is in `T`
        - `[+pres]` or `[-pres]` (-ed, -s are also fine)
      - The `VP` is headed by the bare verbal form
    - Lexical entries specify the basic properties of the syntactic atoms (heads), including the category and all the selectional properties
      - Only unpredictable properties that the speaker has to acquire should be included
    - Selection => the type of phrase that the head requires in its specifier (or subject) position
    - C-selection => complement selection, tells us what kind of sister a particular head is selecting for



## Lecture 5: Complementizer Phrases and Verb Phrases

- Complementizer Phrases (`CP`s)
  - Sentences can be embedded inside others
    - Recursion is one of the core properties of human language
  - We have a new test we can use: "so" or "this" or "that" replacement
  - Here is the proposal:
    - The complementizer is the head of the constituent
      - It selects for a `TP` complement
    - The X' schema applies to `CP`s as well
    - `CP`:
      - The head: `C`
      - Complement: `TP`
      - Specifier: wh-phrases in wh-questions
  - Realization of `C`
    - The `C` head can be unpronounced in some languages
      - In English, only "that" has this property
  - `C` subcategorize for properties of `TP`
    - "if", "that" select finite `TP`s
    - "for" select non-finite `TP`s
    - "whether" seems to allow both `-tense` and `+tense` `TP`s
- Verb Phrases (`VP`s)
  - We detect `VP`s using: "do so" replacement test, topicalization, pseudoclefting, coordination, etc.
  - `VP`:
    - The head: `V`
    - Complement(s): `DP`s, `CP`s, `PP`s, (`TP`s)
    - Specifier: none (for now)
  - Verb Types
    - Intransitive verbs
      - Ex) `VP -> V' -> V -> leave`
    - Transitive verbs
      - Ex) `VP -> V' -> V DP -> love Pat`
    - Ditransitive verbs
      - Ex) `VP -> V' -> V DP PP -> give a present to Peter`
  - Complements are selected by `V`
    - They fill a required slot in the lexical entry of the head
    - They are in a sisterhood relation to `V`
    - They tend to be obligatory
  - Adjuncts are modifiers which modify the phrase
    - They are sisters to phrases
    - They tend to be optional
  - `V` Complement Adjunct Order
    - The 3 rules we have so far:
      - Complement rule
      - Specifier rule
      - Adjunct rule
    - When the adjunct is on the right side, it must follow the complement
      - Head => Complement => Adjunct
  - Summary: `VP` Structure
    - The head of the `VP` is the verb
    - Transitive and ditransitive verbs select for complement(s): they are sisters of `V` (we merge them applying the complement rule)
    - No specifier in the `VP` (not yet)
    - Adjuncts attach to the maximal projection (the `VP`)
    - "do so" replaces `VP` (complement must be included)
      - If a phrase need not be included as part of the sequence being replaced by "do so", then it is an adjunct
      - If it must be included, then it is a complement



## Lecture 6: Determiner Phrases and Noun Phrases

- Determiner Phrases (`DP`s)

  - We detect `DP`s using: replacement using a pronoun, topicalization, clefting, pseudoclefting, coordination, etc.

  - `DP`s:

    - The head: `D`
    - Complement(s): `NP`s
    - Specifier: `DP`s
    - `DP -> (DP) D' -> D NP`

  - `D` can be empty

    - Bare plurals
      - Ex) Dogs bite
    - Proper nouns in English
      - Ex) Mary
    - Is an empty `D` needed in the structure of proper names?
      - In other languages proper names do require overt determiners

  - Proper names and phrases headed by determiners have the same syntactic distribution

    - They can be replaced by pronouns
    - Proper names and phrases headed by determiners can be coordinated

  - The specifier position of `DP` can be filled: the Saxon genitive case

    - Ex) Mary's brother
      - Has the distribution of a `DP` (replacement, coordination, etc.)
      - "Mary" is also a `DP`, therefore it is a phrase
      - The only other position available for a phrase in the `DP` is the specifier position
        - Options: "'s" is the determiner, or "Mary's" is the specifier
        - We prefer "'s" as the determiner because genitive phrases are in complementary distribution with other determiners
    - Results in the following structure:
      - The `DP`-possessor is in specifier position
      - "'s" is the determiner (this accounts for the complementary distribution with determiners)
      - The `NP`-possessee is in complement position

  - Summary: `DP` structure

    - The head of the `DP` is a determiner, which can be null

    - The complement is a `NP`

    - `DP`s can have subjects (the phrase occurring in specifier position), as in the case of the Saxon genitive construction

    - We did not see any examples of `DP` adjuncts (and we won't see any)

    - Lexical entries:

      - | the  | `D`  | free  |              | c-selects `NP` | "the book"    |
        | ---- | ---- | ----- | ------------ | -------------- | ------------- |
        | this | `D`  | free  |              | c-selects `NP` | "this book"   |
        | 's   | `D`  | bound | selects `DP` | c-selects `NP` | "John's book" |

  - Summary: specifiers

    - Specifier (or subject) of `T` or `TP` (the subjects of the sentence)
      - Can be a `DP` or a `CP`
    - Specifier (or subject) of `D` or `DP` (the possessor)

- Possessive Pronouns

  - The structure of possessive pronouns is very similar to the genitive construction we discussed above

- Noun Phrases (`NP`s)

  - Nouns can also take complements (especially if they are the nominalized version of some verbs)

    - The complement of `N` is `N`'s sister, whereas the adjunct attaches to the maximal projection (`NP`)

  - "One" can only replace `NP`s, not `N`s or `N'`s

  - We expect to find the same strict order we find in `VP`s

    - Head => Complement => Adjunct

  - Summary: `NP` structure

    - The head of the `NP` is a noun

    - Some nouns can take `PP` or `CP` complements

    - No specifiers so far

    - `NP` adjuncts are very common: they can be `PP` or `AP`

      - `PP` adjuncts are normally on the right
      - `AP` adjuncts are normally on the left

    - Lexical entries:

      - | cat     | `N`  | free |                         | "cat"           |
        | ------- | ---- | ---- | ----------------------- | --------------- |
        | student | `N`  | free | c-selects "of" (`PP`)   | "student of CS" |
        | claim   | `N`  | free | c-selects "that" (`CP`) | "claim that..." |



## Lecture 7: Adjective Phrases, Adverbial Phrases, Prepositional Phrases, and Cross-Linguistic Variation

- Adjective Phrases (`AP`s)

  - `AP`:

    - The head: `A`
    - Complement(s): `PP`s and `CP`s
    - Specifier: none (for now)

  - Adjectives allow adjuncts

    - They normally express a degree and appear on the left in English

  - Summary: `AP` structure

    - The head of the `AP` is an adjective
    - (Some) adjectives can take `PP` or `CP` complements
      - Proud [of `DP`]
      - Proud [that `TP`]
      - Fond [of `DP`]
    - Specifiers coming soon
    - `AP` adjuncts are normally expression of degree
      - Ex) "very", "extremely"

  - Lexical entries:

    - | red   | `A`  | free  |                        | "red"           |
      | ----- | ---- | ----- | ---------------------- | --------------- |
      | proud | `A`  | free` | c-selects `PP` or `CP` | "proud of John" |

- Adverbial Phrases (`AdvP`s)

  - `AdvP`:

    - The head: `Adv`
    - No complements
    - No specifiers

  - `AdvP`s can be modified by another `AdvP` (the ones that express a degree)

  - Summary: `AdvP` structure

    - The head of the `AdvP` is an adverb
    - No complements
    - No specifiers
    - `AdvP` adjuncts are normally expression of degree
      - Ex) "very", "extremely"

  - Lexical entries

    - | quickly | `Adv` | free | "quickly" |
      | ------- | ----- | ---- | --------- |
      | slowly  | `Adv` | free | "slowly"  |

- Prepositional Phrases (`PP`s)

  - `PP`:

    - The head: `P`
    - Complement(s): `PP`s and `DP`s
    - Specifier: none (for now)

  - Prepositions can sometimes have adjuncts

    - Ex) "right against the grain", "exactly under the tree"

  - Summary: `PP` structure

    - The head of the `PP` is a preposition
    - (Some) prepositional phrases can take `PP` or `DP` complements
    - Specifiers coming soon
    - `PP` adjuncts are normally adverbial phrases
      - Ex) "right", "exactly"

  - Lexical entries

    - | up   | `P`  | free | (c-selects `DP`)       | "up" or "up the rope"                |
      | ---- | ---- | ---- | ---------------------- | ------------------------------------ |
      | of   | `P`  | free | c-selects `DP`         | "of linguistics"                     |
      | from | `P`  | free | c-selects `DP` or `PP` | "from Italy" or "from under the rug" |

- Cross-Linguistic Variation

  - X-bar theory says something about how subjects and complements are hierarchically organized in a given category, but does not entirely predict linear order
    - Heads combine with complements first and then with subjects
    - Adjuncts combine with the projection that is formed
  - It doesn't say anything about the order of sisters
    - How the relations are linearized is determined by parameters in each individual language
      - In English we have Specifier =>< Head => Complement
      - In other languages we could have a different linear order
    - Some orders are excluded by X-bar theory



## Lecture 8: Lexical Entries and Subjects

- Lexical Entries
  - Syntactic structures are determined in part by the properties of the syntactic atoms
  - Lexical entries are a list of the relevant properties of the syntactic atoms (the heads):
    - All the selectional properties are included
    - Only unpredictable properties should be included
- Subjects
  - So far we only encountered two phrases with specifiers `TP`s and `DP`s
  - `VP`s, `AP`s, and `PP`s can also have subjects: let's talk about small clauses
  - We know that a full sentence can be embedded under another one



## Lecture 9: Subjects and Binding Theory

- Subjects
  - Cases where the embedded clause is smaller
    - Ex) Fred saw John drunk
    - Some of them have a "that"-`TP` counterpart
      - Ex) Peter heard Mary leave => Peter heard that Mary left
      - These sentences and their counterparts seem to have different semantic requirements
    - These constituents lack the `T`-layer
- Binding Theory
  - Binding theory is a theory of what controls possible coreference between different types of `DP`s in particular syntactic configurations
    - Reference: the relation between a `DP` and the ting in the world that the meaning of that `DP` picks out
    - Two `DP`s co-refer if they refer to the same entity
  - Indexation: notational tool for keeping track of what `DP`s refer to
    - Indices: `i`, `j`, `k`, etc.
      - Two `DP`s with the same index refer to the same thing
        - Two `DP`s that have the same index are said to be co-indexed
        - Two `DP`s that are co-indexed are said to co-refer (i.e., refer to the same entity in the world)
      - Two `DP`s with different indices refer to different things
  - Based on their syntactic behavior, we need to distinguish among three kinds of `DP`s
    - Anaphors: itself, herself, yourselves
    - Pronouns: you, me, us, him
    - R-expressions: John, the professor, etc.
  - Quick note on notation:
    - Syntacticians will sometimes abbreviate two sentences that are otherwise identifical, but have different indices
    - The two possible indices are separated by a slash and the index that would make the sentence ungrammatical is marked with an asterisk
  - Principle A: Anaphors
    - Seems that unlike pronouns and R-expressions, anaphors need a co-referential `DP` in the same sentence (we'll call this `DP` the antecedent or binder)
    - Principle A
      - An anaphor must be bound in its binding domain
        - Binds: `A` binds `B` and only if `A` c-commands `B` and `A` and `B` are co-indexed
          - C-command: the relationship between a node, its sister, and the stuff dominated by its sister
        - Binding domain: the smallest `XP` containing the anaphor that has a subject
          - According to this definition the anaphor can be the subject of the `XP`
    - Binding Principle A also applies to reciprocals like "each other"



## Lecture 10: Binding Theory

- Principle B: Pronouns
  - Pronouns seem to have the opposite requirements
  - Principle B
    - A pronoun must be free in its binding domain
    - Binds: `A` binds `B` if and only if `A` c-commands `B` and `A` and `B` are co-indexed
      - C-command: the relationship between a node, its sister, and the stuff dominated by its sister
    - Binding domain: the smallest `XP` containing the pronoun that has a subject
      - According to this definition the pronoun can be the subject of the `XP`
- The Binding Domain of Anaphors and Pronouns
  - If the definition of BD is the same, we should expect pronouns and anaphors to be in complementary distribution
  - In most cases, this prediction is borne out, but in other cases, it isn't
    - In order to account for this, we need to allow the anaphor to have a larger BD
    - Principle A
      - An anaphor must be bound in its binding domain
        - Binds: `A` binds `B` and only if `A` c-commands `B` and `A` and `B` are co-indexed
          - C-command: the relationship between a node, its sister, and the stuff dominated by its sister
        - Binding domain: the smallest `XP` that has a subject that c-commands the anaphor
          - According to this definition the anaphor cannot be the subject of the `XP`
  - Binding domain of anaphors:
    - The smallest `XP` that has a subject that c-commands the anaphor
  - Binding domain of pronouns:
    - The smallest `XP` containing the pronoun that has a subject
-  Principle C: R-Expressions
  - R(eferential)-expressions = non-pronomnial expressions
    - Proper names
    - Descriptions
  - Principle C: R-expressions cannot be bound
- Locality of Selection
  - At this point, we have a system where structure is mostly dictated by the selectional properties of individual lexical terms
    - Lexical entries can be used to represent this information => information in those lexical entries tells us how to build structure
      - It tells us whether, and which, complements/specifiers are required
  - Locality of selection means that these requirements are local
  - Preliminary:
    - If an atom selects an element, it acts as a head
      - This head must have the selected element as its complement or its subject
    - Selection is local in the sense that there is a maximal distance between a selector and what is selects
    - In other words, complements and subjects are realized within the maximal projection headed by the lexical term
  - `VP`-Internal Subject Hypothesis
    - There is a blatant violation of this principle in what we've been doing so far
      - Ex) "I saw John hug Peter" vs. "John will hug Peter"
    - "John" is indeed selected for locally by "hug", but other factors force it to be realized in the specifier of `TP`
    - Proposal:
      - Subjects enter the derivation in the `VP`
      - They end up in `Spec, TP` as a result of movement
      - Called the `VP`-internal subject hypothesis
      - It applies to the selected subject of all predicates
      - If subjects are `VP` internal, why do they move?
        - The Extended Projection Principle (EPP): the specifier of `TP` always has to be filled



## Lecture 11: Verb Raising, Tense Lowering, and "Do"-Support

- `VP`-Internal Subject Hypothesis

  - What evidence do we have? Floating quantifiers
    - Seems to only apply to universal quantifiers
    - Examples:
      - "The students all left"
      - "My brothers have both studied Greek for years"

- Head Movement

  - Problem in our tree structures: present and past tense morphemes are separated from the `V`

    - Our tree structures correctly represent the relations between head and phrases but they do not capture the way in which the heads are actually pronounced

    - What movement operations could explain this issue?

      - The `V` moves up to `T`
      - `T` moves down to `V`
      - Both operations turn out to exist, but for different subclasses of verbs, the former for auxiliary verbs (like "have" and "be"), the second for all other English verbs

    - The evidence for this account comes from the distribution of adverbs and negation

      - Adverbs and negation are adjuncts to `VP`

      - We use them as a diagnostic for where the verb is

        - Is the verb before or after negation

      - |                  | Modal `V` | Aux `V` | Lexical `V` |
        | ---------------- | --------- | ------- | ----------- |
        | Before Negation? | Y         | Y       | N           |
        | After Negation?  | N         | Y       | Y           |

      - Triggered by bound morphology

  - `V`-to-`T` Movement/Verb Raising

    - Modals are generated in `T`
    - Auxiliaries are verbs (heads of `VP`s)
      - They can move to `T` when the position is not otherwise occupied
    - How do we know they are different?
      - There is only one modal verb per `TP`, whereas we can have more than one auxiliary verb
        - Ex) "John has been reading for hours"
      - As we saw, we have no evidence for modal verbs ever being in a position lower than `T`
    - Lexical verbs do not move to `T` in English
      - In other languages, lexical verbs do move to `T` (i.e., French)
        - English: Subject >> Adverb/Negation >> Lexical Verb >> Direct Object
        - French: Subject >> Lexical Verb >> Adverb/Negation >> Direct Object

  - Tense Lowering

    - Lexical verbs do not move to `T` in English
    - `T` moves onto the verb
    - "Do"-Support
      -  Negation
      - Tense lowering doesn't apply in some cases
        - A dummy verb "do" can be inserted to support the stranded affix
      - When there is no other option for supporting inflectional affixes, insert the dummy verb "do" into `T`
      - Ex) "John studied the report" vs. "John did not study the report"



## Lecture 12: T-to-C Movement

- Head Movement
  - We saw different ways in which inflectional morphology on `T` can be supported
    - The `V` moves to `T`
      - In English, only auxiliary verbs can undergo this movement
      - In some other languages (French), lexical verbs can too
    - `T` lowers down to `V`
      - e.g., lexical verbs in English
    - "do" is inserted
      - e.g., negated sentences in English
  - Next, we are going to look at another type of head-to-head movement
  - `T`-to-`C` movement
    - An operation that moves the material in `T` to `C`
    - Unpronounced complementizers trigger the movement
      - In English, `C [+Q]`
    - So we get subject auxiliary inversion
      - Ex) "John should study the report" vs. "Should John study the report?"
    - All auxiliaries do this
      - More precisely, only the auxiliary in `T` can precede the subject (head movement targets the closest auxiliary)
    - Proposal: the question complementizer `C [+Q]` is an affix which needs to be pronounced
      - It triggers the movement of `T` to `C`
    - Head movement is local (intervening heads cannot be skipped)
      - `V` to `C` doesn't exist: in order to move what is in `V` to `C`, you need a two step movement
        - `V` to `T`
        - `T` to `C`
        - Ex) "Has John already eaten?"
    - What about cases in which what is in `T` is not a free morpheme (a sentence with no auxiliary or modal verb)?
      - In these cases, `T` doesn't lower to `V`
      - In these cases, we move `T` to `C` and then we insert the dummy verb "do"
      - The same rule ("do" support) applied in the case of negation
        - It is a last resort rule: it applies in cases where there is nothing else to do
  - Summary
    - Head movement is a way of providing bound morphemes with a pronunciation
    - Languages differ on which heads undergo and which heads trigger head movement (French vs. English)
    - How to proceed (for English):
      - Is `V`-to-`T` possible/justified?
      - If that's not an option or the output is still unpronounceable: is `T`-to-`C` possible/justified?
      - If that's not an option or the output is still unpronounceable: is tense lowering (from `T` to `V`) possible?
      - If that's not an option or the output is still unpronounceable: "do" support
      - If you still have a morpheme that needs to be pronounced, go back to the first step



## Lecture 13:

- 

