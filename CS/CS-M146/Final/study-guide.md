# M146 Study Guide

## Supervised Learning

- Definition
  - Consideration of systems that apply a function `f` learned from examples to input items `x` and return an output `y = f(x)`
  - Learner takes examples from a training dataset to learn a model `g(x)`, which is then tested on a testing dataset and evaluated by comparing results to actual labels
- Terms
  - Instance space: features we're using, typically drawn from an `n`-dimensional vector space
  - Label space: the learning task we're dealing with
  - Hypothesis space: what kinds of models we're learning
  - Learning algorithm: how we learn the model from the training data
  - Loss function: how we evaluate the model's success
- Underfitting vs. Overfitting
  - Underfitting is when the model is too general and has too rough of a decision boundary
  - Overfitting is when the model is too specific to the training data and is likely to not generalize well
    - Easily hindered by noise in the training data/small sample sizes
    - Can be prevented with:
      - Less expressive models (e.g., linear models)
      - Regularization to promote simpler models
      - Noise in training data
      - Earlier stopping of the optimization/fitting process
- Bias vs. Variance
  - Bias is how close the training data is to the true distribution
  - Variance is how close the training data is to each other
- Train/Dev/Test Splits
  - Split data into two sets: a larger training set and a smaller testing set
    - The training set is further split into two sets: a larger training set and a smaller dev set
  - For each possible value of the hyperparameter, train the model using the training set and evaluate its performance using the dev set
  - Choose the model parameter with the best performance on the dev set
    - Optionally, retrain the model using this optimal parameter on the union of the training and dev sets
  - Evaluate the model on the test set
  - Tradeoffs:
    - A larger dev set means there may not be enough data in the training set for a model to be trained on
    - A smaller dev set means the result of testing the model in dev may be insignificant
- N-Fold Cross Validation
  - Instead of using a single training/dev split, the data is split into `N` equal-sized parts, and `N` different classifiers are trained with each value of the hyperparameter
  - The average accuracy and standard deviation of each fold is reported as the success of that hyperparameter
- Classification vs. Regression
  - Classification works to predict a discrete quantity
  - Regression works to predict a continuous quantity

## Hypothesis Space

- The number of possible functions `f` from `X` to `Y` is:

  - $$
    |Y|^{|X|}
    $$

  - Learners typically consider only a *subset* of these functions, called the hypothesis space

- Example: m-of-n rules for Boolean datasets

- Flexible hypothesis spaces are ideal, and algorithms should find the "best" hypothesis in the hypothesis space that fits the data and hope it generalizes well

## KNN

- Algorithm
  - Learning: store all the training examples
  - Prediction: find the `k` *closest* training examples to `x` and construct the label of `x` using these `k` points
- Considerations
  - How is distance defined? => Can be domain-dependent
    - Euclidean/Manhattan/`Lp`-norm for numerical features
    - Hamming distance for symbolic features
  - How are the hyperparameters (`k` and distance measure) chosen? => empirical studies
  - How should the information of the `k`-nearest neighbors be aggregated to make the prediction?
    - For classification: every neighbor votes on the label => the most frequent label is predicted
    - For regression: predict the mean value, possibly weighted by the distances to each point
- Inductive Bias
  - Definition: the set of assumptions the learner uses to predict outputs of unseen inputs
  - The label of the point is similar to the label of nearby points
- Decision Boundary
  - KNN never builds an explicit function/hypothesis
  - KNN partitions the space into sections with various predictions, based on the proximity of the test point to other points
- The Curse of Dimensionality
  - In high-dimensional spaces, the majority points lie far away from the center
  - This is bad for KNN specifically, since it means that most points will lie far away from each other in a complex vector space
  - However, in most real-world data, the data points are not uniformly distributed throughout the high-dimensional space, and can therefore be preprocessed to simplify the vector space and bring points closer to the origin
    - Normalize the data to have 0 mean and unit standard deviation in each dimension

## Decision Trees

- Representation
  - Decision trees are classifiers for instances represented as feature vectors
  - Nodes are tests for feature values
  - Edges exist for each value of the feature tested by the parent node
  - Leaves specify the final prediction
  - Numerical values can be used by splitting nodes with thresholds
- Learning
  - Process all the data available
  - Recursively build a decision tree top-down
- ID3 Algorithm
  - If all examples are labeled the same, return a single leaf node with that label
  - Otherwise, pick an attribute to split the data on and create branches for it, calling ID3 on each of the resultant datasets
- Attribute Splitting
  - Goal of splitting is to have the resulting decision tree be as small as possible => optimal solution is NP-hard
  - Use a greedy heuristic search to approximate the optimal solution
- Information Gain
  - The information gain of an attribute is the expected reduction in entropy caused by partitioning on this attribute
  - The entropy of partitioning the data is calculated by weighting the entropy of each partition by its size

## Linear Models

- Definition

  - In `n` dimensions, a linear classifier represents a hyperplane that separates the space into two half-spaces

  - $$
    \bold{w}^T\bold{x}+b
    $$

    - `w` is the weight vector, `x` is the feature vector, and `b` is the bias term

- Hiding the Bias Term

  - The weight and feature vectors can be reformulated so that a value `b` is added to the weight vector with a corresponding value of 1 added to the feature vector, allowing `+ b` to be removed from the expression

- Learning

  - Finding the *best* parameters `w` and `b`
  - Can be done with Perceptron, logistic regression, linear SVMs, naïve Bayes', etc.,  which each define "best" differently
  - Not all target functions can be represented linearly, e.g., XNOR

## Perceptron

- Properties
  - Goal is to find a separating hyperplane
  - Is an online algorithm, which means one example is processed at a time
  - Converges if the data is separable, given some mistake bound
  - All separating hyperplanes are equally good
- Algorithm
  - Initialize the weight vector as `0`
  - For each point in the training data, predict the label of the point using the current weight vector
  - If the prediction is wrong, add the feature vector times the true label to the weight vector
- Hyperparameters
  - `T`: the number of epochs (number of times the algorithm runs through the training dataset)
  - `𝜂`: the learning rate
- Learnability
  - Perceptron cannot learn what it cannot represent (linearly separable functions only)
- Convergence
  - If the data is linearly separable, the model will converge
    - The rate at which this happens is dependent on the "difficulty" of the problem, as defined by the margin
    - Mistake Bound Theorem bounds the number of mistakes Perceptron can make on the training dataset
  - If the data is not linearly separable, the model will eventually enter an infinite loop
  - Mistake Bound Theorem guarantees that  
- Margin
  - The margin of a hyperplane for a dataset is the distance between the hyperplane and the data point nearest to it
  - The margin of a data set `𝛾` is the maximum margin possible for that dataset using any weight vector
    - Note that the margin is not scale-invariant

## Logistic Regression

- Non-Linearly Separable Data

  - Causes:
    - Non-linear decision boundary
    - Noise
    - Lack of features
    - Nature of the prediction task

- Idea

  - Instead of predicting the label, we predict the probability of the label being some value

- Sigmoid Function

  - The sigmoid function is used as a transformation function to map values to the range of probabilities
  - Defines the hypothesis space of logistic regression

- Properties

  - Tends to find a centered decision boundary

- Maximum Likelihood

  - The likelihood function is defined as the joint density of the data
  - The Maximum Likelihood Estimator (MLE) is the value that maximizes the likelihood
    - The log-likelihood is defined as the log of the likelihood function, and is used to replace products with sums in the likelihood function
    - Log-likelihood is used because its maximum occurs at the same place as that of the likelihood function
  - Use the MLE to find the hypothesis that predicts the probability

- $$
  \arg\max_{\bold{w},b}P(y_i\ |\ \bold{x}_i;\bold{w},b)=\arg\max_{\bold{w},b}-\sum_{i=1}^m\log(1+\exp(-y_i(\bold{w}^T\bold{x}_i+b)))
  $$

## Gradient Descent

- Algorithm
  - Start at a random point
  - Determine a descent direction, choose a step size, and update your location, repeating until some stopping criterion is satisfied
    - Descent direction is determined by calculating the gradient across all points in the training data
- Convergence
  - GD will converge at the global minimum, given that the function is convex and a proper step-size is chosen
  - Too small of a step size results in the algorithm taking too long to converge, too large of a step size makes the algorithm unstable
  - Usually requires many iterations to converge
- Stochastic Gradient Descent (SGD)
  - Instead of updating the weight vector by iterating over all of the data in the dataset, SGD picks a random data point and calculates the gradient using it
  - Idea is to approximate the true gradient by going one example at a time

## Neural Network

- Definition

  - Neural networks are made up of nodes connected by links
  - Each link has an associated weight and activation level
  - Each node has an input function, an activation function, and an output

- Feed-Forward

  - The input function of each unit is applied to compute the input value
  - The activation function transforms this input value into some final value
    - This activation function must be non-linear, otherwise the neural network reduces to a linear classifier
  - Bias terms are added at each level

- Learning

  - Maximizing the likelihood of cross-entropy loss

  - $$
    \Theta=\arg\min_\Theta-\sum_i^m[y_i\log h_\Theta(x_i)+(1-y_i)\log(1-h_\Theta(x_i))]
    $$

  - Leverage chain rule to simplify the process of finding the gradient of the above

- Backpropagation

  - The process of using local gradients to compute the global gradient
  - Sum the paths from `x` to `f` to compute `df/dx`

## Multiclass Classification

- Definition

  - $$
    \text{Output}\in\{1,2,3,\ldots,K\}
    $$

  - Each input belongs to exactly one class

- Key Ideas

  - Reducing multiclass to binary
  - Training a single classifier

- One-Against-All (OAA)

  - A decomposition into `K` binary classification tasks
  - For a class `k`, construct a binary classification task as positive examples are elements with label `k` and negative examples are all other elements
  - Ideally, only the correct label will have a positive score when evaluated on test data
    - Choose the label that gives the highest score => "winner takes all"
  - Analysis:
    - Not always possible to learn, as classes aren't always individually separable from all the others
    - Easy to implement, works well in practice

- One-Against-One/All-Against-All (OAO)

  - A decomposition into `C(K,2)` binary classification tasks
  - For each class pair `(i, j)`, construct a binary classification task as positive examples are elements with label `i` and negative examples are elements with label `j`
  - Decision Options:
    - More complex; each label gets `k - 1` votes
    - Classify example `x` to take label `i` if `i` wins on `x` more often than `j`

- Comparisons

  - `O(K)` weight vectors to train/store for OAA vs. `O(K^2)` weight vectors for OAO
  - OAA is less expressive due to the strong assumption that all classes are separable
  - `K × (# of features + 1)` parameters for OAA vs. `(KC2) × (# of features + 1) ` parameters for OAO
  - `K × (# examples per class)` training data points for OAA vs. `2 × (# of examples per class)` training data points for OAO

- Issues

  - Learning optimizes over local metrics => doesn't guarantee good global performance
  - Poor decomposition = poor performance, but local problems are generally irrelevant to the main problem

- Softmax

  - Used for constructing a log-linear model for multiclass problems in place of the sigmoid function
  - A single classifier approach as opposed to the reduction approach detailed by OAA/OAO

- Reduction vs. Single-Classifier

  - Reduction is future-proof and easy to implement
  - Single classifier leads to global optimization by directly minimizing the loss

## Computational Learning Theory

- Concept Space vs. Hypothesis Space

  - Concept space is the set of all possible target functions
  - The simplest case is when concept space = hypothesis space

- PAC Learning

  - Stands for "Probably Approximately Correct Learning"
  - Intuition:
    - If a concept is reasonable, after some amount of samples, it is unlikely to see points that would get predicted incorrectly
    - If a concept is too complex, the number of samples needed to eliminate incorrect predictions is exponential

- Hypothesis Error

  - The error of a hypothesis is defined by the probability that the learned function predicts wrong
  - It is possible to have a learned model that is consistent with the training data, but still makes mistakes

- PAC Learning for Monotone Conjunctions

  - Consider the concept/hypothesis space are both monotone conjunctions with `n` variables

  - With probability `1 − 𝛿`, how many examples are needed to achieve an error rate `< 𝜖`?

  - To achieve the target error rate, it is sufficient to ensure the probability of each "bad literal" appearing is less than that error rate

  - Theorem:

    - For a monotone conjunctive concept with `n`-dimensional Boolean features and `m` training examples, he error of the learned hypothesis will be less than `𝜖` if:

      - $$
        m>\frac{n}{\epsilon}\left(\log n+\log\left(\frac{1}{\delta}\right)\right)
        $$

- PAC Learnability

  - The concept class `C` is PAC learnable by a learner `L` using hypothesis space `H` if for all `f ∈ C`, for all distributions `D` over instance space `X`, and fixed `𝜖 > 0`, `d < 1`, given `m` examples sampled IID according to `D`, the algorithm `L` produces, with probability at least `(1 - d)`, a hypothesis `h ∈ H` that has error at most `ε`, where `m` is polynomial in `1 / ε`, `1 / d`, `n`, and `size(H)`
  - The concept class `C` is *efficiently learnable* if `L` can produce the hypothesis in time that is polynomial in `1 / ε`, `1 / d`, `n`, and `size(H)`
  - Limitations:
    - Polynomial *sample complexity*: is there enough information in the sample to distinguish a hypothesis `h` that approximates `f`?
    - Polynomial *time complexity*: is there an efficient algorithm that can process the sample and produce a good hypothesis `h`?

## Kernel Methods

- Main Idea

  - Take data that is not linearly separable in one dimension and map it to another dimension where is is linearly separable

- Mapping Function

  - Use some mapping function `𝜙(·)` to map from old vector space to new vector space

  - For Perceptron, the weight vector `w` can be rewritten as:

    - $$
      \bold{w}^T\phi(\bold{x})=\sum_{1\ldots m}\alpha_iy_i\phi(\bold{x}_i)^T\phi(\bold{x})
      $$

- Kernel Function

  - $$
    K(\bold{x},\bold{z})=\phi(\bold{x})^T\phi(\bold{z})
    $$

  - Perceptron prediction becomes:

    - $$
      \text{sgn}(\bold{w}^T\phi(\bold{x}))=\text{sgn}\left(\sum_i\alpha_iy_iK(\bold{x}_i,\bold{x})\right)
      $$

  - Would be much more efficient if we can compute `K(x, z)` without needing to compute `𝜙(x)` and `𝜙(z)`

- The Kernel Trick

  - Idea is to save time/space by computing the value of `K(x, z)` by performing operations in the original space

  - A kernel function must satisfy:

    - $$
      K(\bold{x}_m,\bold{x}_n)=K(\bold{x}_n,\bold{x}_m)\text{ and }K(\bold{x}_m,\bold{x}_n)=\phi(\bold{x}_m)^T\phi(\bold{x}_n)
      $$

## Support Vector Machines

- Motivation

  - Expand Perceptron by introducing a margin requirement `𝛾`
  - Goal is to find the highest `𝛾` possible in a given dataset

- Hard SVM

  - Margin is formed by:

    - $$
      -1<(\bold{w}^T\bold{x}_i+b)<1
      $$

    - The distance to each margin is then given by:

      - $$
        \frac{1}{||\bold{w}||}
        $$

  - No data points are allowed within the margin:

    - $$
      \forall_i,\quad y_i(\bold{w}^T\bold{x}_i+b)\ge1
      $$

    - No training error can be made

- Soft SVM

  - Allows some examples to break into the margin

  - Introduces a slack variable `𝜉` per example:

    - $$
      \forall_i,\quad y_i(\bold{w}^T\bold{x}_i+b)\ge1-\xi_i\text{ and }\xi\ge0
      $$

    - Allows examples to exist within the margin

    - If the slack variable is 0, then the example is either on or outside the margin

- Loss Function

  - For hard SVM:

    - $$
      \arg\min_{w,b}\frac{1}{2}\bold{w}^T\bold{w}
      $$

  - For soft SVM:

    - $$
      \arg\min_{w,b,\xi_i}\frac{1}{2}\bold{w}^T\bold{w}+C\sum_i\max(0,1-y_i(\bold{w}^T\bold{x}_i+b))
      $$

    - The first term is the regularization term, which maximizes the margin

      - In general, regularization terms impose a preference over the hypothesis space to push for better generalization (higher margin, simpler model, etc.)

    - The second term is the empirical loss term, which penalizes weight vectors that make mistakes using hinge loss

      - Compare hinge loss to 0-1 loss, which only penalizes *incorrect* predictions
      - In general, other types of losses can be used here

    - `C` is a hyperparameter that defines the tradeoff between the regularization and the loss

- Training

  - Hinge loss is not differentiable, due to the hinge at 1 => use sub-derivatives instead

- Perceptron vs. SVM

  - Perceptron optimizes a different loss (Perceptron loss) and has no regularization term
  - SVM optimizes the hinge loss with regularization that maximizes the margin

- Primal vs. Dual

  - The SVM objective's primal form is as follows:

    - $$
      \arg\min_{\bold{w},b,\xi_i}\frac{1}{2}\bold{w}^T\bold{w}+C\sum_i\xi_i
      $$

  - The primal form can be rewritten into the dual form:

    - $$
      \arg\max_\alpha\sum_i\alpha_i-\frac{1}{2}\sum_{i,j}\alpha_i\alpha_jy_iy_j\bold{x}_i\bold{x}_j
      $$

- Support Vectors

  - From the dual form, we can conclude that the weight vector is completely defined by training examples whose `𝛼`s are not zero => the support vectors
  - Graphically, support vectors are examples that fall either within or on the margin

## Bayesian Learning

- Bayes' Theorem

  - $$
    P(Y\ |\ X)=\frac{P(X\ |\ Y)P(Y)}{P(X)}
    $$

  - `P(Y)` is the prior probability: the probability of `Y` occurring before seeing `X`

  - `P(X | Y)` is the likelihood of observing `X` given a specific `Y`

  - `P(Y | X)` is the posterior probability: the probability of `Y` given that `X` is observed

- Probabilistic Learning

  - Bayesian Learning: the use of probabilistic criterion in selecting a hypothesis, which can be deterministic
  - Learning probabilistic concepts: the learned concept can be interpreted as the probability that the label `1` is assigned to `x`

- Basics

  - Goal is to find the *best* hypothesis from some hypothesis space `H`, using the observed data `D`

    - "Best" means the most probable hypothesis in `H`
    - We assume a probability distribution over the class `H`

  - $$
    P(h\ |\ D)=\frac{P(D\ |\ h)P(h)}{P(D)}
    $$

- Maximum a Posteriori Hypothesis

  - $$
    h_{MAP}=\arg\max_{h\in H}P(D\ |\ h)P(h)
    $$

  - The posterior probability is proportional to the product of the likelihood and the prior

  - Most likely value of `p` is then dependent on choice of prior

- Maximum Likelihood Hypothesis

  - If we assume that the prior `P(h)` is uniformly distributed, then we get:

    - $$
      h_{ML}=\arg\max_{h\in H}P(D\ |\ h)
      $$

  - As usual, it's often easier to optimize the log-likelihood

  - $$
    p_{\text{best}}=\frac{a}{a+b}
    $$

    - The intuitive answer

- MAP Prediction

  - Uses Bayes' Theorem to predict `y` given an input `x`:

    - $$
      \arg\max_yP(X\ |\ Y)P(Y)
      $$

- Difficulty of Learning Probabilistic Models

  - Prior: if there are `k` labels, we need `k - 1` parameters

  - Likelihood: for each `y`, we need a value for each possible:

    - $$
      P(x_1,x_2,\ldots,x_d\ |\ y)
      $$

    - Assuming `k` labels, the number of parameters needed is:

      - $$
        (|x_1|\times|x_2|\times\cdots\times|x_d|-1)\times k
        $$

  - Use independence assumptions to reduce the number of parameters needed

- Naïve Bayes

  - Assumption:

    - All features are conditionally independent given the label

    - $$
      P(x_1,x_2,\ldots,x_d\ |\ y)=P(x_1\ |\ y)P(x_2\ |\ y)\cdots P(x_d\ |\ y)
      $$

    - Reduces the number of parameters needed for the likelihood to:

      - $$
        (d-1)k
        $$

  - Decision Boundary:

    - NB is a linear function of the feature space => NB has a linear decision boundary

  - Learning:

    - Count how often features occur with each label, normalize to get likelihoods
    - Priors are from the fraction of examples with each label

  - Prediction:

    - Use learned probabilities to find the highest scoring label

  - Caveats:

    - In practice, probabilities are not always conditionally independent
    - Enough training data is needed to get good estimates of the probabilities from counts

## Clustering

- Goal

  - Organize given unlabeled data into sensible groups

- K-Means

  - Algorithm:
    - Randomly assign cluster centers
    - Minimize the total distances between cluster members and centers by assigning members to clusters
    - Minimize the total distances between cluster members and centers by setting cluster centers to the average of their members
    - Loop until convergence
  - Properties:
    - The cluster center may not be in the training set
    - `K` needs to be pre-defined
    - The procedure either improves or stays the same on each iteration
    - Converges in, at worst, exponential to the number of data points
    - Solves for a local minimum
    - Sensitive to outliers
  - Choosing `K`:
    - Increasing `K` will always decrease the optimal value of the K-means objective
      - Not always good => overfitting

- K-Medoids

  - Algorithm:
    - Randomly assign cluster centers
    - Minimize the total distances between cluster members and centers by assigning members to clusters
    - Minimize the total distances between cluster members and centers by setting cluster centers to the data point that is closest to all other data points in the cluster
    - Loop until convergence
  - Properties:
    - More resilient to outliers

- Gaussian Mixture Models (GMM)

  - Algorithm:

    - Soft assign points to clusters based on posterior distribution

    - Update cluster centers/variances

      

