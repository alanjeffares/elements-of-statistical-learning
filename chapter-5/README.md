# Chapter 5 - Basis Expansions and Regularization

## Worked examples & proofs
* [Derivation 5.2](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/derivations/derivation_5.2.pdf) - Proving that the linear piecewise constant basis expansions in section 5.2 are equivalent. 

## Code implementations
* [Recreating Figure 5.1](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/figure_5.1.R) - Using R's 'lm' function to fit splines from basis functions on data similar to that in Figure 5.1. <br />
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/images/figure_5.1.png"  width="800">

* [Recreating Figure 5.3](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/exercise_5.3.R) - Cubic splines pointwise variance can be erratic near the boundaries, however natural cubic splines can improve the situation. <br />
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/images/figure_5.3.png"  width="400">

* [Recreating Figure 5.5](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/figure_5.5.R) - Following the Phoneme example (Section 5.2.3) to use splines as a smooth regularisation technique. <br />
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/images/figure_5.5.png"  width="400">

* [Splines for Periodic Data](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/periodic_splines.R) - An example of fitting cubic splines to some synthetic periodic data. The green line is a single global cubic polynomial between 0 and T that is repeated indefinitely. The red line is a cubic spline with knots at T/3 and 2T/3 in the same range also repeated indefinitely. Both functions are constrained to be continuous at the boundaries. These basis are derived in [exercise 5.6](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.6.pdf).<br />
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/images/periodic_splines.png"  width="600">


## Exercises
* [Exercise 5.1](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.1.pdf)
* [Exercise 5.3](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/images/figure_5.3.png) - And [the code](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/exercise_5.3.R) to generate it. 
* [Exercise 5.4](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.4.pdf)
* [Exercise 5.5](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/code/exercise_5.5.R) - 12 knots distributed more densely on the first 100 frequencies achieved the lowest error rate at 7.7\%.
* [Exercise 5.6](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.6.pdf)
* [Exercise 5.7](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.7.pdf)
* [Exercise 5.9](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.9.pdf)
* [Exercise 5.12](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.12.pdf)
* [Exercise 5.13](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-5/exercises/exercise_5.13.pdf)

