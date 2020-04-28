# Chapter 6 - Kernel Smoothing Methods

## Worked examples & proofs
* [Derivation of Variance](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/derivations/variance.pdf) - Deriving the variance for local polynomial regression as described in Section 6.1.2. 
* [Sherman-Morrison Formula](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/derivations/sherman-morrison.pdf) - Proving the Sherman-Morrison formula as used in my solution to exercise 6.7.

## Code implementations

* [Recreating Figure 6.1](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/code/figure_6.1.R) - Applying 30-nearest-neighbours smoother (left) and a kernel-weighted average using an Epanechnikov kernel (right) to synthetically generated data. 
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/images/figure_6.1.png"  width="800">

* [Recreating Figure 6.3](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/code/figure_6.3.R) - Demonstrating local linear regression reducing bias, particularly near to the boundary, when compared to Nadaraya–Watson kernel-weighted average.
<img src="https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/images/figure_6.3.png"  width="800">

* [Local Linear Discriminant Analysis](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/code/llda.pdf) - Performing Local Linear Discriminant Analysis on the zipcode data for exercise 6.12. This is not an efficient LLDA implementation and took ~24 hrs to run on my raspberry pi 4. The results indicate that a local approach improves performance over regular LDA. <br />
| Model | Accuracy | 
| :--------------: | :----------: | 
| LLDA -> λ = 18 | 0.93 | 
| LLDA -> λ = 20 | 0.91 | 
| LLDA -> λ = 25 | 0.9 | 
| LLDA -> λ = 30 | 0.9 | 
| LLDA -> λ = 50 | 0.89 | 
| LDA | 0.89 |


## Exercises
* [Exercise 6.1](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.1.pdf)
* [Exercise 6.2](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.2.pdf)
* [Exercise 6.5](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.5.pdf)
* [Exercise 6.6](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.6.pdf)
* [Exercise 6.7](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.7.pdf)
* [Exercise 6.8](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.8.pdf) 
* Exercise 6.9 - Need to cover GAM's in Chapter 9 first in order to complete this question.
* [Exercise 6.10](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.10.pdf)
* [Exercise 6.11](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/exercises/exercise_6.11.pdf)
* [Exercise 6.12](https://github.com/alanjeffares/elements-of-statistical-learning/blob/master/chapter-6/code/llda.pdf)



## References
 Loader, C. (1999) _Local Regression and Likelihood_
