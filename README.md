

# Linear Regression Model Estimation and Selection

##### Elena Badillo Goicoechea, June 10, 2019

## Descripton

This project consists of a purely functional programming implementation of multivariate linear model 
estimation using: i) gradient descent,  ii) greedy best model selection

The pipeline of this project consists of the following steps:

	1. Take args provided by the user [ i) json filepath, ii) no. of iterations, iii) task, iv) scaling favtor]
	2. Read file and load data
	3. Parse data into appropriate data type
	4. Execute required task
	5. Print results

The Main module takes args provided by user via command line, reads the data into required form, and executes required algorithms.

The DoLinReg module implements gradient descent algorithm to fit a linear regression model to a given dataset.

The RegModels module implements tasks consisting of different model selection algorithms, the last three of which optimize 
over adjusted R2:

	1) Produce a list of all univariate models ( one for each predictor variable)
	2) Estimate a model including the set of all predictors, X
	3) Select the best possible bivariate model
	4) Greedily select the best k-variate model for each k = 1,..., |X|
	5) Greedily select the best model from all combinations

The RTypes module contains all the data type signatures and instances necessary for the implementation, namely:

	- Coefs
	- Record
	- DataSet
	- Model

## Dataset

The assuptions made by the program, regarding the dataset provided by the user are the following:

	- All variables are numeric
	- JSON format
	- Target variable (y) corresponds to the last field in each record
	- All records have the same size

To test the program, a sample dataset (student.json) is included in the folder. The dataset comes from UCI Machine Learning repository and contains data on Portuguese student achievement in secondary education (target variable) and various potential predictors.
Source: https://archive.ics.uci.edu/ml/datasets/Student+Performance

## Advanced Features 

DataSet and Record data types are made instances of Monoid, and Semigroup, and Functor. Functor instance is currently used 
(for both data types, jointly) for scalling the dataset provided by the user as input, by the factor also provided by the user as input.

Potential uses for the Monoid and Semigroup include i) merging different datasets, and ii) managing the case of having different sized entries in
the json file provided by the user (with the benefit of requiring less assumptions from it, with respect to those stated above, and thus making the 
program more flexible).

Further use of these features, combining them with the addition of a Monad instance for Model and its components could allow to parallelize gradient descent algorithm, for a more efficient implementation.

## References:

	- https://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell
	- http://mccormickml.com/2014/03/04/gradient-descent-derivation/
    - https://www.classes.cs.uchicago.edu/archive/2018/fall/12100-1/pa/pa5/index.html
    - https://samcgardner.github.io/2018/10/06/linear-regression-in-haskell.html





