[![Build Status](https://travis-ci.org/jbxjbx/bis557.svg?branch=master)](https://travis-ci.org/jbxjbx/bis557)

BIS557
===
  
  This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

The first thing we've done is to create and document a function that
calls `lm`. You can use it like this:

```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```
The second thing we did is to create and document a function that calls "ridge_reg", which can output the coefficient that renerated by the ridge regression model. You can use it like this:

```{R}
library(bis557)
fit <- ridge_reg(Sepal.Length ~.,1.2, iris)
summary(fit)
```
The third thing is the homework in class, 

The fourth thing is also the homework, but we create a class called "sparse.matrix" that can give the out
put of the addition, multiplication, and transpose operations. 