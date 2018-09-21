#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @importFrom stats model.matrix terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export


linear_model <- function(formula, data) {
  #use matrix to get x and y values - reference to the help from TA session and Hongyu Li
  x= model.matrix(formula, data = data)
  char = all.vars(formula)
  y = data[,char[1]]
  
  #solve the predicted coefficients - reference to the R documentation
  coefficients = qr.coef(qr(x),y)
  
  #solve the fitted value use x and coefficients
  fitted.values = x %*% coefficients
  
  #solve residuals by making subtraction
  residuals = y - fitted.values
  
  #create the result in a list
  final = list(coefficients=coefficients, 
             residuals=residuals,
             fitted.values= fitted.values, 
             rank = ncol(x), 
             weights = NULL, 
             df.residual = nrow(x)-ncol(x), 
             call = call("linear_model", formula),
             terms = terms(x = formula, data = data), 
             contrasts = NULL, 
             xlevels = NULL, 
             offset = NULL, 
             y = y, 
             x = x, 
             model = formula, 
             na.action = NULL,
             qr = qr(x))
  
  #class the final list and return the result
  class(final)='lm'
  return(final)
}

#linear_model(Sepal.Length~.,iris)
