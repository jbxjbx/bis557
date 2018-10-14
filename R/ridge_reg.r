#' Fit a ridge regression model
#'
#' @description this function passes parameters to the ridge function.
#' @param formula a formula
#' @param lambda a parameter in numberic values
#' @param data a data.frame
#' @return An ridge regression object
#' @importFrom stats model.matrix 
#' @examples
#' fit <- ridge_reg(Sepal.Length ~.,1.2, iris)
#' summary(fit)
#' @export

# reference to the code taught in class by Prof. Kane
ridge_reg <- function(formula, lambda, data) {
  rownames(data) <- NULL
  m <- model.matrix(formula, data)
  y <- matrix(data[,as.character(formula)[2]],ncol=1)
  y <- y[as.numeric(rownames(m)),,drop = FALSE]
  
  # then fit directly by solve the matrix via svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  
  #return the values
  ret <- list(coefficients = beta, lambda = lambda, formula=formula)
  class(ret) <- "ridge_reg"
  ret
}
