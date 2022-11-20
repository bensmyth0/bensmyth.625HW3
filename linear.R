linear <- function(y, x, data = NULL) {
  ## Returns the values of the regression coefficients for multiple linear regression
  ## y = name of the response variable in data
  ##     If data = NULL, then y will be taken as the vector of the response
  ## x = Vector of names of explanatory variable columns in data
  ##     If data = NULL, then x will be taken as the columns themselves
  
  #### LIMITATIONS
  ## No interactions          - have to code yourself
  ## No categorical variables - have to code yourself
  
  ## Differentiate between cases where data are specified vs not specified
  if (is.null(data) == TRUE) {
    X <- x
    Y <- y
  } else if (is.null(data) == FALSE) {
    data <- na.omit(data)
    X <- data[, x]
    Y <- data[, y]
  }
  Y <- as.matrix(Y)
  ## Save the model to be returned at the end
  model <- cbind(Y, X)
  if (is.null(data) == TRUE) {
    colnames(model)[1] <- "y"
  }
  
  ## Create the design matrix based on our explanatory variables
  ## Differentiate between Simple Linear Regression and Multiple Linear Regression
  if (is.null(dim(X)) == TRUE) {
    X <- as.matrix(cbind(rep(1, length(X)), X))
  } else {
    X <- as.matrix(cbind(rep(1, nrow(X)), X))
  }
  
  ## Compute the regression parameters
  ## This will compute the regression parameters for the specified model as well
  ## as nested models in order to determine Sums of Squares
  betaM <- matrix(rep(0, (dim(X)[2] -1) * dim(X)[2]), dim(X)[2], dim(X)[2]-1)
  
  for (i in 1:(dim(X)[2]-1)) {
    betaM[1:(i+1), i] <- solve(t(X[, 1:(i+1)]) %*% X[, 1:(i+1)]) %*% t(X[, 1:(i+1)]) %*% Y
  }
  beta <- as.matrix(betaM[, dim(betaM)[2]])
  if (is.null(data) == FALSE) {
    rownames(beta) <- c("Intercept", x)
  } else {
    rownames(beta) <- c("Intercept", paste("x", as.character(1:(dim(beta)[1]-1)), sep = ""))
  }
  ## Compute the fitted values of the linear regression
  fitsM <- matrix(rep(0, (dim(X)[2]-1) * dim(X)[1]), dim(X)[1], dim(X)[2]-1)
  for (i in 1:(dim(X)[2]-1)) {
    fitsM[, i] <- X[, 1:(i+1)] %*% betaM[1:(i+1), i]
  }
  fits <- fitsM[, dim(fitsM)[2]]
  
  ## Compute the residuals of the linear regression
  resM <- matrix(rep(Y, dim(fitsM)[2]), dim(fitsM)[1], dim(fitsM)[2]) - fitsM
  res <- Y - fits
  
  ## Compute the sums of squares of the fit
  SSE <- colSums(resM^2)
  SSR <- colSums((fitsM - mean(Y))^2)
  SST <- SSR + SSE
  SSq <- cbind(SSR, SSE, SST)[length(SSE), ]
  
  ## Compute the R-squared value of the fit
  R.sq <- SSR/SST
  
  #### Compute the ANOVA Table
  F.Test <- matrix(rep(NA, 5 * dim(X)[2]), dim(X)[2], 5)
  
  ## Determine the DF column
  F.Test[1:(dim(F.Test)[1]-1), 1] <- 1
  F.Test[dim(F.Test)[1], 1] <- dim(X)[1] - dim(F.Test)[1]
  
  ## Determine the Sum Sq Column
  F.Test[1, 2] <- SSR[1]
  if (dim(F.Test)[1] > 2) {
    F.Test[2:(dim(F.Test)[1]-1), 2] <- diff(SSR)
  }
  F.Test[dim(F.Test)[1], 2] <- SSE[length(SSE)]
  
  ## Determine the Mean Sq Column
  F.Test[, 3] <- F.Test[, 2] / F.Test[, 1]
  
  ## Return the F Test Statistics
  F.Test[1:(dim(F.Test)[1]-1), 4] <- F.Test[1:(dim(F.Test)[1]-1), 3] / F.Test[dim(F.Test)[1], 3]
  
  ## Return the p-values for the F test statistics
  F.Test[1:(dim(F.Test)[1]-1), 5] <- 1- pf(F.Test[1:(dim(F.Test)[1]-1), 4], 1, F.Test[dim(F.Test)[1], 1])
  
  ## Name all rows/cols
  if (is.null(data) == TRUE) {
    rownames(F.Test) <- c(paste("x", as.character(1:(dim(F.Test)[1]-1)), sep = ""), "Residuals")
  } else if (is.null(data) == FALSE) {
    rownames(F.Test) <- c(x, "Residuals")
  }
  colnames(F.Test) <- c("DF", "Sum Sq", "Mean Sq", "F Value", "p-value")
  
  ## Return all of the values computed above
  return(list(beta = t(beta), 
              fits = fits, 
              res = res,
              SSq = SSq,
              R.sq = R.sq,
              F.Test = F.Test,
              model = model))
}
