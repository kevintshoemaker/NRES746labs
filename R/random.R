df=trees;responsevar="Volume"
RegressionCoefs <- function(df,responsevar){
  response <- df[,responsevar]
  names <- names(df)
  coef <- numeric(length(names))
  names(coef) <- names(df)
  coef <- coef[names(coef)!=responsevar]
  for(v in names(coef)){

  }
  model <- lm(response ~ ., data = df)
  return(summary(model)$coefficients)
}
