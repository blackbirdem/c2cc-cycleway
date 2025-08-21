library(dplyr)
library(MASS)

# set working directory
directory <- 'C:/Users/emmah/Documents/GitHub/c2cc-cycleway/data/survey/'
setwd(directory)

variables <- read.csv('regression_dependents.csv')
data <- read.csv('numeric_reduction_3.csv')
# remove redundant index columns
data <- data[,2:length(names(data))]

# for loop iterates over each dependent variable selected in variables table
for (i in 1:nrow(variables)) {
  m <- variables[i, ]
  # Y: ordinal outcome variable
  # X: predictor variables
  Yname <- m$dependent
  predictors <- names(m)[which(m=='x')]
  
  Y <- data[, Yname]
  X <- data[, which(names(data) %in% predictors)]
  
  # if binary outcome
  # use as.factor() for categorical/ordinal predictors if needed.
  if (m$type == 'bin') {
    model <- glm(as.factor(Y) ~ ., data = X, family = binomial)
    # if ordered outcome
    # use as.ordered() for ordinal variables.
  } else if (m$type == 'ord') {
    model <- polr(as.ordered(Y) ~ ., data = X, method = "logistic", Hess = TRUE)
  }
  summary(model)
  
  # store table
  ctable <- coef(summary(model))
  
  ## calculate and store p values
  if (m$type == 'bin') {
    test <- anova(model, test="Chisq")
    p <- test$`Pr(>Chi)`
  } else if (m$type == 'ord') {
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  }
  
  ## combined table
  (ctable <- cbind(ctable, "p_value" = p))
  
  ctable <- data.frame(ctable)
  
  ctable$significance <- lapply(
    ctable$p_value,
    function (x) ifelse(x < 0.01, 'highly significant',
                        ifelse(x < 0.05, 'significant', NA)))
  
  ctable$significance <- unlist(ctable$significance)
  # arrange alphabetically
  if (m$type == 'bin') {
    ctable <- arrange(ctable, significance, Estimate)
  } else if (m$type == 'ord') {
    ctable <- arrange(ctable, significance, Value)
  }
  
  names(ctable)[names(ctable) == 'Estimate'] <- 'Value'
  
  if (any(with(ctable,abs(ctable$Value) > 0.4)) && any(!is.na(ctable$significance))) {
    print(paste(Yname, 'is significant'))
  } else {
    print(paste(Yname, 'disregard'))
  }
  
  write.csv(ctable, paste('OLR_', Yname, '.csv', sep = ''))
}
