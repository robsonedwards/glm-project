require(MASS)
require(ggplot2)
require(car)

#### Functions ####
logistic <- function(x) exp(x) / (1 + exp(x))

logit <- function(p) log(p / (1 - p))

get_confusion_matrix <- function(Y, Ypred){
  confusion <- rbind ( c(sum(!Ypred & !Y), sum(!Ypred & Y), sum(!Ypred)   ), 
                       c(sum(Ypred & !Y),  sum(Ypred & Y),  sum(Ypred)    ),
                       c(sum(!Y),          sum(Y),          length(Ypred) ) )
  confusion <- confusion / length(Ypred)
  rownames(confusion) <- c("Predicted False", "Predicted True", "")
  colnames(confusion) <- c("Actually False", "Actually True", "")
  return(confusion)
}

get_accuracy <- function(Y, Ypred){
  return( sum(Ypred == Y) / length(Ypred))
}

print_model_details <- function(Y, Ypred){
  confusion <- get_confusion_matrix(Y, Ypred)
  print(round(confusion, 3))
  print(paste("Accuracy:", round(get_accuracy(Y, Ypred), 3)))
  print(paste("Specificity:", round(confusion[1, 1] / confusion[3, 1], 3)))
  print(paste("Sensitivity:", round(confusion[2, 2] / confusion[3, 2], 3)))
}
#### Exploratory Data Analysis ####

data <- read.csv("data/HTRU_2.csv", header=FALSE) #Change path for your system
summary(data)
summary(data[data$V9==T,])
summary(data[data$V9==F,])
set.seed(4607)
indices <- sample(1:nrow(data), 1000, replace = F)

# Note the collinearity issue: high correlation between (V3, V4) and (V7, V8). 
round(cor(data), 2)

# Fit a GLM with all 8 features and no interactions
X <- cbind(rep(1, nrow(data)), data[, 1:8])
Y <- data[, 9]
mdl <- glm(Y~., family = binomial, data = data[, 1:8])
summary(mdl)

# Accuracy and Confusion Matrix for this model as a classifier 
#   with cutoff p = 0.5
print_model_details(Y, mdl$fitted.values > 0.5)
#   with cutoff p = 0.4 
print_model_details(Y, mdl$fitted.values > 0.4)
#   best cutoff
accuracy <- Vectorize(function(cutoff){
  get_accuracy(Y, mdl$fitted.values > cutoff)
})
cutoff <- unlist(optimize(accuracy, c(0.1, 0.8), maximum = T)["maximum"])
print_model_details(Y, mdl$fitted.values > cutoff)

# Fitting smaller GLMs by removing some of V3, V7, V8 
#   Removing V8 gives a model with AIC 2634.3
mdl2 <- glm(Y~., family = binomial, data = data[,1:7])
summary(mdl2)
#   Removing V7 gives a model with AIC 2632.1
mdl3 <- glm(Y~., family = binomial, data = data[,c(1:6, 8)])
summary(mdl3)
#   Removing V7 AND V8 gives AIC 2640.7. We have gone too far. 
mdl4 <- glm(Y~., family = binomial, data = data[,1:6])
summary(mdl4)
#   Removing V3 and V7 should give a better model...
#     but it leads to numerical issues. 
mdl5 <- glm(Y~.-V3-V7, family = binomial, data = data[,1:8])
summary(mdl5)
#   Even removing only V3 has this issue. 
mdl6 <- glm(Y~.-V3, family = binomial, data = data[,1:8])
summary(mdl6)
print_model_details(Y, mdl6$fitted.values > 0.5)
mdl6$fitted.values[which(duplicated(mdl6$fitted.values))]
sum(mdl6$fitted.values[which(duplicated(mdl6$fitted.values))])
mdl6$linear.predictors[which(duplicated(mdl6$fitted.values))]
min(mdl6$fitted.values[as.logical(Y)])
max(mdl6$fitted.values[as.logical(1-Y)])
#   This is because 11 observations are fitted to a value which is numerically
#     equal to 1. I am not sure how to fix this problem. 

#   Same as mdl3
stepAIC(mdl, direction = "both",  trace = F)

# Fitting larger GLMs by adding interaction terms
mdl7 <- glm(Y~.*., family = binomial, data = data[,1:8])
summary(mdl7)

# Only significant interaction terms from the above: AIC 2478
mdl8 <- glm(Y~.+V1*V2+V1*V3+V2*V3+V2*V4+V5*V6+V6*V7,
            family = binomial, data = data[,1:8])
summary(mdl8)

# Stepwise selection for the best model with interactions. Takes a while to run.
if(false){
  mdl9 <- stepAIC(mdl7, direction = "both",  trace = F)
  summary(mdl9)
}

mdl10 <- glm(Y~.*., family = binomial, data = data[,c(1:6, 8)])
summary(mdl10)

mdl11 <- stepAIC(mdl10, direction = "both",  trace = F)
summary(mdl11)
