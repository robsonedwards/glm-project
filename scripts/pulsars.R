require(MASS)
require(ggplot2)
require(car)
data <- read.csv("data/HTRU_2.csv", header=FALSE) #Change path for your system

#### Functions #################################################################
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
#### Exploratory Data Analysis #################################################

summary(data)
summary(data[data$V9==T,])
summary(data[data$V9==F,])
set.seed(4607)
indices <- sample(1:nrow(data), 1000, replace = F)
# See also plotting code 

# Note the collinearity issue: high correlation between (V3, V4) and (V7, V8). 
round(cor(data), 2)

Y <- data[, 9]

#### Fit simple GLMs ###########################################################
# Fit a GLM with all 8 features and no interactions
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

# Fit smaller GLMs by removing some of V3, V7, V8 
#   Removing V8 gives a model with AIC 2634.3
mdl1_v8 <- glm(Y~., family = binomial, data = data[,1:7])
summary(mdl1_v8)
#   Removing V7 gives a model with AIC 2632.1
mdl2 <- glm(Y~.-V7, family = binomial, data = data[,1:8])
summary(mdl2)
#   Removing V3 and V7 should give a better model...
#     but it leads to numerical issues. 
badmdl1 <- glm(Y~.-V3-V7, family = binomial, data = data[,1:8])
summary(badmdl1)
#   Even removing only V3 has this issue. 
badmdl2 <- glm(Y~.-V3, family = binomial, data = data[,1:8])
summary(badmdl2)
#     Diagnostics... 
print_model_details(Y, badmdl2$fitted.values > 0.5)
badmdl2$fitted.values[which(duplicated(badmdl2$fitted.values))]
sum(badmdl2$fitted.values[which(duplicated(badmdl2$fitted.values))])
badmdl2$linear.predictors[which(duplicated(badmdl2$fitted.values))]
min(badmdl2$fitted.values[as.logical(Y)])
max(badmdl2$fitted.values[as.logical(1-Y)])
#   I believe that the issues arise because 11 observations are fitted to a
#     value which is numerically equal to 1. I am not sure how to fix this
#     problem.
mdl3 <- glm(Y~.-V7-V4, family = binomial, data = data[,1:8])
summary(mdl3)

#   Stepwise selection finds the same model as mdl3
if(F) stepAIC(mdl, direction = "both",  trace = F)

#### Fit larger GLMs by adding interaction terms ###############################
# Fit a model with every interaction term. AIC 2466.8
mdl4 <- glm(Y~.*., family = binomial, data = data[,1:8])
summary(mdl4)

# Stepwise model selection with interactions. Takes a while to run.
if(F){ #Change to T to run this. 
  mdl5 <- stepAIC(mdl3, direction = "both",  trace = F)
  summary(mdl5)
}

#### Model Comparison #####
for(model in list(mdl, mdl2, mdl3, mdl4, mdl5)){
  accuracy <- Vectorize(function(c){
    get_accuracy(Y, model$fitted.values > c)
  })
  cutoff <- unlist(optimize(accuracy, c(0.1, 0.8), maximum = T)["maximum"])
  confusion <- get_confusion_matrix(Y, model$fitted.values > cutoff)
  ac <- round(get_accuracy(Y, model$fitted.values > cutoff), 3)
  sp <- round(confusion[1, 1] / confusion[3, 1], 3)
  se <- round(confusion[2, 2] / confusion[3, 2], 3)
  confusion <- get_confusion_matrix(Y, model$fitted.values > 0.5)
  ac5 <- round(get_accuracy(Y, model$fitted.values > cutoff), 3)
  sp5 <- round(confusion[1, 1] / confusion[3, 1], 3)
  se5 <- round(confusion[2, 2] / confusion[3, 2], 3)
  print(paste(
              round(model$aic, 1), round(max(unname(vif(model))), 2), ac5, sp5, se5, 
              ac, sp, se, round(cutoff, 3),
              sep = " & "))
}

