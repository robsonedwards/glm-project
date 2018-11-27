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

data <- read.csv("data/HTRU_2.csv", header=FALSE) #Change path 
summary(data)
summary(data[data$V9==T,])
summary(data[data$V9==F,])
set.seed(4607)
indices <- sample(1:nrow(data), 1000, replace = F)

# Some plots of the data
store <- par()$xpd # Store your xpd so it can be restored later
par(xpd = NA)
pairs(data[indices, 1:8], col = c("orange1", "cyan")[data[indices, 9] + 1],
       lower.panel = NULL#, panel = panel.smooth
      )
legend( "bottomleft", fill = c("orange1", "cyan"), 
        legend = c("non-pulsar", "pulsar") )
par(xpd = store) # Restore your xpd
rm(store)

# Note the collinearity issue: high correlation between (V3, V4) and (V7, V8). 
round(cor(data), 2)

# Fit a GLM with all 8 features and no interactions
X <- cbind(rep(1, nrow(data)), data[, 1:8])
Y <- data[, 9]
mdl <- glm(Y~., family = binomial, data = data[, 1:8])
summary(mdl)

# Accuracy and Confusion Matrix for this model as a classifier 
#   with cutoff p = 0.5
Ypred1 <- mdl$fitted.values > 0.5
print_model_details(Y, Ypred1)

#   with cutoff p = 0.4 
Ypred2 <- mdl$fitted.values > 0.4
print_model_details(Y, Ypred2)

#   plotting all cutoffs in the interval [0, 1]
accuracy <- Vectorize(function(cutoff){
  get_accuracy(Y, mdl$fitted.values > cutoff)
})
sensitivity <- Vectorize(function(cutoff){ # true positives / actual positives
  confusion <- get_confusion_matrix(Y, mdl$fitted.values > cutoff)
  confusion[2, 2] / confusion[3, 2]
})
specificity <- Vectorize(function(cutoff){ # true negatives / actual negatives 
  confusion <- get_confusion_matrix(Y, mdl$fitted.values > cutoff)
  confusion[1, 1] / confusion[3, 1]
})

plot(accuracy, 0, 1, lwd = 2, col = "orange1", xlab = "cutoff", ylab = "", 
     ylim = c(0.6, 1))
plot(sensitivity, 0, 1, add = T, lwd = 2, col = "cyan")
plot(specificity, 0, 1, add = T, lwd = 2, col = "green2")
legend(0.15, 0.8, legend=c("accuracy", "sensitivity", "specificity"),
       col=c("orange1", "cyan", "green2"), lwd = 2)
cutoff <- unlist(optimize(accuracy, c(0.1, 0.8), maximum = T)["maximum"])
points(cutoff, accuracy(cutoff))
text(0.4, 0.95, paste(round(cutoff, 3), ", ", round(accuracy(cutoff), 3), 
                      sep = ""))

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
#   Removing V3 and 
mdl5 <- glm(Y~.-V3-V7, family = binomial, data = data[,1:8])
summary(mdl5)

# Fitting larger GLMs by adding interaction terms
mdl5 <- glm(Y~.*., family = binomial, data = data[,1:8])
summary(mdl5)

# Only significant interaction terms from the above: AIC 2478
mdl6 <- glm(Y~.+V1*V2+V1*V3+V2*V3+V2*V4+V5*V6+V6*V7,
            family = binomial, data = data[,1:8])
summary(mdl6)


mdl7 <- stepAIC(mdl5, direction = "both",  trace = F)
summary(mdl7)
