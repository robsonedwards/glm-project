require(MASS)

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
  print(get_confusion_matrix(Y, Ypred))
  print(paste("Accuracy:", get_accuracy(Y, Ypred)))
}
#### Exploratory Data Analysis ####

data <- read.csv("data/HTRU_2.csv", header=FALSE) #Change path 
summary(data)
summary(data[data$V9==T,])
summary(data[data$V9==F,])
set.seed(4607)
indices <- sample(1:nrow(data), 100, replace = F)
plot(data[indices, ])

# Fit a GLM with all 8 features and no interactions
X <- cbind(rep(1, nrow(data)), data[, 1:8])
Y <- data[, 9]
mdl <- glm(Y~., family = binomial, data = data[, 1:8])
summary(mdl)

#linpred <- as.matrix(X) %*% mdl$coefficients
#nlpred <- logistic(linpred)

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

# Fitting smaller GLMs by removing V7 and/or V8 
mdl2 <- glm(Y~., family = binomial, data = data[,1:7])
summary(mdl2)
mdl3 <- glm(Y~., family = binomial, data = data[,c(1:6, 8)])
summary(mdl3)
mdl4 <- glm(Y~., family = binomial, data = data[,1:6])
summary(mdl4)

# Fitting larger GLMs by adding interaction terms
mdl5 <- glm(Y~.*., family = binomial, data = data[,1:8])
summary(mdl5)

mdl6 <- glm(Y~.+V1*V2+V1*V3+V2*V3+V2*V4+V5*V6+V6*V7 ,
            family = binomial, data = data[,1:8])
summary(mdl6)

mdl7 <- stepAIC(mdl5, direction = "both",  trace = T)
summary(mdl7)
