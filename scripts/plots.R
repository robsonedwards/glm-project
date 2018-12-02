data <- read.csv("data/HTRU_2.csv", header=FALSE)
indices <- sample(1:nrow(data), 200, replace = F)

#### Functions ####
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
#### Plotting ####

# Scatterplot matrix of V1-V8 with different colors for pulsars 
store <- par()$xpd # Store your xpd so it can be restored later
#pdf(file = "figures/scattermatrix.pdf", width = 8, height = 8)
par(xpd = NA)
for(v in 5:8){print(min(data[, v]))}
data2 <- data %>% transform(V7 = pmax(V7, 0), V8 = pmax(V8, 0))
for(v in 5:8){print(min(data2[, v]))}
data2 <- data2 %>% transform(LV7 = log(V7), LV8 = log(V8)) %>% select(-V7, -V8)
data2 <- data2[ , c(1:6, 8, 9, 7)]
pairs(data2[indices, 1:8], col = c("orange1", "cyan")[data2[indices, 9] + 1],
      lower.panel = NULL#, panel = panel.smooth
)
legend( "bottomleft", fill = c("orange1", "cyan"), 
        legend = c("non-pulsar", "pulsar") )
par(xpd = store) # Restore your xpd
rm(store)

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

Y <- data[, 9]
mdl <- glm(Y~., family = binomial, data = data[, 1:8])

pdf(file = "figures/cutoffs.pdf", width = 5, height = 5)
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
dev.off()

# Plotting for model 4
mdl <- glm(Y~., family = binomial, data = data[, 1:8])

pdf(file = "figures/cutoffs4.pdf", width = 5, height = 5)
plot(accuracy, 0, 1, lty = 2, col = "grey", xlab = "cutoff", ylab = "", 
     ylim = c(0.75, 1))
plot(sensitivity, 0, 1, add = T, lty = 2, col = "grey")
plot(specificity, 0, 1, add = T, lty = 2, col = "grey")
mdl <- glm(Y~.*., family = binomial, data = data[,1:8])
plot(accuracy,    0, 1, add = T, lwd = 2, col = "orange1")
plot(sensitivity, 0, 1, add = T, lwd = 2, col = "cyan")
plot(specificity, 0, 1, add = T, lwd = 2, col = "green2")
legend(0.11, 0.82, legend=c("accuracy", "sensitivity", "specificity"),
       col=c("orange1", "cyan", "green2"), lwd = 2)
cutoff <- unlist(optimize(accuracy, c(0.1, 0.8), maximum = T)["maximum"])
points(cutoff, accuracy(cutoff))
text(0.5, 0.97, paste(round(cutoff, 3), ", ", round(accuracy(cutoff), 3), 
                      sep = ""))
points(0.048, accuracy(0.048))
text(0.2, 0.93, paste(round(0.048, 3), ", ", round(accuracy(0.048), 3), 
                       sep = ""))
dev.off()

