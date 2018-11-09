HTRU_2 <- read.csv("~/Downloads/HTRU2/HTRU_2.csv", header=FALSE)
View(HTRU_2)
summary(HTRU_2)
summary(HTRU_2[HTRU_2$V9==T,])
summary(HTRU_2[HTRU_2$V9==F,])
mdl <- glm(HTRU_2[,9]~., family=binomial, data=HTRU_2[,1:8])
summary(mdl)

X <- cbind(rep(1, 17898), HTRU_2[, 1:8])
Y <- HTRU_2[, 9]
linpred <- as.matrix(X) %*% mdl$coefficients
nlpred <- exp(linpred)/(1+exp(linpred))
Ypred <- nlpred>0.5
length(Ypred)
sum(Ypred==Y)
sum(Ypred & Y)
sum(!Ypred & Y)
sum(Ypred & !Y)
sum(!Ypred & !Y)