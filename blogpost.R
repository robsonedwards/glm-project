spam <- t(data.frame(round(head(data), 3)))
str <- ""
i <- 0
for(cell in names(data)){
  str <- paste(str, cell, "|")
}
str <- paste(str, "\n")
for(cell in spam){
  i <- i + 1
  str <- paste(str, cell, "|")
  if(i %% 9 == 0){
    str <- paste(str, "\n")
  }
}
cat(str)
rm(i, str)
