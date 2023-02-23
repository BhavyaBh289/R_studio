#case 2 
se <- 17/sqrt(35)
cat ("Standard error is :",se)
p <-pnorm(134,130,se,lower.tail = F)
cat ("P value is :",p)