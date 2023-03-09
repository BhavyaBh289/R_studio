f <- read.csv("Hypothesis_csv1.csv")

#first case 

m  <-mean(f$Life_Hrs)
cat ("mean observed is :",m)
sd<-sd(f$Life_Hrs)
se <-sd/sqrt(50)
cat ("\nstandard error is :",se)
p <-pnorm(m,10000,se)
cat ("\npvalue for the 1st case is :",p)
if(p<0.05){
  cat ("\nclaim can be rejected for 0.05")
  
  
  
  
  
}else{
  cat ("\nclaim can not be rejected for 0.05")
}
if(p<0.01){
  cat ("\nclaim can be rejected for 0.01")
}else{
  cat ("\nclaim can not be rejected for 0.01")
}
#q =qnorm(0.05,m,sd) ## what can be the actual value 




#case 2 
se <- 17/sqrt(35)
cat ("\nfor second case \nStandard error is :",se)
p <-pnorm(134,130,se,lower.tail = F)*2
cat ("\nP value is :",p)
if(p<0.05){
  cat ("\nclaim can be rejected for 0.05")
}else{
  cat ("\nclaim can not be rejected for 0.05")
}
if(p<0.01){
  cat ("\nclaim can be rejected for 0.01")
}else{
  cat ("\nclaim can not be rejected for 0.01")
}