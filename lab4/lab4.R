f <- read.csv("Toy_sales_csv.csv")
l1<-lm(Unitsales~Price,f)
s1<-summary(l1)
print(s1)
library(ggplot2)
p<-ggplot(f,aes(Price,Unitsales))+geom_point() +geom_smooth(method = "lm",formula = y~x,col="red",se=F)
print(p)
pred <-predict(l1)
cat("predicted values:- ",pred)
err <- f$Unitsales-pred
cat("\nerror values:- ",err)
l2<-lm(Unitsales~Price+Adexp+Promexp,f)
s2<-summary(l2)
print(s2)
df <- data.frame(Price=c(9.1,8.1),Adexp=c(52,50),Promexp=c(61,60))
pred1<-predict(l2,df)
data<- cbind(df,pred1)
print(data)