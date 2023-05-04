library(class)
f<-read.csv("wbc_csv.csv")
f$diagnosis <- as.factor(f$diagnosis)
set.seed(123)
f<-f[sample(nrow(f)),]
n<-function(b){
  (b-min(b))/(max(b)-min(b))
}
fn<-f[,3:31]
fnor <- as.data.frame(lapply(fn,n))

train <- fnor[1:469,]
test <-fnor[470:569,]
train.label <- f[1:469,2]
test.label <- f[470:569,2]
p<-knn(train,test,train.label,test.label,k = 7)
print(p)
t <-table(actual=test.label,predicted =p)
print(t)

acc<-sum(diag(t))/sum(t)
cat("\nAccuracy= ",acc)

sen<- t[2,2]/ sum(t[2, ])
cat("\nSensitivity= ",sen)

prec<-t[2,2] / sum(t[ ,2])
cat("\nPrecision= ",prec)

spec<-t[1,1]/ sum(t[1, ])
cat("\nSpecificity= ",spec)
