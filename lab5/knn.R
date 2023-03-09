f <- read.csv("knn1_csv.csv")
euclid <- sqrt((3-f$x)**2+(2-f$y)**2)
data <-cbind(f,euclid)
data<-data[order(data$euclid),]


#NN

cat("class of p by NN is :",data[1,4])


#KNN for k = 5

df <- data[1:5,]
s1 <- sum(df$class==1)
s2 <- sum(df$class==2)
s3 <- sum(df$class==3)
cat("\nclass of p by KNN for k = 5  is : ")
if(s1>s2 & s1>s3)
  cat("1")
if(s2>s1 & s2>s3)
  cat("2")
if(s3>s1 & s3>s2)
  cat("3")


#RNN for r = 1.45

df <- data[data$euclid<1.45,]
s1 <- sum(df$class==1)
s2 <- sum(df$class==2)
s3 <- sum(df$class==3)
cat("\nclass of p by RNN for r = 1.45  is : ")
if(s1>s2 & s1>s3)
  cat("1")
if(s2>s1 & s2>s3)
  cat("2")
if(s3>s1 & s3>s2)
  cat("3")
