# pollutant data set
f <- read.csv("pollutant_csv.csv")
f
m <- mean(f$Temp[f$Month==6])
m

cat("\nMean of temp when month is 6: ",m)

n <- nrow(f)
cat("\nNumber of observations: ", n)

print(tail(f,2)) 

oz <- f$Ozone[47]
oz
cat("\nOzone in 47th row: ", oz)

ms <- sum(is.na(f$Ozone))
cat("\nNumber of missing values in Ozone column: ", ms)

mn <- mean(f$Ozone, na.rm = T)
cat("\nMean of Ozone excluding NA values: ", mn)

a <- mean(f$Solar.R[f$Ozone>31 & f$Temp>90], na.rm=T)
cat("\nMean of Solar.R when Ozone value is above 31 and Temp value is above 90: ", a)

b <- max(f$Ozone[f$Month==5], na.rm=T)
cat("\nMax of Ozone when month is 5:",b)
# Hair Eye color Data set ----
f2<-read.csv("dataset_LAB_1_hair_eye_color_csv.csv")
bec<-sum(f2$Eye.Color=="Brown")
cat("\nNo. of people having brown eye color:",bec)

bh<-sum(f2$Hair.Color=="Blonde")
cat("\nNo. of people having Blonde hair color:",bh)

bhbe<-sum(f2$Hair.Color=="Brown"&f2$Eye.Color=="Black")
cat("\nNo. of people having brown hair and black eyes:",bhbe)

green<-(sum(f2$Eye.Color=="Green")/nrow(f2))*100
cat("\nPercentage of people with green eyes:",green,"%")

rb<-(sum(f2$Hair.Color=="Red"&f2$Eye.Color=="Blue")/nrow(f2))*100
cat("\nPercentage of people have red hair and blue eye:",rb,"%")

# Germination Data Set ----
f3 <- read.csv("dataset_LAB_1_germination_csv.csv")
m1 <- mean(f3$germinated[f3$Box=="Uncovered" & f3$water_amt==4])
cat("\nMean when box is uncovered and water amount is 4: ", m1)

med <- median(f3$germinated[f3$Box=="Covered"])
cat("\nMedian value of covered boxes is:",med)




# Box Plot ----
library(ggplot2)
p <- ggplot(iris, aes(Sepal.Length, Species, fill=Species)) + geom_boxplot(outlier.color ="red", outlier.shape = 4, outlier.size = 6) + theme(legend.position = "none") + labs(x="SepalLength", y="Species", title="BoxPlot") + coord_flip()
print(p)

# Scatter Plot ----
library(dslabs)
p1<-ggplot(murders,aes(population/10*6,total,,label=abb))+geom_point(aes(col=region))+scale_x_log10()+scale_y_log10()+geom_text(nudge_x=0.055,size=3)+labs(x="POPULATION",y="TOTAL",title="SCATTERPLOT")
print(p1)