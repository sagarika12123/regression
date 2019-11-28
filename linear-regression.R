##k means clustering 

library(datasets)
head(iris)
View(iris)
library(ggplot2)
ggplot(iris,aes(Petal.Length,Petal.Width,color=Species))+geom_point()
#clustering
set.seed(20)
icluster <- kmeans(iris[,3:4],3,nstart=20)
icluster
table(icluster$cluster,iris$Species)
icluster$cluster <- as.factor(icluster$cluster)
ggplot(iris,aes(Petal.Length,Petal.Width,color=iris$Species))+geom_point()
##now let us try to run this on a different data set 

data <- read.csv("master.csv",header=TRUE)

View(data)
library(ggplot2)
ggplot(data,aes(data$suicides.100k.pop,data$gdp_per_capita....,color=data$country))+geom_point()
#clustering
set.seed(20)
mcluster <- kmeans(data[,2:3],101,nstart=20)
mcluster
table(mcluster$cluster,data$country)
mcluster$cluster <- as.factor(mcluster$cluster))
ggplot(data,aes(data$suicides.100k.pop,data$gdp_per_capita....,color=data$cluster))+geom_point()


##regression analysis 
library(readxl)
lmsuicide <- lm(data$suicides.100k.pop~data$gdp_per_capita....)
summary(lmsuicide)

##now let us check if gender has a impact on the regression analysis 
lmsuicide1 <- lm(data$suicides.100k.pop~data$gdp_per_capita....+data$sex+data$country+data$year+data$age)
summary(lmsuicide1)
ggplot(data,aes(data$suicides.100k.pop,data$gdp_per_capita....,color=data$country))+geom_point()

##remove the countried with null hypothesis rejected and see if the regression curve fits best


