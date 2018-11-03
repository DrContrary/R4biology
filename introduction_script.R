
#### Load Data and Look at Structure  --------------------------------------------------------------- 



#load one of the many data sets avalible in R
data(iris)
#other sets include mtcars - try that one at home
#do you want to know more about this dataset --- just ask
?iris

#look at the first couple rows
head(iris)
#what kind of structure is the iris data 
data.class(iris)
#how about the structure
str(iris)
#what are the column names
colnames(iris)
iris$Sepal.Length
#### Start with Some Basic Plots --------------------------------------------------------------- 

#lets try some plots with base R functions to vizualize
?plot
plot(iris$Sepal.Length, iris$Sepal.Width, type = "h")
?hist
hist(iris$Sepal.Length)
?boxplot
boxplot(iris$Sepal.Length, col="red")
boxplot(iris, col=heat.colors(5))
boxplot(iris, col=topo.colors(5))

#species doesnt have numerical values assocaited and doesnt make sense on a boxplot
#lets remove it
boxplot(iris[,1:4], col=heat.colors(2))

#why is do these two commands produce the same result?
plot(iris[,1:2])
plot(iris$Sepal.Length, iris$Sepal.Width)

#to vizualize patterns across the whole data set - try a scatter plot matrix
plot(iris)



#### Load in Data from Local Files ---------------------------------------------------------------

#load some data in from a local file
#change your working directory to where you have saved your file
setwd("C:/Users/Alyssa/Dropbox/R Group")
getwd()
dir()
#read in your file
read.csv("mtcars.csv")
#R is object oriented - so lets make this an object named cars

cars = read.csv("mtcars.csv", row.names = 1)
cars2 = cars[,1:5]

head(df)
#try some of the data inspection we did on iris to learn more about this
#try to find the column names
rownames(cars)

data.class(cars$cyl)
#lets make this a factor instead
cars$cyl = as.factor(cars$cyl)
data.class(cars$cyl)
#what are the factor levels now
levels(cars$cyl)


#install.packages("tidyverse")
library(ggplot2)

ggplot2::ggplot(iris, aes(x= iris$Sepal.Length, y=iris$Sepal.Width)) +
  geom_point()+
  labs(title = "this is a graph") +
  ylab ("Sepal Width")+
  xlab("Sepal Length")+
  theme_dark()


#### Running Statistical Tests ---------------------------------------------------------------

#this is a statistical programing language - so lets run some statistics 
t.test(cars$mpg, cars$disp)
t.test(cars$mpg, cars$hp)
cars$cyl = as.factor(cars$cyl)
levels(cars$cyl)
#lets run a one-way ANOVA to see if mpg changes with cyclinder 
#Compute the analysis of variance
res.aov <- aov(mpg ~ cyl, data = cars)
# Summary of the analysis
summary(res.aov)
#how about a post-hoc test to look for pairwise comparisons?
TukeyHSD(res.aov)

#plot your residuals against the values
#make sure you dont violate the test assumptions - ie equal variance
plot(res.aov, 1)
#what about normal distribution
plot(res.aov, 2)

# the plot looks pretty good - but lets run the numbers just to be sure
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )


