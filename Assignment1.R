#############         MANIPULATION
#Installing and loading the package ggplot2

#install.packages("ggplot2")
library("ggplot2")

#loading the dataset diamonds
data(diamonds)

#using help() and str() to access to the description of the dataset
help(diamonds)
str(diamonds)

#Creating a dataframe bestColor

bestColor_prep <- diamonds[diamonds$color == "D", ]
bestColor<- bestColor_prep[order(bestColor_prep$carat, bestColor_prep$price, decreasing=c(TRUE, TRUE)), ]
 
#Deleting column $color and saving the dataframe in txt file
bestColor <- bestColor[,-3]
write.table(bestColor, "Nina.txt")

dim(bestColor) # shows the number of obs. and the number of variables


#Creating a function countESI2

countESI2 <- function(n) {
  numESI2 <- 0
  for (i in 1:n) {
    if ((diamonds$color[i] == "E") & (diamonds$clarity[i] == "SI2")) 
      numESI2 <- numESI2 + 1
  }
  return(numESI2)
}
countESI2(50) #the results for n = 50 and n = 150.
countESI2(150)

#Creating a faster function countESI2
countESI2_fast <- function(n) {
  data <- diamonds[1:n,]
  dataESI2 <- subset(subset(data, color == "E"), clarity == "SI2")
  return(nrow(dataESI2))
}
countESI2_fast(50)
countESI2_fast(150)

#alternatively countESI2_fast2
#countESI2_fast2 <- function(n) {
# data <- diamonds[1:n,]
#  return(sum((data$color =="E")&(data$clarity == "SI2")))
#}
#countESI2_fast2(50)
#countESI2_fast2(150)



############        ANALYSIS

####### 1. Size of the most expensive diamond

index_exp <- which(diamonds$price == max(diamonds$price)) # index of the most expensive diamond
size_exp <- c(diamonds$x[index_exp],diamonds$y[index_exp],diamonds$z[index_exp])
size_exp # size of the most expensive diamond- length, width and depth in mm

### Alternative way:
# sset <- subset(diamonds, price == max(diamonds$price))
# size_exp <- c(sset$x, sset$y, sset$z)
# size_exp

####### 1. Size of the diamond which costs 1344 USD

index_cost <- which(diamonds$price == 1344) # index of the diamond which costs 1344
size_cost <- c(diamonds$x[index_cost],diamonds$y[index_cost],diamonds$z[index_cost])
size_cost # size of the diamond which costs 1344

####### 2. The 7 most expensive prices for diamonds of Ideal cut

diamonds_ideal <- subset(diamonds, cut == "Ideal") # creating a new dataframe with only Ideal cut diamonds
diamonds_ideal <- diamonds_ideal[order(diamonds_ideal$price, decreasing = TRUE ),] #sorting by price
best7 <- diamonds_ideal[1:7,]
best7$price
####### 3. Number of diamonds of Idealcut, best color and best clarity

nrow(subset(subset(diamonds_ideal, clarity == "IF"), color == "D"))
# or sum((diamonds$cut == "Ideal")&(diamonds$clarity == "IF")&(diamonds$color == "D"))
# or nrow(diamonds[which((diamonds$cut == "Ideal")&(diamonds$clarity == "IF")&(diamonds$color == "D")),])

####### 4. Creating a table of the frequency of the clarity and the color

freq <- table(diamonds$clarity, diamonds$color)
freq
which(freq == max(freq), arr.ind = TRUE)  #finding the most common combination of clarity and color

######## 5. The probability that a randomly chosen diamond is of color F and VS1 clarity

prob_dist <- round(freq/sum(freq), digits = 4)
prob_dist["VS1", "F"]

#############         CREATIVITY

mean(diamonds$price)
median(diamonds$price) #median < mean, the the distribution of the price is positively skewed  
par(mar = c(4,7,2,2), mgp = c(3,0.5,0), las = 1) #parameters for graphic environment

#histogram of dimonds prices 
hist(diamonds$price, breaks = 10, main = "Histogram of the prices of diamonds",
     xlab = "Price($ USA)", xaxt = "n", col = "lightblue")
axis(1, at = pretty(diamonds$price, n = 10))

#percentage of diamonds with a price in the range [0,2000]
nrow(diamonds[diamonds$price <= 2000, ]) / nrow(diamonds)

# plotting frequencies of diamonds of different colors
barplot(height = table(data$color), main = "Number of diamonds by color", xlab = "Color", 
        ylab = "Number of diamonds", col = 1:7, ylim = c(0, 12000))

#adding new column that specifies price per carat for each diamond 
data <- diamonds
data$pricePerCarat <- data$price/data$carat
summary(data$pricePerCarat)

#for what cut, color and clarity price per carat is the most expensive?
tapply(data$pricePerCarat, data$cut, median)
tapply(data$pricePerCarat, data$color, median)
tapply(data$pricePerCarat, data$color, IQR)
tapply(data$pricePerCarat, data$clarity, mean)

#boxplot of prices per carat by color

boxplot(data$pricePerCarat~data$color, main = "Prices for different diamonds colors", xlab = "Color",
        ylab = "Price per carat", col = 1:7)
grid()
#boxplot of prices per carat by cut

boxplot(data$pricePerCarat~data$cut, main = "Prices for different diamonds cuts", xlab = "Cut",
        ylab = "Price per carat", col = 1:7)
grid()

#number of dimonds of different price categories
priceRange <- table(cut(round(data$pricePerCarat), c(1000,2000,4000,6000,10000,19000), 
                        labels = c("Low", "Medium_Low", "Medium_High", "High", "Extra_High")))

# visualisation of priceRange
barplot(height = priceRange, 
        main = "Number of diamonds of different price categories",
        xlab = "Price per carat($USA)", col = 1:5, ylab = "Number of diamonds", ylim = c(0,30000), 
        names = c("1.000 - 2.000$", "2.001 - 4.000$","4.001 - 6.000$","6.001 - 10.000$","10.001 - 19.000$" ))

grid()

                    
                    
                      
                    

