# loading the dataset
#可使用getwd():(在console輸入)來看目前的工作目錄
#或是點選上方選項的Tools->Global options來看目前工作目錄
data <- read.csv("DiamondsPrices.csv") 
#點開右邊工作區，Environment中的data

#install & library dplyr
install.packages("dplyr")
library(dplyr)

#select()
#A %>% B: A且B
carat_and_price_data <- data %>% select(carat,price) %>% head(5)
carat_and_price_data

#filter()
Above_300_USD_diamond <- data %>% filter(price>300) %>% head(5)
Above_300_USD_diamond

data %>% filter(price>300, cut == "Premium")
#R對字母大小寫敏感


### Practice for the function of select() & filter() ###

## List the diamond that its cut is "Premium" and show its clarity
data %>%filter(cut=="Premium") %>% select(cut, clarity) %>% head(5)

#mutate()
sumofxyz_data <- data %>% mutate(sumofxyz = x+y+z) %>% select(sumofxyz, price) %>% head(5)
sumofxyz_data

#arrange()
desc_list_the_carat_and_price <- carat_and_price_data %>% arrange(desc(price)) %>% head(5)
desc_list_the_carat_and_price


#summarise()
data %>% summarise(avgprice = mean(price), 顆=n())


#group_by()
data %>% group_by(color) %>% summarise(avgprice = mean(price), 顆=n())



### Practice 2 ###

## Add a new column which is calculated by the price divided by table 
## and classifying the data depends on its clarity
## and summarise the average of the new column(the price divided by table) with descending order
pri_div_tab <- data %>% mutate(price_devided_by_table=price/table) %>% group_by(clarity) %>% summarise(avgprice_table=mean(price_devided_by_table))%>% arrange(desc(avgprice_table)) 
pri_div_tab


## Find the diamond that its price is above 500 USD
## and list the cut and price with the ascending order
data %>% filter(price>500) %>% select(cut, price) %>% arrange(desc(price)) %>% head(5)


### End of introduction of dplyr ###



### Sampling distribution of R ###

## Assumption: the file is the population
population <- read.csv("DiamondsPrices.csv")


## obtaining the sample & set seed based on the last two digit number from your student ID
set.seed(41)
mysample <- sample(population$price, 50)
mysample

## Find the mean of your sample
mean(mysample)


## If I want to repeat the above process 10,000 times and calculate its mean and the form a sampling distribution
#create a vector(初始化向量)
dist_of_mean <- rep(0,10000) # 10000個0
# 抽樣50個->計算平均->抽樣10000次
for(i in 1:10000){
  dist_of_mean[i] <- mean(sample(population$price, 50))
}

### creating a histogram of the distribution of the sample mean
hist(dist_of_mean,
     main = "Sampling distribution of diamond", #標題
     xlab = "Price value of diamond",
     col = "cadetblue") #圖表顏色

### let's see the distribution of the population
hist(population$price,
     main = "Sampling distribution of diamond",
     xlab = "Price value of diamond",
     col = "cadetblue")

### It's obviously right-skewed distribution
### but still, on the basis of central limit theorem
### No matter WHAT THE POPULATION DISTRIBUTION BE
### If sample size is large enough (n>30)
### We can apply CLT and the sampling distribution will be normal distribution

### compare the population mean and the mean of the sampling distribution
print(mean(population$price))
print(mean(dist_of_mean))

### compare the population variance and the std of sampling distribution
print(var(population$price))
print(var(dist_of_mean))


### What the sampling distribution be if we increase the sample size to 100 ?
dist_of_mean_2 <- rep(0,10000) 

for(i in 1:10000){
  dist_of_mean_2[i] <- mean(sample(population$price, 100))
}


### Let's put the two graphs together 

par(mfrow=c(1,2)) #一列兩行

hist(dist_of_mean,
     main = "Sample size = 50",
     xlab = "Price value of diamond",
     col = "cadetblue")

hist(dist_of_mean_2,
     main = "Sample size = 100",
     xlab = "Price value of diamond",
     col = "cadetblue")



