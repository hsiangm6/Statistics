### Load the data (diamond.csv) on your own and answering the following question
### Snapshot your code as well as your result to WORD, and hand over your file to CU as PDF file.

library(dplyr)
### Q1 50% ### 
### Recall from the last semester's midterm 1 (If you still remember it, I guess not. )
### Please classifying the diamond depends on its cut and clarity
### and calculate the average price of each group
### Finally, only list the cut, clarity, the amounts diamonds of each group and average price with descending order
diamond <- read.csv("DiamondsPrices.csv")
q1 <- diamond %>% group_by(cut, clarity) %>% 
  summarise(avgprice = mean(price), 顆=n()) %>%
  select(cut, clarity, 顆, avgprice) %>% 
  arrange(desc(avgprice))
q1

### Q2 50% ###
### Subset the data with the following condition:
### cut is good and clarity is VS1,
### and take it as the population
### then we are interest in its price
population <- read.csv("DiamondsPrices.csv")
q2 <-  population %>% filter(cut == "Good", clarity== "VS1")
q2
### Plot the population distribution (5%)

### Just like we did in the lecture
### We sampled from the population 10,000 times
### Please plot the sampling distribution with sample size n=50, n=250, n=500, respectively. (45%)
### Note that you need to put the three graphs together as we did it in the lecture
### and name the title to recognize what its sample size is.
s50 <- rep(0,10000) # 10000個0
s250 <- rep(0,10000)
s500 <- rep(0,10000)
# 抽樣50個->計算平均->抽樣10000次
for(i in 1:10000){
  s50[i] <- mean(sample(q2$price, 50))
  s250[i] <- mean(sample(q2$price, 250))
  s500[i] <- mean(sample(q2$price, 500))
}
par(mfrow=c(1,3)) #一列三行
hist(s50,
     main = "Sample size = 50",
     xlab = "Price value of diamond",
     col = "cadetblue")

hist(s250,
     main = "Sample size = 250",
     xlab = "Price value of diamond",
     col = "cadetblue")

hist(s500,
     main = "Sample size = 500",
     xlab = "Price value of diamond",
     col = "cadetblue")
