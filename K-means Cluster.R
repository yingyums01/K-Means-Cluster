---
  title: "K-means Cluster"
author: "Doris Ying-Yu Kuo"
date: "11/3/2019"
output:
  html_document:
  toc: true
toc_float: true
theme: darkly
---
  

knitr::opts_chunk$set(echo = TRUE)


# ??? PART I: Collecting the Data

library(stats)
library(mice)
library(tidyverse)
library(factoextra)

acme_raw <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/mallcustomers.csv")
acme_raw
str(acme_raw)

# ??? PART II: Explore and Prepare the Data

## 1. Dealing Gender's Missing Value with mice

#Missing value
acme_raw %>%
  select_if(function(x) any(is.na(x)))

#Gender Missing - predictive

acme_raw$Gender <- as.factor(acme_raw$Gender)
summary(acme_raw$Gender)

acme_impute <- mice(acme_raw,m=1,maxit=5,meth='logreg',seed=1234)
acme_impute

#complet replacing the NA
acme_impute$imp$Gender[1:10,]
acme <- mice::complete(acme_impute)
summary(acme$Gender)


head(acme)



## 2. Convert Values in Income

acme <- acme%>%
  mutate(Income=str_replace_all(Income,(",| USD"),""))%>%
  mutate(Income=as.numeric(Income))
str(acme)
head(acme)



# ??? PART III: Segment Customers

## 1. K-means Cluster - 3 Cluster

#select the column and scale it to z score
acme_k <- acme %>%
  select(Income,SpendingScore)
summary(acme_k)
acme_z <- scale(acme_k)
summary(acme_z)
#train the model
set.seed(1234)
acme_k3 <- kmeans(acme_z, centers=3, nstart = 25)
acme_k3$size
acme_k3$center
acme_k3


## 2. Visualization - 3 Cluster

#Graph
fviz_cluster(acme_k3, data = acme_z)
acme_k3$cluster
#complete the cluster in the original data
acme$cluster <- acme_k3$cluster
head(acme)
summary(acme$Gender)


## 3. Recommended Cluster Number

# ... then specify the loop that generates the values.
wcss <- vector()

library(gridExtra)
n = 20
set.seed(1234)
for(k in 1:n) {
  wcss[k] <- sum(kmeans(acme_z, k)$withinss)
}

wcss

# Visualize the values of WCSS as they relate to number of clusters
tibble(value = wcss) %>%
  ggplot(mapping=aes(x=seq(1,length(wcss)), y=value)) +
  geom_point()+
  geom_line() +
  labs(title = "The Elbow Method", y = "WCSS", x = "Number of Clusters (k)" ) +
  theme_minimal()




From the elbow graph, we think both k=5 and k=9 are possible candidate for the final number of clusters. To know which one will work better for the subsequent labeling, we dig into cluster sizes and fviz_cluster graph.


set.seed(1234)
acme_k5 <- kmeans(acme_z, centers = 5, nstart = 25)
acme_k9 <- kmeans(acme_z, centers = 9, nstart = 25)

acme_k5$size
acme_k9$size

# Plot and compare the results.
p_k3 <- fviz_cluster(acme_k3, geom = "point", data = acme_z) + ggtitle("k = 3")
p_k5 <- fviz_cluster(acme_k5, geom = "point",  data = acme_z) + ggtitle("k = 5")
p_k9 <- fviz_cluster(acme_k9, geom = "point",  data = acme_z) + ggtitle("k = 9")

# Here, we make use of the grid.arrange() function to display several plots at the same time.
# This function is part of the griExtra package. Install it, if you haven't already.
library(gridExtra)
grid.arrange(p_k3, p_k5, p_k9, nrow = 2)


#We have two directions to think what number of k should we choose. First is to check whether the sizes of clusters are balanced. Second, if the sizes of clusters are not balaced, is that necessary and meaningful to separate the group with bigger size into smaller group?
  
#  From the size statistic and multiple graph, when k =5, the size is not well balanced becaue the cluster 2 contains 81 observations. However, except the cluster 2, the other clusters' sizes are not too different from each other.

#We then dig into the cluster2(in k =5) group and notice that the group belong to middle income with middle SpendingScore. We think, for these ordinary customers, it is okay not to separate them into smaller group because they are not the customers we want to target. We only want to target customers with some significant traits ex: high/low income or high/low SpendingScore.

#Also, when we further look at the k = 9, the size is still unbalanced. We also think it is not necessary to split the original 5 groups into that detailed.

#In conclusion, we think the appropriate number of cluster here is k =5.

## 4. New Visualiztion - Recommended Cluster Number


p_k5


# ??? PART IV: Interpret the Results
## 1. Assigned Label

Group 1(red): Low Income, High Consumption. ( Freshman in the workplace, fresh graduated students )

Group 2(yellow): Normal Income, Normal Income. ( Normal people )

Group 3(green): High Income, Low Consumption.  ( Have family and children , Middle or High-level class )

Group 4(blue): High Income, High Consumption. ( Gold Collar,  New Money)

Group 5(purple): Low Income, Low Consumption. ( The Poor, Single Mum)


## 2. Age and Gender Distribution

# How  does  the  average  age  and  gender  distribution  for  each  cluster compare   to   that   of   the   overall   data   set?

#Assign the result to the original dataframe first
acme$cluster <- acme_k5$cluster
head(acme)
unique(acme$cluster)

#Original Proportion - Age
acme %>%
  summarize(Mean_Age = mean(Age))

#Cluster Proportion - Age
acme %>%
  group_by(cluster)%>%
  summarize(Mean_Age = mean(Age))

#Original Proportion - Gender
#Change the Gender into dummy variable first
acme <- acme %>%
  mutate(Male = ifelse(Gender == 'Male',1,0)) %>%
  mutate(Female = ifelse(Gender == 'Female',1,0))

#Original Proportion - Female
acme %>%
  summarize(Mean_Female = mean(Female))
#Cluster Proportion - Female
acme %>%
  group_by(cluster) %>%
  summarize(Mean_Female = mean(Female))

#Original Proportion - Male
acme %>%
  summarize(Mean_Male = mean(Male))
#Cluster Proportion - Male
acme %>%
  group_by(cluster) %>%
  summarize(Mean_Male = mean(Male))


## 3. Recommendation

#Combine the results of the cluster analysis and the age & gender distribution in these five clusters. We came out with the following recommendation for promotion and coupon:

--------------------------------------------------------------------------------------

#**Group 1 | Discount or coupons on voluminous formal dressing and accessories / Outlet Store Promotion**

#**Low Income, High Consumption / Mean Age: 25.3 / Male%: 22.7%**

#we assume people in this cluster are workers who just graduated and have a great demand to purchase stuff, to dress themselves up for work or to socialize with their co-workers. Since their salaries are not high, they might be sensitive to discount or voucher.  

#For the first marketing strategy, we recommend to provide discount and voucher for these people if they buy more stuff at once or buy a product buddle, especially in the product fields like boutique , formal dressing, perfume, high heels shoes, and accessories. Therefore, the strategy can not only match the group 1 customers' high demands but also increase our store's sales and revenue to cover the voucher and discount cost. Second, we can also promote the outlet, where the products price are lower, advertisement to people in this group. 



#**Group 2 | Coupons on daily grocery & bourgeoisie entertainment**

#**Normal Income, Normal Consumption / Mean Age: 42.7/ Male%:  43.2% **

#We assume people in the second cluster are typical middle class, which have enough income to sustain their spending evenly. Their major demand is to balance the expenses in daily life and probably pursuing the quality of life. Concerning the gender, the demand of this group should majorly concentrating on the house quality and living quality at ordinary prices. 

#Therefore, for the market strategy, we recommend to provide weekly grocery package with some boutique winery invitations or bourgeoisie entertainment. This strategy not only satisfies the demand of the group, especially regarding their family properties, but also encourages this group to consume the probable interested activities and products at the middle class level. 



#--------------------------------------------------------------------------------------

#**Group 3 | Delicate Luxury & High End Entertainment Invitation**

#**High Income, Low Consumption / Mean Age: 41.1 / Male%: 62.9% **

#We assume that people in this cluster have family and children, normally they are middle or high level class in society. Most of them are men, usual luxury goods can no longer satisfy them, usually they will buy high value persered goods, such as cars and delicate male accessories. 

#Hence, to target this cluster, we offer them coupons for watches and male accessories of Outlets. Meanwhile, the high-end wine tasting, golf course will be provided as the supplement as well. 


#--------------------------------------------------------------------------------------

#**Group 4 | Coupons for high quality products| encourage them to be memberships**

#**High Income, High Consumption / Mean Age: 32.7 / Male%: 48.7%**

#We assume that people in these class are golden collar, usually these young people are the middle class in the company. Although they earn a lot, they got higher pressure. So they always need consume to relieve their pressure.

#So, we give them high quality products, such as AVEDA haircare service and Godiva Ice cream, and encourage them to have memberships. Jewelries and high-end  




#--------------------------------------------------------------------------------------

#**Group 5 | Inferior Food Coupon & Goods Stamp Rewards**

#**Low Income, Low Consumption / Mean Age: 45.2 / Male%:  34.8%**

#We assume that people in this cluster are the poor people or the single mom. Maybe they don't have a stable or high-paid work. Thus, when they went shopping, they will make a list of all the daily necessities, and they rarely buy luxurious goods.

#So, we offer them coupons for low price products, like food, clothes. Furthermore, for this group, we provide them with goods stamp rewards to inspire them to purchase more inferior goods (increase volumes) by Acme Holding cards. This strategy will increase the contribution of this specific group in general.
