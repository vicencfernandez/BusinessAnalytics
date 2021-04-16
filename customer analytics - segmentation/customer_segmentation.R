## ---- libraries---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(rfm)

## ----loading transaction data-------------------------------------------------
customers <- read.csv("customers.csv", sep=";", dec=",")
customers$Gender <- as.factor(customers$Gender)
customers$Education <- as.factor(customers$Education)
head(customers)
str(customers)
summary(customers)

orders <- read.csv("purchases.csv", sep=";", dec=",")
orders$Date <- as.Date(orders$Date, format =  "%d/%m/%y")
head(orders)
str(orders)
summary(orders)

## ======RFM by package=========================================================

### ----create rfm score -------------------------------------------------------
rfm_result <- rfm_table_order(select(orders, -1), IDcustomer, Date, Revenue, Sys.Date())
str(rfm_result)
head(rfm_result$rfm)
str(rfm_result$rfm)

## ----heat map of the situation ----------------------------------------------
rfm_heatmap(rfm_result)

## ----bar chart of the situation ----------------------------------------------
rfm_bar_chart(rfm_result)

## ----histohgram---------------------------------------------------------------
rfm_histograms(rfm_result)

## ----customer by order---------------------------------------------------------
rfm_order_dist(rfm_result)

## ----some scatter plots---------------------------------------------------------
rfm_rm_plot(rfm_result)
rfm_fm_plot(rfm_result)
rfm_rf_plot(rfm_result)


## ======RFM - segments=========================================================

segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Hibernating", "Lost")

recency_lower   <- c(4, 2, 3, 4, 3, 3, 2, 1, 1, 2, 1)
recency_upper   <- c(5, 4, 5, 5, 4, 4, 3, 2, 1, 3, 1)
frequency_lower <- c(4, 3, 1, 1, 1, 3, 1, 2, 4, 2, 1)
frequency_upper <- c(5, 4, 3, 1, 1, 4, 2, 5, 5, 3, 1)
monetary_lower  <- c(4, 4, 1, 1, 1, 3, 1, 2, 4, 2, 1)
monetary_upper  <- c(5, 5, 3, 1, 1, 4, 2, 5, 5, 3, 1)

segments <- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
                        frequency_lower, frequency_upper, monetary_lower, monetary_upper)
segments
segment10 <- filter (segments, customer_id==10)
print (segment10, width = Inf)

## ----tabulate segments---------------------------------------------------
theList <- segments %>%
  dplyr::count(segment) %>%
  dplyr::arrange(dplyr::desc(n)) %>%
  dplyr::rename(Segment = segment, Count = n)

theList

theNeedAttention <- filter (segments, segment=="Need Attention")
theNeedAttention
print (theNeedAttention, width = Inf)

theLost <- filter (segments, segment=="Lost")
theLost
print (theLost, width = Inf)

## ----average monetary value----------------------------------------------
rfm_plot_median_monetary(segments)

## ----average recency-----------------------------------------------------
rfm_plot_median_recency(segments)

## ----average frequency---------------------------------------------------
rfm_plot_median_frequency(segments)






## ======RFM by hand============================================================

### ----joining data------------------------------------------------------------
alldata <- merge(orders, customers, by = "IDcustomer")
head(alldata)

### ----money-------------------------------------------------------------------
moneyAG <- alldata %>% group_by(IDcustomer) %>% summarise (money = sum (Revenue))
ggplot(moneyAG, aes(money)) + geom_histogram(bins = 50)

### ----frequency---------------------------------------------------------------
freqAG <- alldata %>% group_by(IDcustomer) %>% summarise (frequency = n())
ggplot(freqAG, aes(frequency)) + geom_histogram(bins = 50)

### ----recency------------------------------------------------------------------
recAG <- alldata %>% group_by(IDcustomer) %>% summarise (recency = max(Date))
recAGdays <- data.frame (IDcustomer = recAG$IDcustomer, recency2 = as.numeric(as.Date("2021/01/01") - recAG$recency))

### ----combination of r,f,m----------------------------------------------------
tmp <- merge(moneyAG,freqAG,"IDcustomer")
tmp <- merge(tmp,recAG,"IDcustomer")
modelRFM <- merge(tmp,recAGdays,"IDcustomer")
modelRFM
head(modelRFM)
str(modelRFM)


### ----Money clustering---------------------------------------------------

tmp <- data.frame(modelRFM$money)
kmeansObj <- kmeans(tmp, centers = 8)
resylts01 <- data.frame (customer_id = segments$customer_id, rfm = segments$segment, kmeans_money = kmeansObj$cluster) %>% arrange (rfm)
resylts01
ggplot(resylts01, aes(x = kmeans_money, y = rfm)) + geom_point()

### ----Freq clustering---------------------------------------------------

tmp <- data.frame(modelRFM$frequency)
kmeansObj <- kmeans(tmp, centers = 8)
resylts02 <- data.frame (customer_id = segments$customer_id, rfm = segments$segment, kmeans_freq = kmeansObj$cluster) %>% arrange (rfm)
resylts02
ggplot(resylts02, aes(x = kmeans_freq, y = rfm)) + geom_point()

### ----Money+Freq clustering---------------------------------------------------

tmp <- data.frame(modelRFM$money, modelRFM$frequency)
kmeansObj <- kmeans(tmp, centers = 8)
resylts03 <- data.frame (customer_id = segments$customer_id, rfm = segments$segment, kmeans_money_freq = kmeansObj$cluster) %>% arrange (rfm)
resylts03
ggplot(resylts03, aes(x = kmeans_money_freq, y = rfm)) + geom_point()


### ----Money+Freq+Rec clustering---------------------------------------------------

tmp <- data.frame(modelRFM$money, modelRFM$frequency, modelRFM$recency2)
kmeansObj <- kmeans(tmp, centers = 8)
resylts04 <- data.frame (customer_id = segments$customer_id, rfm = segments$segment, kmeans_money_freq_rec = kmeansObj$cluster) %>% arrange (rfm)
resylts04
ggplot(resylts04, aes(x = kmeans_money_freq_rec, y = rfm)) + geom_point()

tmp2 <- resylts04 %>% arrange (customer_id)
ggplot(modelRFM, aes(x=money, y=frequency, color=as.factor(tmp2$rfm)))  +  geom_point(size=2, shape=23)
ggplot(modelRFM, aes(x=money, y=recency, color=as.factor(tmp2$rfm)))  +  geom_point(size=2, shape=23)
ggplot(modelRFM, aes(x=recency, y=frequency, color=as.factor(tmp2$rfm)))  +  geom_point(size=2, shape=23)





### ----Let's see the right number of clusters---------------------------------------------------
library(NbClust)
library(factoextra)
fviz_nbclust(tmp,kmeans,method = "silhouette")


## ======rfm score clustering==================================================
tmp <- data.frame(r = rfm_result$rfm$recency_score, f= rfm_result$rfm$frequency_score, m = rfm_result$rfm$monetary_score)
kmeansObj <- kmeans(tmp, centers = 6)

resylts05 <- data.frame (customer_id = segments$customer_id, rfm = segments$segment, kmeans = kmeansObj$cluster) %>% arrange (rfm)
resylts05
ggplot(resylts05, aes(x = kmeans, y = rfm)) + geom_point()
