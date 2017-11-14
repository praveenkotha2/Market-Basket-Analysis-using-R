setwd("C:/Users/PraveenKotha/Desktop/marketba")
# install.packages('readxl')
# install.packages('lubridate')
# install.packages('tidyverse')
# install.packages('knitr')
# install.packages('arules')
# install.packages('arulesViz')


library(arulesViz)
library(arules)
library(knitr)
library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)

retail<-read_excel('retail.xlsx')
retail2<-retail
table(is.na(retail))

retail<-retail[complete.cases(retail),]
table(is.na(retail))

str(retail)
summary(retail)


retail$Description<-as.factor(retail$Description)
retail$Date<-as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Country<-as.factor(retail$Country)

summary(retail)

retail$Time<-as.factor(retail$Time)
a<-hms(retail$Time)
retail$Time<-hour(a)

retail$month<-format(retail$Date,"%m")
table(retail$month)

ggplot(retail,aes(x=month))+
  geom_bar(fill="indianred")+
  ggtitle("Transactions across the year")+
  xlab("Month")+
  ylab("No. of transactions")
  

ggplot(retail,aes(x=Time))+
  geom_bar(fill="indianred")+
  ggtitle("Transcations across the day")+
  xlab("Time")+
  ylab("No. of transactions")
  

items<-retail %>%
  group_by(InvoiceNo) %>%
  summarise(total=n())

ggplot(items,aes(x=total))+
  geom_histogram(fill="indianred", binwidth = 1)+
  geom_rug()+
  coord_cartesian(xlim=c(0,80))+
  ggtitle("No. of transactions with different basket sizes")+
  xlab("Basket size")+
  ylab("No of transactions ")

top_items<-retail %>%
  group_by(Description) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

summary(retail)
top_items<-head(top_items,10)

ggplot(top_items,aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()+
  scale_y_continuous(limits = c(0,3000))+
  ggtitle("Frequency plot of top 10 Items")+
  xlab("Description of item")+
  ylab("Count")


retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',',skip=1)
tr
summary(tr)

itemFrequencyPlot(tr, topN=20, type='absolute')

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, target='rules'))
df_basket <- as(rules,"data.frame")
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")

plot(topRules, method = "grouped")
