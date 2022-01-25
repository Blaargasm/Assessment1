rm(list=ls())

library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(caret)
library(pROC)
set.seed(1029)

ub = read.csv('test_dataset/UB.csv')
head(ub)

#1: Find the Average, Median, Q1, Q3, Min, Max and the Mode of Age variable

summary(ub$Age)

#2: Provide a Histogram of the Tenure variable. What can you say about the distribution?

hist(ub$Tenure, xlab = 'Tenure (Months)')
#bimodal shape, tenure ranges from 0-38 months. Tenures of 15-19 months are less common. 
#Vast majority of customers have either 10-14 months of tenure, or 20-25 months.

#3: Provide a 95% Confidence Interval of the Tenure. 
#Can we claim that the average Tenure of the Userbase is 17 months

#Since sample size is very large, apply Central Limit Theorem. Therefore, I am assuming sample mean is ~normally distributed.
#calculate mean, standard deviation, standard error of mean
t.mean = mean(ub$Tenure)
t.size = length(ub$Tenure)
t.sd = sd(ub$Tenure)
t.se = t.sd/sqrt(t.size)
print(t.se)
#calculate t-score, use alpha of 0.05 since we want 95% confidence
alpha = 0.05
degrees.freedom = t.size - 1
t.score = qt(p = alpha/2, df=degrees.freedom, lower.tail=F)
print(t.score)
#calculate margin of error, use it to calculate upper and lower bounds
margin.error = t.score * t.se
t.lower = t.mean - margin.error
t.upper = t.mean + margin.error
print(c(t.lower, t.upper))



#simple one sample t-test to confirm results
t.test(ub$Tenure)

#No, average tenure is ~16 months with 95% confidence.

#4: Calculate the Average Age and the Proportion of Customers per Gender and Type.
#output columns: Gender, Type, Average Age, Proportion of customers

#select columns we want, group by gender and age,create new columns and calculate average age, proportion of customers
head(ub)
cbygta = ub %>% select(Gender, Type, Age) %>% 
  group_by(Gender, Type) %>% 
  summarize('Average Age' = mean(Age), 'Proportion of Customers'= (n()/nrow(ub)))
head(cbygta)

#check with tapply()
tapply(ub$Age, list(ub$Gender, ub$Type), mean)

#5:Calculate  the Number of  Customers per Email  Domain. 
#Output: Email Domain, Number of Customers

#select column we want, use regex to extract domain name, then calculate number of customers
cbye = ub %>% select(Email_Address) %>% 
  group_by('Email Domain' = str_extract(Email_Address,'(?<=@)(\\w+)')) %>%
  select('Email Domain') %>%
  summarize('Number of Customers' = n())
#check that number of customers is what we expect (500k)
head(cbye)
sum((cbye$`Number of Customers`))

#6: Calculate the Average Number of Emails Sent per Weekday
#output: Weekdays, Average Number of Emails Sent

#read in data, isolate Sent_Date column in new dataframe, convert dates to weekdays
sent = read.csv('test_dataset/Sent_Table.csv', stringsAsFactors = FALSE)
head(sent)
s.wkdays = data.frame(Sent_Date = as.Date(sent$Sent_Date))
s.wkdays$Weekdays = weekdays(s.wkdays$Sent_Date)
head(s.wkdays)

#calculate occurrence of each weekday in data 
nperwkday = s.wkdays %>% group_by(Sent_Date) %>% 
  summarize(nperday = n()) %>% 
  mutate(Weekdays = weekdays((Sent_Date)))
cperwkday = nperwkday %>% count(Weekdays)
head(cperwkday)

#calculate total number of emails sent by day
sentbyday = s.wkdays %>% select(Weekdays) %>% 
  group_by(Weekdays) %>% summarize('npbyday' = n())
head(sentbyday)

#join weekday occurrence with dataframe containing total emails by day,
#calculate average number of emails sent by day
sentbyday = sentbyday %>% 
  left_join(cperwkday, by=c('Weekdays')) %>% 
  mutate('Average Number of Emails Sent' = npbyday/n) %>% 
  select(Weekdays, 'Average Number of Emails Sent')
head(sentbyday)

#7: Calculate the Average Open Rate by SubjectLine_ID
#What can you say about the performance of the messages?
# infer if these three SubjectLined IDs statistically equal
resp = read.csv('test_dataset/Responded_Table.csv')
head(resp)

#calculate valid responses by subjectline ID
rns = inner_join(resp, sent, by=c('Customer_ID', 'SubjectLine_ID')) 
vbyid = rns %>% filter(Responded_Date == Sent_Date) %>% 
  group_by(SubjectLine_ID) %>% 
  summarize(totvalidresponses = n())
head(vbyid)

#calculate total emails sent by subject line ID
subjsent = sent %>% count(SubjectLine_ID) 
print(subjsent)

#join valid responses with total emails sent, calculate average open rate by subject line ID
openrbyid = inner_join(vbyid, subjsent, by=c('SubjectLine_ID')) %>% 
  mutate(AvgOpenRate = totvalidresponses/n)
head(openrbyid)
#SubjectLine_ID 3 seems to be underperforming, with an 8.4% open rate.

#model AvgOpenRate as a function of the subjectline_ID
#use one-way ANOVA
one.way = aov(openrbyid$AvgOpenRate ~ openrbyid$SubjectLine_ID, data = openrbyid)
summary(one.way)
#P-value of 0.3 is greater than 0.05 means that we can't reject the null hypothesis (no difference between subjectline ID's)
#there are no significant differences between the SubjectLine_IDs.


#8:Calculate the Average Open Rate by Type
#join data 
sub = sent %>% left_join(ub, by=c('Customer_ID'))
atype = sub %>% 
  inner_join(resp, by=c('Customer_ID', 'SubjectLine_ID','Sent_Date' = 'Responded_Date'))
#calculate number of valid emails by type
cbytype = atype %>% count(Type)

#calculate number of emails sent by type
sentbytype = sub %>% group_by(Type) %>% summarize(sent.type = n())

avgbytype = cbytype %>% left_join(sentbytype, by=c('Type')) %>% mutate(type.AvgOpen = n/sent.type)
#We are seeing ~10% average open rate for businesses, and ~9% average open rate for consumers.

#9 predict open rate based on customer attributes, subjectlineID received
#describe/evaluate model
#pr(opening) of cust(gender=F,type=B,email=aol,age=50,tenure=12), subjectlineID=3

#create indicator variable opened
ind = rns %>% filter(Responded_Date == Sent_Date) %>% mutate(opened = 1)
#
oprate = sent %>% left_join(ind, by=c('Sent_Date','Customer_ID','SubjectLine_ID')) %>% left_join(ub, by=c('Customer_ID'))
#drop duplicate rows, unnecessary columns
oprate = oprate[!duplicated(oprate[,1:3]),] 
oprate = oprate %>% select(-c(Sent_Date,Responded_Date,Customer_ID)) %>% replace(is.na(.),0)
oprate$Email_Address = str_extract(oprate$Email_Address,'(?<=@)(\\w+)')
cols = c('SubjectLine_ID', 'Gender','Type','Email_Address')
oprate[cols] = lapply(oprate[cols], factor)
head(oprate)

#divide data into 70% training and 30% test/validation data
m = nrow(oprate)
trn = sample(1:m, size=round(m*0.7), replace=FALSE)
train = oprate[trn,]
valid = oprate[-trn,]
head(valid)
#since we are predicting the open rate based on the customer attributes and subject line ID received, 
#I use logistic regression generates values between 0 and 1 that can be interpreted as a percentage.

o.glm  = glm(opened ~ SubjectLine_ID + Gender + Type + Email_Address + Age + Tenure, data =train, family = 'binomial')
#create new dataframe with parameters given
newdata = data.frame(Gender='F',Type='B',Email_Address='aol', Age = 50, Tenure = 12, SubjectLine_ID = '3')
head(newdata)
#use model created above to predict open rate of customer = 8.4%
predict(o.glm, newdata, type='response')


#check accuracy of model
valid$results = predict(o.glm, newdata=valid, type='response')
summary(valid$results)
#choose threshhold of 10%
fitround = ifelse(valid$results > 0.1, 1,0)
#calculate accuracy of 72%  with confusion matrix, pretty good. 
#We could probably make this model much more accurate with something like k-fold cross validation and stepwise regression but for qtime and simplicity's sake I'll stick with the model we have.
t = table(fitround, valid$opened)
acc <- (t[1,1] + t[2,2]) / sum(t)
acc
t

r<-roc(valid$opened,fitround)
plot(r, main='ROC Curve')
r
#this ROC curve and AUC calculation shows that our model is not very good at predicting the open rate.
#since the AUC value is only ~52%, model is only slightly better than guessing.

#10: graph Open Rate by Age, Type and Gender
#create logistic regression model on parameteres specified
openr = glm(opened ~ Age + Type, family=binomial, data=oprate)

#create new dataframe with unique values of Type, quantiles of Age, and Gender
newdat = with(oprate, expand.grid(Type=unique(Type),
                                   Age=quantile(Age),
                                   Gender=Gender))
#use model created above to predict results 
newdat$prob = predict(openr, newdat, type='response')
#plot relationship between open rate, age, type with separate plots for each gender
ggplot(newdat, aes(Age, prob, color=factor(Type))) +
  geom_line() +
  facet_grid(.~Gender)
