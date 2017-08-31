setwd("C:\\Users\\Admin\\Desktop\\R project\\Project2")
R_train = read.csv("bank-full_train.csv", stringsAsFactors = F ) 
library(dplyr)
glimpse(R_train) #Row = 31647, col = 18

R_test = read.csv("bank-full_test.csv", stringsAsFactors = F ) 
glimpse(R_test) #Row 13,564, col = 17

#Joining the two data sets
R = bind_rows(R_train, R_test)
glimpse(R) #Looking at the combination of two sets

#Converting y feature to numeric
R$y <- ifelse(R$y=="yes", 1, 0)


hist(R$campaign) #creating a histogram of campaign
R$campaign = log(R$campaign) #Converting campaign feature to normal
summary(R$campaign) #Looking at summary of campaign feature 
hist(R$campaign) #Checking how campaign varaible distribution looking after making it normal

#Bucketing different job types as per count 
table(R$job)
R<- R %>%
  mutate(j_1= as.numeric(job %in% c("blue-collar")),
         j_2= as.numeric(job %in% c("management")),
         j_3= as.numeric(job %in% c("technician")),
         j_4= as.numeric(job %in% c("admin.")),
         j_5= as.numeric(job %in% c("services")),
         j_6= as.numeric(job %in% c("retired")),
         j_7= as.numeric(job %in% c("self-employed")),
         j_8= as.numeric(job %in% c("entrepreneur")),
         j_9= as.numeric(job %in% c("housemaid")))%>%  select(-job)

#Bucketing marital status as per count 
table(R$marital)
R<- R %>%
  mutate(m_1= as.numeric(marital %in% c("married")),
         m_2= as.numeric(marital %in% c("single"))) %>% select(-marital)

#Bucketing education status as per count 
table(R$education)
R<- R %>%
  mutate(e_1= as.numeric(education %in% c("secondary")),
         e_2= as.numeric(education %in% c("tertiary")))  %>% select(-education)

#Converting default into numeric type
table(R$default)
R<- R %>%
  mutate(d_1= as.numeric(default %in% c("no"))) %>% select(-default)

#Bucketing housing as per count 
table(R$housing)
R<- R %>%
  mutate(h_1= as.numeric(housing %in% c("no"))) %>% select(-housing)

#Converting loan into numeric type
table(R$loan)
R<- R %>%
  mutate(l_1= as.numeric(loan %in% c("no"))) %>% select(-loan)

#Converting contact type into numeric
table(R$contact)
R<- R %>%
  mutate(c_1= as.numeric(contact %in% c("cellular")))%>% select(-contact)

#Bucketing month 
unique(R$month)
R<- R %>%
  mutate(q_1= as.numeric(month %in% c("jan", "feb", "mar")),
         q_2= as.numeric(month %in% c("apr", "may", "jun")),
         q_3= as.numeric(month %in% c("jul", "aug", "sep")))%>%  select(-month)

#Bucketing poutcome as per count 
table(R$poutcome)
R<- R %>%
  mutate(p_1= as.numeric(poutcome%in% c("unknown")),
         p_2= as.numeric(poutcome %in% c("failure")),
         p_3= as.numeric(poutcome %in% c("success"))) %>% select(-poutcome)

#Dividing data set into train and test data
R_train <- R[1:31647,]
R_test <- R[31648:45211,]

library(car) #This isuse for checking up VIF 
R_fit <- lm(y ~ ., data=R_train) 
summary(R_fit)
R_vif <- vif(R_fit) #Calculating VIF
sort(R_vif, decreasing = T)[1:3] #Arranging as per VIF only looking at top 3 

#Fitting logistic gregression
model <- glm(y ~. ,family=binomial(link='logit'),data=R_train)
summary(model)

#Applying step function
model <- step(model)
formula(model)


#Fitting Logistic regression to see which feature is highly insignificant
model = glm(y~ balance + day + duration + campaign + pdays + ID + j_1 + 
              j_5 + j_7 + j_9 + m_1 + e_1 + e_2 + h_1 + l_1 + c_1 + q_1 + 
              q_3 + p_2 + p_3, family=binomial(link="logit"), data=R_train)
summary(model)

#Drop J_7
model = glm(y~ balance + day + duration + campaign + pdays + ID + j_1 + 
              j_5 + j_9 + m_1 + e_1 + e_2 + h_1 + l_1 + c_1 + q_1 + 
              q_3 + p_2 + p_3, family=binomial(link="logit"), data=R_train)
summary(model)

#Creating column score
R_train$score <- predict(model, newdata = R_train, type = "response")

P=TP+FN # Total number of 1's in reality
N=TN+FP # Total number of 0's in relity
#Total number of points
Total <- P + N 
#We can do this for multiple cutoff values as for only one cutoff = 0.2 is of no use
#Creating table with different cutoff values
cutoff_data = NULL
cutoffs = round(seq(0,1,length=100),3)
cutoffs # A vector of 100 cutoff values

for (cutoff in cutoffs)
{
  predicted=as.numeric(R_train$score>cutoff)
  
  TP=sum(predicted==1 & R_train$y==1)
  FP=sum(predicted==1 & R_train$y==0)
  FN=sum(predicted==0 & R_train$y==1)
  TN=sum(predicted==0 & R_train$y==0)
  cutoff_data=rbind.data.frame(cutoff_data,c(cutoff,TP,FP,FN,TN))
}
colnames(cutoff_data) = c("Cutoff", "TP", "FP", "FN", "TN")


#Now we can make the metrics to find the best cut off
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, 
         Sp=TN/N,
         P= FN+TP,
         N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  select(-P,-N)


#It would be easier for us to determine best cut off if we can see the data in 
#visualization but before that
#we must convert matrix into "long format" from "wide format"

library(tidyr)#For tiding the data together

cutoff_viz=cutoff_data %>%
  select(Cutoff,KS) %>%
  gather(Criterion,Value,KS)

View(cutoff_viz) # Data is in long format


str(R_train) #Training data set
str(R_test) #Test data set
R_test$score= predict(model,newdata = R_test,type = "response") #Predicting score on test data


# Cutoff with max KS
KS_cutoff=cutoff_data$Cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff

#Getting y feature as 0 and 1
R_test$y<- as.numeric(R_test$score<KS_cutoff) 
glimpse(R_test$y)

#Converting 0 and 2 into yes and no in y feature
R_test$y <- ifelse(R_test$y==1, "yes", "no")
glimpse(R_test$y)

#Submitting the file 
submit = data.frame(y = R_test$y)
#Giving name to the file
write.csv(submit, file = "Atul_Kumar_P5_part2.csv", row.names = FALSE)


