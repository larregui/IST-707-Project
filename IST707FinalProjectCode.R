####################################
#
# Author: Laura C. Larregui & TEAM
# Purpose: IST 707 Final Project
#
####################################

#Import Libraries
library(plyr)
library(dplyr)
library(grid)
library(tm)
library(arules) #for ARM
library(arulesViz) #for ARM
library(RColorBrewer)#contains color palettes

# First lets load the data set
### link to dataset: https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility
#APA Citation: Naresh (2020). Health care: Data set on Heart Attack Possibility (Version 1) [CSV file]. Retrieved from https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility. 
fname<- "C:\\Users\\laura\\OneDrive\\Documents\\Syracuse iSchool\\IST 707\\IST-707-Project\\heart.csv"
heart<- read.csv(fname, header = TRUE)
#View(heart) #Takes a look of the csv file
str(heart) # 303 records and 14 variables

#Check for missing values
(sum(is.na(heart))) #There isn't any missing values
heart<-na.omit(heart) # Just to make sure there aren't any missing values
str(heart)# 303 records and 14 variables

#Check for duplicates
duplicated(heart)# One duplicated record
heart<-heart[!duplicated(heart),]# Removed duplicated record
str(heart) #302 records and 14 variables
# Note: there was one duplicated record.

#Convert variables from numeric to nominal 
## sex, cp, fbs, restecg, exang, slope, ca, thal, target
heart$sex<- factor(heart$sex)
heart$cp<- factor(heart$cp)
heart$fbs<- factor(heart$fbs)
heart$restecg<- factor(heart$restecg)
heart$exang<- factor(heart$exang)
heart$slope<- factor(heart$slope)
heart$ca<- factor(heart$ca)
heart$thal<- factor(heart$thal)
heart$target<-factor(heart$target)

str(heart)#test

#Recoding Variables
heart$restecg=dplyr::recode(heart$restecg, "0"="Normal", "1"="ST-T wave abnormality", "2"="left ventricular hypertropy")
heart$slope=dplyr::recode(heart$slope, "0"="upslopping", "1"="flat", "2"="down-sloping")
heart$exang=dplyr::recode(heart$exang, "0"="No", "1"="Yes")
heart$sex=dplyr::recode(heart$sex, "0"="Female", "1"="Male")
#For thal, the dataset has 0,1,2,3 values. However, the dataset metadat does not identify what the value 0 means, so it was removed from the dataset.
heart<-heart[!(heart$thal=="0"),]
heart$thal=dplyr::recode(heart$thal, "1"="Normal", "2"="Fixed Defect", "3"="Reversable Defect")
heart$cp=dplyr::recode(heart$cp, "0"="Typical Angina", "1"="Atypical Angina", "2"="Non-anginal pain","3"="Asymptomatic")
heart$target=dplyr::recode(heart$target, "0"="high risk", "1"="low risk")
heart$fbs=dplyr::recode(heart$fbs, "0"="less than 120 mg/dl", "1"="more than 120 mg/dl")
heart$ca=dplyr::recode(heart$ca, "0"="0 major vessels", "1"="1 major vessel", "2"="2 major vessels","3"="3 major vessels")
heart$ca=dplyr::recode(heart$ca, "4"="4 major vessels")


#head(heart$target)
#Discretize Age
## -- Testing first
heart$ï..age <- cut(heart$ï..age, breaks = c(0,20,30,40,50,60,70,80,90),
                labels=c("teens","twenties","thirties","forties","fifties","sixties", "seventies", "eighties"))

# discreticize trestbps 
heart$trestbps <- cut(heart$trestbps, breaks = c(0,120,140,160,180,200),
                    labels=c("optimal","prehypertension","high blood pressure stage 1","high blood pressure stage 2","hypertension crisis"))
# discreticize chol 
heart$chol <- cut(heart$chol, breaks = c(0,200,240,600),
                      labels=c("Healthy Chol","Boderline High Risk","High Risk"))


#I would like to change the name of heart$ï..age to heart$age
names(heart)[names(heart) == "ï..age"] <- "age"
par(mfrow = c(2,1))
aa<- table(heart$age)
barplot(aa, col="red", main = "Age Frequency")
bb<- table(heart$trestbps)
barplot(bb, col="red", main = "TRESTBPS Frequency")
# Observing the Distributions
par(mfrow = c(1,2))#only used this if you want all graphs in the same window

a<- table(heart$age,heart$target)
a
barplot(a, main = "Heart Attack Risk across All Ages", beside = TRUE, col= brewer.pal(7, "Spectral"),legend.text = rownames((a)),args.legend=list(x="topright",bty="s"))

b<- table(heart$sex,heart$target)
b
barplot(b, main = "Heart Attack Risk VS Gender", beside = TRUE, col= c("purple", "lightblue2"),legend.text = rownames((b)),args.legend=list(x="topright",bty="s"))
#
par(mfrow = c(2,2))
c<- table(heart$exang,heart$target)
c
barplot(c, main = "Heart Attack Risk VS Exang", beside = TRUE, col= c("purple", "lightblue2"),legend.text = rownames((c)),args.legend=list(x="topright",bty="s"))

d<- table(heart$slope,heart$target)
d
barplot(d, main = "Heart Attack Risk VS Slope", beside = TRUE, col= c("purple", "cadetblue","lightblue2"),legend.text = rownames((d)),args.legend=list(x="topright",bty="s"))

e<- table(heart$ca,heart$target)
e
barplot(e, main = "Heart Attack Risk VS CA", beside = TRUE, col= brewer.pal(5, "Spectral"),legend.text = rownames((e)),args.legend=list(x="topright",bty="s"))

cp<- table(heart$cp,heart$target)
cp
barplot(cp, main = "Heart Attack Risk VS CP", beside = TRUE, col= brewer.pal(4, "Spectral"),legend.text = rownames((cp)),args.legend=list(x="topright",bty="s"))

#
par(mfrow = c(1,1))
f<- table(heart$target)
f
piepercent<- round(100*f/sum(f), 1)
pie(f, labels = piepercent, main = "Heart Disease Risk",col = c("white","darkred"))
legend("topright", c("low risk","high risk"), cex = 0.8,
       fill = c("white","darkred"))
g<-tapply(heart$sex, list(heart$target, heart$sex, heart$age), length)

################################
# Courtney's code
library(ggvis)
library(ggplot2)
library(ggpubr)
##Histograms
dev.off()
lapply(heart1[1:14], FUN=hist)
list <-lapply(1:ncol(heart1),
              function(col) ggplot2::qplot(heart[[col]],
                                           geom = "histogram",
                                           binwidth = 1))
cowplot::plot_grid(plotlist = list)


##Age plots
a=heart1%>%ggplot(aes(x=age))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+theme(axis.text = element_text(size =25 ),
                                               axis.title = element_text(size =26 ))
a

b=heart1%>%ggplot(aes(x=age))+geom_boxplot(fill='#A4A4A4', color="black",outlier.size = 5)+
  theme(axis.text = element_text(size =25 ),axis.title = element_text(size =26 ))  
b

#BP plots
c=heart1%>%ggplot(aes(x=trestbps))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+theme(axis.text = element_text(size =25 ),
                                               axis.title = element_text(size =26 ))
c

d=heart1%>%ggplot(aes(x=trestbps))+geom_boxplot(fill='#A4A4A4', color="black",outlier.size = 5)+
  theme(axis.text = element_text(size =25 ),axis.title = element_text(size =26 )) 
d

#Cholesterol plots
e=heart1%>%ggplot(aes(x=chol))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+theme(axis.text = element_text(size =25 ),
                                               axis.title = element_text(size =26 ))
e

f=heart1%>%ggplot(aes(x=chol))+geom_boxplot(fill='#A4A4A4', color="black",outlier.size = 5)+
  theme(axis.text = element_text(size =25 ),axis.title = element_text(size =26 )) 
f

#Plots for exercise-induced ST depression
g=heart1%>%ggplot(aes(x=oldpeak))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+theme(axis.text = element_text(size =25 ),
                                               axis.title = element_text(size =26 )) 
g

h=heart1%>%ggplot(aes(x=oldpeak))+geom_boxplot(fill='#A4A4A4', color="black",outlier.size = 5)+
  theme(axis.text = element_text(size =25 ),axis.title = element_text(size =26 )) 
h

i=heart1%>%ggplot(aes(x=thalach))+geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+theme(axis.text = element_text(size =25 ),
                                               axis.title = element_text(size =26 )) 
i

j=heart1%>%ggplot(aes(x=thalach))+geom_boxplot(fill='#A4A4A4', color="black",outlier.size = 5)+
  theme(axis.text = element_text(size =25 ),axis.title = element_text(size =26 )) 
j

#All plots
ggarrange(g,h,i,j,nrow = 2,ncol = 2)


################################
#Correlation
library(corrplot)
# Importing the dataset again because we need the variables to be numeric
heart1<- read.csv(fname, header = TRUE)
heart_cor <- cor(heart1) 
round(heart_cor, 2) 
corrplot(heart_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients.

#Correlation matrix with numbers 
par(mfrow = c(1,1))
corrplot(heart_cor, method = 'number', type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)

################################
#Tree Maps
library(treemap)
str(heart)
treemap(heart, index = c("target"), vSize = "chol")
treemap(heart, index = c("target"), vSize = "oldpeak")
treemap(heart, index = c("target"), vSize = "thalach")

################################
#Association Rule Mining
head(heart)#test
newheart<- heart[,-10]
newheart<- newheart[,-8]
head(newheart)#test
suppVar <- 0.01
confVar <- 0.9
maxlenVar <- 3
rulesHeartRight <- apriori(newheart, parameter = list(supp = suppVar, conf = confVar, maxlen = maxlenVar), 
                         appearance = list (default = "lhs", rhs= "target=high risk" ),control=list(verbose=F))
options(digits=2)
inspect(rulesHeartRight)
plot(rulesHeartRight, method="graph", interactive=TRUE)

# Sort by LIFT
rulesRightByLift <- head(sort(rulesHeartRight, by="lift"), 100)  
inspect(rulesRightByLift)
# Sort by Support
rulesRightBySupp <- head(sort(rulesHeartRight, by="supp"), 20)  
inspect(rulesRightBySupp)
# Sort by Confidence
rulesRightByConf <- head(sort(rulesHeartRight, by="conf"), 20)  
inspect(rulesRightByConf)

################################
#Recursive Partitioning and Regression Trees 
library(rpart,warn.conflicts = FALSE) 
library(rpart.plot,warn.conflicts = FALSE) 
library(rattle, warn.conflicts = FALSE) 
## Rattle: A free graphical interface for data science with R. 
## Version 5.2.0 Copyright (c) 2006-2018 Togaware Pty Ltd. 
## Type 'rattle()' to shake, rattle, and roll your data. 
#Recursive Partitioning and Regression Trees  
nrows<-nrow(heart) 
cutPoint<- floor(nrows/3*2) 
cutPoint 
## [1] 201 
rand<-sample(1:nrows) 
#training set 
heart_train <- heart[rand[1:cutPoint],] 
#test set 
heart_test <- heart[rand[(cutPoint+1:nrows)],] 
heart_test<-na.omit(heart_test) 
w.rpart <- rpart(target ~. , data = heart_train)
w.rpart
rpart.plot(w.rpart, digits = 3)
fancyRpartPlot (w.rpart)

###########################################
# Confusion Matrix for the desicion tree model
library(caret)
pred<-predict(w.rpart, heart_test, type="class")
confusionMatrix(heart_test$target, pred)

########################################
#New decision trees

train_treeh<- rpart(target ~., data=heart_train, method="class", control=rpart.control(cp=0))
summary(train_treeh)
predictedh=predict(train_treeh, heart_test, type="class")
rsq.rpart(train_treeh)

plotcp(train_treeh)
fancyRpartPlot(train_treeh)
table(HeartDP=predictedh, true=heart_test$target)
#           true
#HeartDP     low risk high risk
#low risk        38         5
#high risk       10        48

########################################
#Decision trees part 3
library(tree)
tree_model <- tree(target ~ ., heart_train)
plot(tree_model)
text(tree_model, pretty = 0)

p2 <- predict(tree_model, heart_test, type = 'class')

# Confusion matrix - test data

(tab2 <- table(predicted = p2, Actual = heart_test$target))

# Miss classification error

(1 - sum(diag(tab2))/sum(tab2)) * 100

#Accuracy

(sum(diag(tab2))/sum(tab2)) * 100

## cross-validation to check where to stop pruning

cv_tree = cv.tree(tree_model, FUN = prune.misclass)

names(cv_tree)

plot(cv_tree$size,
     
     cv_tree$dev,
     
     type = 'b')

## Pruning the tree

pruned_model = prune.misclass(tree_model, best = 8)

plot(pruned_model)

text(pruned_model, pretty = 0)
# Prediction

# Confusion matrix - test

p2 <- predict(pruned_model, heart_test, type = 'class')

# Confusion matrix - test data

(tab2 <- table(predicted = p2, Actual = heart_test$target))

# Miss classification error

(1 - sum(diag(tab2))/sum(tab2)) * 100

#Accuracy

(sum(diag(tab2))/sum(tab2)) * 100
#######################################
# More trees
fit1 <- rpart(target ~ thalach + slope + exang+ oldpeak + ca + cp,  data=heart_train, method="class")

plot(fit1)

text(fit1)

rpart.plot(fit1, roundint = FALSE , digits = 4)
pred<-predict(fit1, heart_test, type="class")
confusionMatrix(heart_test$target, pred)


par(mfrow=c(1,1))
fit2 <- rpart(target~age + sex +chol+trestbps+cp , data=heart_train, method="class")

plot(fit2)

text(fit2)

rpart.plot(fit2, roundint = FALSE , digits = 4)

pred<-predict(fit2, heart_test, type="class")
confusionMatrix(heart_test$target, pred)

#######################################
#Naive Bayes
library(e1071)
library(caret)
nrows<-nrow(heart1) 
cutPoint<- floor(nrows/3*2) 
cutPoint 
## [1] 201 
rand<-sample(1:nrows) 
#training set 
heart_train <- heart[rand[1:cutPoint],] 
#test set 
heart_test <- heart[rand[(cutPoint+1:nrows)],] 
heart_test<-na.omit(heart_test) 

#first attempt
#classifier<- naiveBayes(x=heart_train[-14],
                        #y=heart_train$target)
#predicting the test set results
#y_pred= predict(classifier, newdata=heart_test[-14])
#making the confusion matrix
#cm= table(heart_test[,14], y_pred)


#Second attempt
# Build the classifier. 
nbTrain <- naiveBayes(as.factor(target) ~ ., data = heart_train)

# Again, test the results on the train set  
nbTrainPred <- predict(nbTrain, heart_test, type = 'class')
confusionMatrix(nbTrainPred, as.factor(heart_test$target))
par(mfrow = c(1,2))
plot(nbTrainPred, ylab = "Density", main = "NaiveBayes Plot", col="darkred", ylim = c(0,70))
actual<- table(heart_test$target)
barplot(actual, col = "darkred", ylab = "Density", main = "Test Dataset Plot", ylim = c(0,70))
