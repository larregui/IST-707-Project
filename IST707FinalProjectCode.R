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
detach(package:tm, unload=TRUE)
library(arules) #for ARM
library(arulesViz) #for ARM
library(RColorBrewer)#contains color palettes

# First lets load the data set
### link to dataset: https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility
#APA Citation: Naresh (2020). Health care: Data set on Heart Attack Possibility (Version 1) [CSV file]. Retrieved from https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility. 
fname<- file.choose()#browse for the file
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
heart$thal=dplyr::recode(heart$thal, "0"="Normal", "1"="Fixed Defect", "2"="Reversable Defect")
#head(heart$thal)# I see there is another level "3" which wasn't mentioned in the data info
heart$thal=dplyr::recode(heart$thal,"3"="") #I'm assuming this is the value behind 3
heart$cp=dplyr::recode(heart$cp, "0"="None", "1"="Low", "2"="Medium","3"="High")
#head(heart$cp)
heart$target=dplyr::recode(heart$target, "0"="low risk", "1"="high risk")
#head(heart$target)
#Discretize Age
## -- Testing first
heart$ï..age <- cut(heart$ï..age, breaks = c(0,20,30,40,50,60,70,80),
                labels=c("twenties","thirties","forties","fifties","sixties", "seventies", "eighties"))
head(heart$ï..age)

# Should we discreticize trestbps and chol?

#I would like to change the name of heart$ï..age to heart$age
#names(heart)
names(heart)[names(heart) == "ï..age"] <- "age"

# Observing the Distributions
#par(mfrow = c(2,2))#only used this if you want all graphs in the same window
a<- table(heart$age,heart$target)
a
barplot(a, main = "Heart Attack Risk across All Ages", beside = TRUE, col= brewer.pal(7, "Spectral"),legend.text = rownames((a)),args.legend=list(x="topright",bty="s"))
b<- table(heart$sex,heart$target)
b
barplot(b, main = "Heart Attack Risk VS Gender", beside = TRUE, col= c("purple", "lightblue2"),legend.text = rownames((b)),args.legend=list(x="topright",bty="s"))
c<- table(heart$thal,heart$target)
c
barplot(c, main = "Heart Attack Risk VS THAL", beside = TRUE, col= brewer.pal(4, "Spectral"),legend.text = rownames((c)),args.legend=list(x="topright",bty="s"))
d<- table(heart$cp,heart$target)
d
barplot(d, main = "Heart Attack Risk VS CP", beside = TRUE, col= brewer.pal(4, "Spectral"),legend.text = rownames((d)),args.legend=list(x="topright",bty="s"))

################################
#Correlation
library(corrplot)
fname<- file.choose()#browse for the file
heart1<- read.csv(fname, header = TRUE)
heart_cor <- cor(heart1) 
round(heart_cor, 2) 
corrplot(heart_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#Positive correlations are displayed in blue and negative correlations in red color. Color intensity and the size of the circle are proportional to the correlation coefficients.

#Correlation matrix with numbers 
corrplot(heart_cor, method = 'number', type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)

################################
#Tree Maps
library(treemap)
str(heart)
treemap(heart, index = c("age"), vSize = "chol")
treemap(heart, index = c("age"), vSize = "trestbps")
treemap(heart, index = c("age"), vSize = "oldpeak")
treemap(heart, index = c("age"), vSize = "thalach")
################################
#Association Rule Mining
head(heart)#test
newheart<- heart[,-4:-5]
newheart<- newheart[,-6]
newheart<- newheart[,-7]
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
rulesRightBySupp <- head(sort(rulesHeartRight, by="conf"), 20)  
inspect(rulesRightBySupp)

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
#precision when low risk
38/(38+5)# equals 88%
#precision when high risk
48/(10+48) # equals 83%
#recall when low risk
38/(38+10)# equals 79%
#recall when high risk
48/(5+48) # equals 91%

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
#Clustering 

##k-means
library(ggvis)
wss <- kmeans(heart1, centers = 1)$tot.withinss

for (i in 3:72){
  
  wss[i] <- kmeans(heart1, centers = i)$tot.withinss
  
}

sse <- data.frame(c(1:72), c(wss))

colnames(sse) <- c('Clusters', 'SSE')

sse %>% ggvis(~Clusters, ~SSE) %>% layer_points(fill := 'blue') %>% layer_lines()

kclust<- kmeans(heart1, centers = 5, nstart = 10)
fviz_cluster(kclust, data=heart1)
# 

##k-means: distance
dist1<-get_dist(hear1t, method = "manhattan")
fviz_dist(dist1, gradient=list(low="#00AFBB", mid= "white", high="#FC4E07"))

dist2<-get_dist(heart1, method = "euclidean")
fviz_dist(dist2, gradient=list(low="#00AFBB", mid= "white", high="#FC4E07"))

##HAC
clusComp <- hclust(dist(heart1), method = 'complete')

clusAvg <- hclust(dist(heart1), method = 'average')


# We can see that both groups use Euclidean distance and present 85 observations, but the cluster method varies. 

clusComp

clusAvg

plot(clusComp, hang = -1, cex = 0.6, main = "Heart Cluster - Complete")
plot(clusAvg, hang = -1, cex = 0.6, main = "Heart Cluster - Average")

