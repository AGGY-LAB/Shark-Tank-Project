


#Install the packages to be used:
 
library(tidyverse)
library(caret)




project<-read.csv("Shark Tank.csv")
str(project)


project%>%group_by(deal)%>%summarize(n=(n()/nrow(project))*100)



length(unique(project$episode))
project%>%mutate(deal=factor(deal),episode=factor(episode))%>%group_by(deal)%>%ggplot(aes(episode,fill=deal))+geom_bar()

project%>%group_by(episode)%>%summarize(n=n(),n_false=round(sum(deal=="False")/n*100,0),n_true=round(sum(deal=="True")/n*100,0))%>%arrange(n)%>%as.data.frame()
n_episode<-project%>%group_by(episode)%>%summarize(n=n(),n_false=round(sum(deal=="False")/n*100,0),n_true=round(sum(deal=="True")/n*100,0))%>%arrange(n)%>%as.data.frame()
cor(n_episode$episode,n_episode$n_false)
cor(n_episode$episode,n_episode$n_true)

length(unique(project$category))





n_category<- project%>%group_by(category)%>%summarize(n=n(),n_false=round(sum(deal=="False")/n*100,0),n_true=round(sum(deal=="True")/n*100,0))%>%arrange(n)%>%as.data.frame()   

project%>%mutate(deal=factor(deal),category=factor(category))%>%group_by(deal)%>%ggplot(aes(category,fill=deal))+geom_bar()+theme(axis.text.x =element_text(angle = 90))






#season  

unique(project$season)


#episode.season 

unique(project$episode.season)





#Shark1, shark2, shark3, shark4 and shark5 

unique(project$shark1)



unique(project$shark2)


unique(project$shark3)



unique(project$shark4)


unique(project$shark5)



table(project$shark1,project$deal)
table(project$shark2,project$deal)
table(project$shark3,project$deal)
table(project$shark4,project$deal)
table(project$shark5,project$deal)




project_trn<-project%>%mutate(deal=factor(deal),episode=factor(episode),category=factor(category),season=factor(season),shark1=factor(shark1),shark2=factor(shark2),shark3=factor(shark3),shark4=factor(shark4),shark5=factor(shark5),episode.season=factor(episode.season),Multiple.Entreprenuers=factor(Multiple.Entreprenuers))


str(project_trn)



##Modeling Approach

#split the project into train,test and validation sets.

set.seed(1)
index_test<-createDataPartition(project_trn$deal,times = 1,p=0.5,list = FALSE)
train<-project_trn[-index_test,]
test<-project_trn[index_test,]
dim(train)
dim(test)






#1. Classification (Decision) Trees - Rpart


train_rpart<-train(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers,method="rpart", data=train)
y_hat_rpart<-predict(train_rpart,test)
rpart_Accuracy<-confusionMatrix(y_hat_rpart,test$deal)$overall[["Accuracy"]]
rpart_Accuracy




Results<-data.frame(Model="Decision Trees",Accuracy=rpart_Accuracy)
Results





#2.Random Forest



library(randomForest)




library(caret)

train_rf <- randomForest(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers, data =train)
deal_hat<-predict(train_rf,test)
rf_Accuracy<-confusionMatrix(deal_hat,test$deal)$overall["Accuracy"]


Results<-rbind(Results,data.frame(Model="RandomForest",Accuracy=rf_Accuracy))
Results




# Random Forest with crossvalidation


train_rf_cv <- train(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers,
      method = "Rborist",
      tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
      data = train)
y_hat_rf_cv<-predict(train_rf_2, test)
rf_cv_Accuracy<-confusionMatrix(y_hat_rf_cv,test$deal)$overall["Accuracy"]




Results<-rbind(Results,data.frame(Model="RandomForest with Cross Validation",Accuracy=rf_cv_Accuracy))
Results






#3.Logistic Regression


train_glm<-train(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers,method="glm",data=train)
y_hat_glm<-predict(train_glm,test)
glm_Accuracy<-confusionMatrix(y_hat_glm,test$deal)$overall[["Accuracy"]]



Results<-rbind(Results,data.frame(Model="Logistic Regression",Accuracy=glm_Accuracy))
Results



#4. K Nearest Neighbours


train_knn <- train(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers, method = "knn", 
                   data = train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

control <- trainControl(method = "cv", number = 10, p = .9)

train_knn_cv <- train(deal ~episode+askedFor+exchangeForStake+valuation +season +shark1+shark2+shark3+shark4+shark5+Multiple.Entreprenuers, method = "knn", 
                      data= train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
knn_Accuracy<-confusionMatrix(predict(train_knn_cv, test, type = "raw"),
               test$deal)$overall["Accuracy"]




Results<-rbind(Results,data.frame(Model="KNN",Accuracy=knn_Accuracy))
Results











