rm(list=ls())

# setting working dir

getwd()

setwd("D:/project_2")

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees',"readr","xgboost","class","Metrics")


lapply(x, require,character.only=T)

rm(x)

data=read_csv("day.csv")

dim(data)

class(data)

data=data.frame(data)

summary(data)

str(data)


# converting variablesinto their original data type

factor=c("season","yr","mnth","holiday","weekday","workingday","weathersit")

for(i in factor){
  data[,i]=as.factor(data[,i])
}


#visualizations

ggplot(data,aes(x=atemp,fill=season))+geom_histogram()+scale_x_discrete(labels=c("2011","2012")) +
  theme_bw()+ggtitle("season vs atemp")

ggplot(data, aes(x=mnth,fill=workingday)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("month vs working day")


ggplot(data, aes(x=mnth,fill=weekday)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("month vs working day")

ggplot(data, aes(x=weekday,fill=weathersit)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("month vs working day")

ggplot(data, aes(x=weathersit,fill=workingday)) +
  geom_bar(position="dodge")+theme_bw()+ggtitle("month vs working day")+scale_x_discrete(labels=c("cloudy","mist","rainy"))

ggplot(data, aes(x = atemp, y = hum, col= season)) +
  geom_point(shape = 2, size = 2)+ggtitle("atemp VS humidity")

ggplot(data, aes(x = atemp, y = windspeed, col= season)) +
  geom_point(shape = 2, size = 2)+ggtitle("atemp vs windspeed")

ggplot(data, aes(x = atemp, y = cnt, col= season)) +
  geom_point(shape = 2, size = 2)+ggtitle("atemp vs count")

ggplot(data, aes(x = windspeed, y = cnt, col= season)) +
  geom_point(shape = 2, size = 2)+ggtitle("windspeed vs count")

ggplot(data, aes(x = hum, y = cnt, col= season)) +
  geom_point(shape = 2, size = 2)+ggtitle("humidity vs count")


ggplot(data, aes(x = atemp, y = cnt, col= yr)) +
  geom_point(shape = 2, size = 2)+ggtitle("atemp vs count vs year")

ggplot(data, aes(x = windspeed, y = cnt, col= yr)) +
  geom_point(shape = 2, size = 2)+ggtitle("windspeed vs count vs year")

ggplot(data, aes(x = hum, y = cnt, col= yr)) +
  geom_point(shape = 2, size = 2)+ggtitle("humidity vs count vs year")


#########################################################################################################

ggplot(data, aes(x = season, y = cnt, fill = season)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  ylab("count") +
  ggtitle("count VS season") +
  scale_fill_manual(values = c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c("Spring", "Summer", "Fall","Winter"))


###############################################################################################

ggplot(data, aes(x = atemp, y = cnt, color = weekday)) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("atemp") +
  ylab("count of Bike Rentals") +
  ggtitle("count VS atemp") +
  
  scale_color_discrete(name = "Weekday:",
                       breaks = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("sun","mon","tue","wed","thurs","fri","sat"))+
  theme(plot.title = element_text(size = 11, face="bold"))


########################################################################################################

ggplot(data, aes(x = workingday, y = cnt, fill =season)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("workingday") +
  ylab("count of Bike Rentals") +
  ggtitle("workingday VS count") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    breaks=c(1, 2, 3, 4),
                    labels=c( "Spring", "Summer", "Fall","Winter")) +
  theme(plot.title = element_text(size = 11, face="bold"))

#######################################################################################3


ggplot(data, aes(x = atemp, y = cnt, color = weathersit)) +
  geom_smooth(fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("atemp") +
  ylab("count of Bike Rentals") +
  ggtitle("count VS atemp with weather") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  " Heavy Rain + Ice Pallets ")) +
  theme(plot.title = element_text(size = 11, face="bold"))


###########################################################################################

ggplot(data, aes(x = weathersit, y = cnt, fill = weathersit)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Weather") +
  ylab("count of Bike Rentals") +
  ggtitle("weather VS count") +
  scale_fill_manual(values = c("#E74C3C", "#58D68D", "#5DADE2", "#F4D03F"), 
                    name = "Type of Weather:",
                    breaks = c(1, 2, 3, 4),
                    labels = c("Clear or Cloudy ", 
                               " Mist ", 
                               " Light Rain  or Light Snow ", 
                               "Heavy rain or ice pallets ")) +
  theme(plot.title = element_text(size = 11, face="bold"))

##################################################################################################

ggplot(data, aes(x = hum, y = cnt, color = weathersit)) +
  geom_smooth(method = 'loess', fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Humidity") +
  ylab("count of Bike Rentals") +
  ggtitle("humidity vs count") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "")) +
  theme(plot.title = element_text(size = 11, face="bold"))
################################################################################################

ggplot(data, aes(x = windspeed, y = cnt, color = weathersit)) +
  geom_smooth(fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("WindSpeed") +
  ylab("count of Bike Rentals") +
  ggtitle("wind speed vs count") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "")) +
  theme(plot.title = element_text(size = 11, face="bold"))



########################################################################################
ggplot(data,aes(x=season,y=cnt,fill=season))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("season vs count")

ggplot(data,aes(x=season,y=atemp,fill=season))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("season vs atemp")

ggplot(data,aes(x=season,y=windspeed,fill=season))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("season vs windspeed")

ggplot(data,aes(x=yr,y=cnt,fill=season))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("year vs count")

ggplot(data,aes(x=yr,y=atemp,fill=season))+
  geom_boxplot(outlier.color ="red",outlier.size = 3)+ggtitle("year vs count")



hist(data$cnt)

hist(data$atemp)

hist(data$hum)

hist(data$windspeed)

# in the given data set the variables cnt is the sum of casual and registered
# so we removing casual and registered

data=data[,-c(14,15)]

str(data)
# checking for missing values

sum(is.na(data))

# outlier analysis

ind=sapply(data,is.numeric)

data_num=data[,ind]

cn=colnames(data_num)

cn

for(i in cn){
  print(i)
  v=data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
print(v)  
}

#plotting for outliers

for (i in 1:length(cn)) {
  assign(paste0("gn",i), ggplot(aes_string( y = (cn[i]), x= "cnt") , data = subset(data)) + 
           stat_boxplot(geom = "errorbar" , width = 0.5) +
           geom_boxplot(outlier.color = "red", fill = "grey", outlier.shape = 20, outlier.size = 1, notch = FALSE)+
           theme(legend.position = "bottom")+
           labs(y = cn[i], x= "cnt")+
           ggtitle(paste("Boxplot" , cn[i])))
  #print(i)
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3,nrow=2)

# removing outliers

cn

for(i in cn){
  v=data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
  data=data[which(!data[,i]%in%v),]
}

#attaching data

attach(data)

# correlation analysis

corrgram(data_num,order=F,upper.panel = panel.pie,text.panel = panel.txt,main="correlation plot")

# checking anova 

m1=aov(cnt~season+yr+mnth+holiday+weekday+workingday+weathersit)

summary(m1)

# removing the variables which are not carrying much information

data_del=subset(data,select=-c(instant,temp,holiday,dteday))

# as the data is normally distributed we are not doing feature scaling

## dividing the data into train and test sets

rmExcept("data_del")

data=data_del

dt_ind=sample(1:nrow(data),0.8*nrow(data))

train=data[dt_ind,]

test=data[-dt_ind,]


# applying desicion tree

rg=rpart(cnt~.,train,method="anova")

rg

dt_prd=predict(rg,test[,-10])

dt_prd

write(capture.output(summary(rg)), "c50Rules.txt")

# as it is a time series data we have to use rmse and rmsle

regr.eval(test[,10],dt_prd,stats = c("mape","rmse","mae"))

actual=test[,10]

pred=data.frame(dt_prd)

err=actual-pred

rmse <- function(error)
{
  sqrt(mean(error^2))
}

rmse(err)

require(Metrics)

rmsle(test[,10],dt_prd)

rmsle_dT=rmsle(test[,10],dt_prd)

# rmse=890.5
#rmsle=0.2867

##### building regression model

lm_mod=lm(cnt~.,train)

summary(lm_mod)

lm_prd=predict(lm_mod,test[,-10])

lm_prd=round(lm_prd)

lm_prd

regr.eval(test[,10], lm_prd, stats = "rmse")

summary(lm_prd)

# as we can see there are some negetive values so we have to replace those values with min cnt values

#min(cnt)

#max(cnt)

#pred = lm_prd

#pred[pred<=0] = 22

rmsle(test[,10],pred)

rmsle_lm=rmsle(test[,10],pred)

#rmse = 890.49
#rmsle = 0.3298


#random forest

rf=randomForest(cnt~.,train,ntree=500,importance=T,bootstrap=T)

rf

#extracting rules from random forest

tree_list=RF2List(rf)

#extract rules

exct=extractRules(tree_list,train[,-10])

#visualize some rules

exct[1:2,]

#making the rules readable

readable_rules=presentRules(exct,colnames(train))

readable_rules[1:2,]

# rule  metric

rule_metric=getRuleMetric(exct,train[,-10],train$cnt)

rule_metric[1:2,]


rf_prd=predict(rf,test[,-10])

rf_prd

summary(rf_prd)

regr.eval(test[,10],rf_prd,stats = "rmse")

rmsle(test[,10],rf_prd)

rmsle_rf=rmsle(test[,10],rf_prd)

#for 100 trees
#rmse = 637.159
#rmsle = 0.264

#for 500 trees 
#rmse = 636.159
#rmsle = 0.261

# applying XGboost


Prediction <- function(model,data,features){
  predictions <- numeric(nrow(data))
  counter <- 1
  for (model in model){
    if (class(model)[1]=="xgb.Booster"){
      usedata <- data.matrix(data[features[[counter]]])
    } else{
      usedata <- data[features[[counter]]]
    }
    pred_test <- predict(model,usedata)
    pred_test[pred_test<0]=0
           predictions <- predictions+pred_test
    counter <- counter + 1
  }
  return(predictions)
}

evaluate <- function(true,predictions){
  return(rmsle(true, predictions))
}

features=c("season","yr","mnth","weekday","workingday","weathersit","atemp","hum","windspeed")

xgb_mod <- xgboost(data = data.matrix(train[,features]),label = train$cnt,objective = "reg:linear",eval_metric = "rmse",max.depth = 5,nround = 10)                      

xgb_prd = Prediction(list(xgb_mod),test,list(features))

evaluate(test$cnt,xgb_prd)

rmsle_xg=evaluate(test$cnt,xgb_prd)

regr.eval(test[,10],xgb_prd,stats = "rmse")

#RMSE=742.32
#RMSLE=0.2525

xgb.ggplot.importance(xgb.importance(model=xgb_mod))

# svm model

svm_mod<- svm(cnt~.,data=train,kernel="radial",scale=TRUE,epsilon=0.1,gamma=0.17)

svm_mod

svm_prd = Prediction(list(svm_mod),test,list(features))

predict(svm_mod,test[,-10])

evaluate(test$cnt,svm_prd)

rmsle_svm=evaluate(test$cnt,svm_prd)

regr.eval(test[,10],svm_prd,stats="rmse")

prds=data.frame(svm_prd,xgb_prd,lm_prd,rf_prd,dt_prd)

out2=cbind(round(xgb_prd),test)

names(out2)[1]="predicted"

out2=out2[,c(2:11,1)]

out2=out2[,-10]

write.csv(out2,"sample_output.csv", row.names = F)


