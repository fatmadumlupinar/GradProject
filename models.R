setwd("C:/Users/fatma/Desktop/IE 492")

library(caret)
library(tidyr)
library(dplyr)
library(forecast)
library(data.table)
library(lubridate)
library(bnlearn)
library(hrbrthemes)
BNDataset=read.csv("BNData_before2020.csv")
specialDays=read.csv("specialdaysfinal.csv")
specialDays$Date=as.Date(specialDays$Date)
specialDays=specialDays%>%mutate(NegativeSpecialDays=ifelse(NegativeSpecialDays>=1, 1,0))
specialDays$NegativeSpecialDays=as.factor(specialDays$NegativeSpecialDays)
specialDays$PositiveSpecialDays=as.factor(specialDays$PositiveSpecialDays)
specialDays=specialDays[,-5]
specialDays=specialDays%>%filter(Date<"2020-01-01" &Date>"2017-01-01")

str(specialDays)
BNDataset$day=as.factor(BNDataset$day)
BNDataset$Hour=as.factor(BNDataset$Hour)
BNDataset$Date=as.Date(BNDataset$Date)
BNDataset=BNDataset%>%fill(USDExchangeRate)
BNDataset=BNDataset[,-c(13,14)]
#varImp(fit5)

str(BNDataset)

BNDataset=cbind(BNDataset,specialDays,by="Date")
BNDataset=BNDataset[,-c(1,13,14,17)]
str(BNDataset)
BNDataset=BNDataset%>%mutate(FuelPrice=FuelPrice/USDExchangeRate)%>%select(-USDExchangeRate)

# BNDatasetTrain=BNDataset%>%filter(Date<"2019-02-01")
# BNDatasetTest=BNDataset%>%filter(Date>="2019-02-01")

BNDataset=BNDataset%>%fill(FuelPrice)



#TrainData=BNDatasetTrain[,-c(2)]%>%filter(Hour==1)
#str(TrainData)
#TrainData=TrainData[,-2]

#TestData=BNDatasetTest[,-c(2)]%>%filter(Hour==1)
#str(TestData)
#TestData=TestData[,-2]

###

PredLR<-c()
i=1
for(i in 1:12){
  if(i<10){
    date<-as.Date(paste0("2019-0",i,"-01"))
  }
  else {
    date<-as.Date(paste0("2019-",i,"-01"))
  }
  
  LR <- lm(formula = MCP ~ ., data  = (BNDataset%>%filter(Date<date))[,-1] )
  
  Pred<-predict(LR,(BNDataset%>%filter(Date<(date %m+% months(1))& Date>=date))[,-1])
  PredLR<-c(PredLR,Pred)
  
}

summary(LR)
LR$model
accuracy(PredLR, (BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))[,-1]$MCP)

Forecast<-(BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))

Forecast=cbind(Forecast,PredictedMCP_LR=PredLR)
#####


fitControl <- trainControl(method = "timeslice",
                           initialWindow=24*30*4, fixedWindow=TRUE, 
                           horizon=24*30,
                           skip=24*30-1)

####### Stochastic Gradient Boosting
PredSGB<-c()
i=1
for(i in 1:12){
if(i<10){
  date<-as.Date(paste0("2019-0",i,"-01"))
}
else {
  date<-as.Date(paste0("2019-",i,"-01"))
}
gbmFit <- train(MCP ~ ., data = (BNDataset%>%filter(Date<date))[,-1], 
                 method = "gbm", 
                 trControl = fitControl)

Pred<-predict(gbmFit,(BNDataset%>%filter(Date<(date %m+% months(1))& Date>=date))[,-1])
PredSGB<-c(PredSGB,Pred)

}


accuracy(PredSGB, (BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))[,-1]$MCP)


Forecast=cbind(Forecast,PredictedMCP_SGB=PredSGB)
summary(gbmFit)
varImp(gbmFit)
####### Decision Tree

PredDecTree<-c()
i=1
for(i in 1:12){
  if(i<10){
    date<-as.Date(paste0("2019-0",i,"-01"))
  }
  else {
    date<-as.Date(paste0("2019-",i,"-01"))
  }
  DecTree <- train(MCP ~ ., data = (BNDataset%>%filter(Date<date))[,-1], 
                  method = "rpart", 
                  trControl = fitControl)
  
  Pred<-predict(DecTree,(BNDataset%>%filter(Date<(date %m+% months(1))& Date>=date))[,-1])
  PredDecTree<-c(PredDecTree,Pred)
  
}


accuracy(PredDecTree,  (BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))[,-1]$MCP)
plot(DecTree$finalModel)
fancyRpartPlot(DecTree$finalModel)
varImp(DecTree)
Forecast=cbind(Forecast,PredictedMCP_DT=PredDecTree)
library(rattle)
##########################

PredRF<-c()
i=0

RFtuneGrid=expand.grid(mtry = c(5,15))

for(i in 1:12){
  if(i<10){
    date<-as.Date(paste0("2019-0",i,"-01"))
  }
  else {
    date<-as.Date(paste0("2019-",i,"-01"))
  }
  RF <- train(MCP ~ ., data = (BNDataset%>%filter(Date<date))[,-1], 
                   method = "rf", 
                   trControl = fitControl,
              ntree=50,
              tuneGrid=RFtuneGrid)
  
  Pred<-predict(RF,(BNDataset%>%filter(Date<(date %m+% months(1))& Date>=date))[,-1])
  PredRF<-c(PredRF,Pred)
  
}

accuracy(PredRF,  (BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))[,-1]$MCP)
varImp(RF)

Forecast=cbind(Forecast,PredictedMCP_RF=PredRF)




###### Bayesian Network

###################################################
#Directed acyclic graph
names=c("Hour","day","FuelPrice","Generation","BilateralContacts.Consumption","Consumption","MCP","lag_24","RenewableProportion","NegativeSpecialDays","PositiveSpecialDays")
dag = model2network("[day][Hour][FuelPrice][RenewableProportion][Generation|FuelPrice][BilateralContacts.Consumption][Consumption][MCP|Generation:BilateralContacts.Consumption:Consumption:RenewableProportion][NegativeSpecialDays][PositiveSpecialDays][lag_24]")
dag <- set.arc(dag, "NegativeSpecialDays", "Consumption")
dag <- set.arc(dag, "PositiveSpecialDays", "Consumption")

dag <- set.arc(dag, "NegativeSpecialDays", "Generation")
dag <- set.arc(dag, "PositiveSpecialDays", "Generation")

dag <- set.arc(dag, "Hour", "Generation")
dag <- set.arc(dag, "Hour", "Consumption")
dag <- set.arc(dag, "day", "Generation")
dag <- set.arc(dag, "day", "Consumption")
dag <- set.arc(dag, "Consumption","Generation")

dag <- set.arc(dag, "lag_24", "MCP")

plot(dag)

 date=as.Date("2019-01-01")
 fit=bn.fit(dag,(BNDataset%>%filter(Date<date))[,names] )
print(fit)
# 
bn.fit.qqplot(fit$MCP)
# bn.fit.qqplot(fit$Generation)
# bn.fit.qqplot(fit$Consumption)
bn.fit.xyplot(fit$MCP)
 bn.fit.histogram(fit$MCP)
# 
# pred=predict(fit, node="MCP",data=na.omit((BNDataset%>%filter(Date<as.Date("2020-01-01")& Date>=date))[,names]))
# 
# result=data.frame(predicted=pred, Actual=na.omit((BNDataset%>%filter(Date<as.Date("2020-01-01")& Date>=date))[,names])$MCP, index=1:(length(result$Actual)))
# 
# MAPE(result$predicted,result$Actual)
# accuracy(result$predicted,result$Actual)
# 
# ggplot(result)+
#   geom_line(aes(x=index,y=Actual),color="green")+
#   geom_line(aes(x=index,y=predicted), color="red")


########################################### monthly additive bayesian network

PredBN<-c()
i=0
for(i in 1:12){
  if(i<10){
    date<-as.Date(paste0("2019-0",i,"-01"))
  }
  else {
    date<-as.Date(paste0("2019-",i,"-01"))
  }
  
  fit=bn.fit(dag,(BNDataset%>%filter(Date<date))[,names] )
  
  Pred=predict(fit, node="MCP",data=na.omit((BNDataset%>%filter(Date<(date %m+% months(1))& Date>=date))[,names]))
  
  PredBN<-c(PredBN,Pred)
  
}



accuracy(PredBN,((BNDataset%>%filter(Date<"2020-01-01"& Date>="2019-01-01"))[,-1]$MCP))


Forecast=cbind(Forecast,PredictedMCP_BN=PredBN)


###########################################################Comparison of Performances
Forecast=read.csv("Forecast.csv")
Forecast$Date=as.Date(Forecast$Date)
Forecast$Hour=as.factor(Forecast$Hour)
Forecast$day=as.factor(Forecast$day)
Forecast$NegativeSpecialDays=as.factor(Forecast$NegativeSpecialDays)
Forecast$PositiveSpecialDays=as.factor(Forecast$PositiveSpecialDays)
str(Forecast)



PredARIMA=readxl::read_excel("arima_pred.xlsx")

str(PredARIMA)
Forecast=cbind(Forecast, PredARIMA[,6])
Forecast$PredictedMCP_ARIMA=Forecast$predicted
Forecast=Forecast[,-19]
str(Forecast)

error_test <- function(actual, forecasted){
  n=length(actual)
  error = actual-forecasted
  mean=mean(actual)
  sd=sd(actual)
  bias = sum(error)/sum(actual)
  mape = sum(abs(error/actual))/n
  mad = sum(abs(error))#mae
  wmape = mad/mean
  rmse= sqrt(sum(error^2)/n) #buna göre yapýcaz
  mae=sum(abs(error))/n #
  MPE = sum(error/actual)/n
  df = data.frame(n,mean,sd,bias,mape,mad,wmape,rmse,mae,MPE)
  return(df)
}


# MAE_LR=c()
# MAE_DT=c()
# MAE_RF=c()
# MAE_SGB=c()
# MAE_BN=c()
# 
# RMSE_LR=c()
# RMSE_DT=c()
# RMSE_RF=c()
# RMSE_SGB=c()
# RMSE_BN=c()
# 
# 
# i=0
# for(i in 0:23){
# hourlyForecast=Forecast%>%filter(Hour==i)
# 
# 
# hourlyResults=error_test(hourlyForecast$MCP,hourlyForecast$PredictedMCP_LR)
# MAE_LR=c(MAE_LR,hourlyResults$mae) 
# RMSE_LR=c(RMSE_LR,hourlyResults$rmse) 
# 
# 
# hourlyResultsDT=error_test(hourlyForecast$MCP,hourlyForecast$PredictedMCP_DT)
# MAE_DT=c(MAE_DT,hourlyResultsDT$mae) 
# RMSE_DT=c(RMSE_DT,hourlyResultsDT$rmse) 
# 
# hourlyResultsSGB=error_test(hourlyForecast$MCP,hourlyForecast$PredictedMCP_SGB)
# MAE_SGB=c(MAE_SGB,hourlyResultsSGB$mae) 
# RMSE_SGB=c(RMSE_SGB,hourlyResultsSGB$rmse) 
# 
# hourlyResultsRF=error_test(hourlyForecast$MCP,hourlyForecast$PredictedMCP_RF)
# MAE_RF=c(MAE_RF,hourlyResultsRF$mae) 
# RMSE_RF=c(RMSE_RF,hourlyResultsRF$rmse) 
# 
# hourlyResultsBN=error_test(hourlyForecast$MCP,hourlyForecast$PredictedMCP_BN)
# MAE_BN=c(MAE_BN,hourlyResultsBN$mae) 
# RMSE_BN=c(RMSE_BN,hourlyResultsBN$rmse) 
# 
# }
# 
# RMSE_BN=c(11.982022,  9.315508,  9.066990, 11.093466, 10.939353,  9.889360,  9.837279, 10.442440, 10.020847, 11.618282,  9.856135,
# 9.253583,  9.205206 ,10.385998 , 8.784217,  8.830627,  8.653424,  7.871278,  7.753103,  5.997381 , 4.838114,  4.491787,
# 5.907564,  8.654004)
# MAE_BN=c(8.431576, 6.340649, 7.020371, 8.817017, 8.488154, 7.720879, 7.352109, 7.896686, 7.206552, 7.876507, 6.626376, 6.045463,
# 6.411679, 7.153206, 5.923720 ,6.079733 ,5.810577, 5.215329, 4.747765 ,3.507157, 3.012411, 3.009625, 3.863873 ,5.905834)
# 
# RMSE_SARIMA<-c(10.961034,  8.679907,  8.903149, 11.294907, 10.360578  ,9.631053, 
#               9.179315, 10.273837,  7.141368,  8.227345,  8.624408 , 9.500359, 
#               10.721257 ,11.020377, 11.195163, 10.655801,  9.799427  ,7.904881 , 
#               7.180061 , 5.161375,  4.284662, 4.052312,  5.582587 , 8.394674)
# 
# MAE_SARIMA<-c( 7.395031, 5.521686, 6.261176,8.477715, 7.611576, 7.284767, 6.963222, 7.442790,
#               3.679166, 4.429119, 4.591308, 5.228094, 7.085296, 7.378398, 7.110944, 6.670787,
#               5.992710, 4.362683, 3.462143, 2.410927, 2.297011, 2.152030, 3.078528, 5.106537)
# 
# PlotDataMAE=data.frame(Hour=0:23,`Linear Regression`=MAE_LR,`Decision Tree`=MAE_DT,`Stochastic Gradient Boosting`=MAE_SGB,`Random Forest`=MAE_RF,`Bayesian Network`=MAE_BN,`Seasonal ARIMA`=MAE_SARIMA)
# PlotDataRMSE=data.frame(Hour=0:23,`Linear Regression`=RMSE_LR,`Decision Tree`=RMSE_DT,`Stochastic Gradient Boosting`=RMSE_SGB,`Random Forest`=RMSE_RF,`Bayesian Network`=RMSE_BN,`Seasonal ARIMA`=RMSE_SARIMA)
# 
# BoxPlotDataMAE<-PlotDataMAE%>%pivot_longer(.,cols=-c(Hour),names_to="Model Type",values_to="MAE")
# BoxPlotDataRMSE<-PlotDataRMSE%>%pivot_longer(.,cols=-c(Hour),names_to="Model Type",values_to="RMSE")
# 
# BoxPlotData=merge(BoxPlotDataMAE,BoxPlotDataRMSE,by=c("Hour","Model Type"))
# BoxPlotData=BoxPlotData%>%pivot_longer(.,cols=c(MAE,RMSE),names_to="Metric Type",values_to="Performance Value")
# 
# ## Hourly Analysis
# 
# 
# ggplot(BoxPlotData)+
#   geom_col(aes(x=Hour, y=`Performance Value`, fill=`Metric Type`),position="dodge")+
#   facet_wrap(~`Model Type`)+
#   theme(legend.position = "none")+
#   theme_ipsum()+
#   ggtitle("Comparison of the Models",
#           subtitle = "Hourly Analysis")
# 
# 
# 
# ggplot(BoxPlotData, aes(x=`Model Type`, y=`Performance Value`,fill=`Metric Type`)) + 
#   geom_boxplot() +
#   theme()+
#   theme_ipsum()+
#   ggtitle("Comparison of the Models",
#           subtitle = "Hourly Analysis")
# 
# 
# 
# 


### GENERAL Daily

Performance=Forecast%>%group_by(Date)%>%
  summarise(RMSE_LR=(error_test(MCP,PredictedMCP_LR))$rmse,
            MAE_LR=(error_test(MCP, PredictedMCP_LR))$mae,
            RMSE_DT=(error_test(MCP, PredictedMCP_DT))$rmse,
            MAE_DT=(error_test(MCP, PredictedMCP_DT))$mae,
            RMSE_RF=(error_test(MCP, PredictedMCP_RF))$rmse,
            MAE_RF=(error_test(MCP, PredictedMCP_RF))$mae,
            RMSE_SGB=(error_test(MCP, PredictedMCP_SGB))$rmse,
            MAE_SGB=(error_test(MCP, PredictedMCP_SGB))$mae,
            RMSE_BN=(error_test(MCP, PredictedMCP_BN))$rmse,
            MAE_BN=(error_test(MCP,PredictedMCP_BN))$mae,
            RMSE_ARIMA=(error_test(MCP, PredictedMCP_ARIMA))$rmse,
            MAE_ARIMA=(error_test(MCP,PredictedMCP_ARIMA))$mae)

Performance=as.data.frame(Performance)

BoxPlotRMSE=Performance%>%select(-c(MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA))%>%
  pivot_longer(.,cols=-c(Date),names_to="RMSE Model",values_to="RMSE Value")

BoxPlotMAE=Performance%>%select(Date,MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA)%>%
  pivot_longer(.,cols=-c(Date),names_to="MAE Model",values_to="MAE Value")

str(BoxPlotRMSE)

ggplot(BoxPlotRMSE%>%filter(`RMSE Value`<35), aes(x=as.factor(`RMSE Model`), y=`RMSE Value`,fill=as.factor(`RMSE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "General Model Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")+
  scale_x_discrete(labels=c("RMSE_BN" = "Bayesian Network", 
                            "RMSE_DT"= "Decision Tree",
                            "RMSE_LR" = "Linear Regression",
                            "RMSE_RF"="Random Forest",
                            "RMSE_SGB" = "Stochastic Gradient Boosting",
                            "RMSE_ARIMA"="Seasonal ARIMA"
  ))




######### HOURLY 2



HourlyPerformance=Forecast%>%group_by(Hour,Date)%>%
  summarise(RMSE_LR=(error_test(MCP,PredictedMCP_LR))$rmse,
            MAE_LR=(error_test(MCP, PredictedMCP_LR))$mae,
            RMSE_DT=(error_test(MCP, PredictedMCP_DT))$rmse,
            MAE_DT=(error_test(MCP, PredictedMCP_DT))$mae,
            RMSE_RF=(error_test(MCP, PredictedMCP_RF))$rmse,
            MAE_RF=(error_test(MCP, PredictedMCP_RF))$mae,
            RMSE_SGB=(error_test(MCP, PredictedMCP_SGB))$rmse,
            MAE_SGB=(error_test(MCP, PredictedMCP_SGB))$mae,
            RMSE_BN=(error_test(MCP, PredictedMCP_BN))$rmse,
            MAE_BN=(error_test(MCP,PredictedMCP_BN))$mae,
            RMSE_ARIMA=(error_test(MCP, PredictedMCP_ARIMA))$rmse,
            MAE_ARIMA=(error_test(MCP,PredictedMCP_ARIMA))$mae)

HourlyPerformance=as.data.frame(HourlyPerformance)

HourlyBoxPlotRMSE=HourlyPerformance%>%select(-c(MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA))%>%
  pivot_longer(.,cols=-c(Date,Hour),names_to="RMSE Model",values_to="RMSE Value")

HourlyBoxPlotMAE=HourlyPerformance%>%select(Date,Hour,MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA)%>%
  pivot_longer(.,cols=-c(Date,Hour),names_to="MAE Model",values_to="MAE Value")

str(HourlyBoxPlotRMSE)

ggplot(HourlyBoxPlotRMSE%>%filter(`RMSE Value`<35), aes(x=Hour, y=`RMSE Value`,fill=as.factor(`RMSE Model`))) + 
  geom_boxplot(outlier.shape = NA) +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Hourly Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")

ggplot(HourlyBoxPlotMAE, aes(x=Hour, y=`MAE Value`,fill=as.factor(`MAE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Hourly Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")


######### SEASONAL 



seasons <- c(
  "01" = "Winter", "02" = "Winter",
  "03" = "Spring", "04" = "Spring", "05" = "Spring",
  "06" = "Summer", "07" = "Summer", "08" = "Summer",
  "09" = "Fall", "10" = "Fall", "11" = "Fall",
  "12" = "Winter"
)

Forecast=Forecast%>%mutate(season=seasons[format(Date,"%m")])


str(Forecast)
SeasonalPerformance=Forecast%>%group_by(season,Date)%>%
  summarise(RMSE_LR=(error_test(MCP,PredictedMCP_LR))$rmse,
            MAE_LR=(error_test(MCP, PredictedMCP_LR))$mae,
            RMSE_DT=(error_test(MCP, PredictedMCP_DT))$rmse,
            MAE_DT=(error_test(MCP, PredictedMCP_DT))$mae,
            RMSE_RF=(error_test(MCP, PredictedMCP_RF))$rmse,
            MAE_RF=(error_test(MCP, PredictedMCP_RF))$mae,
            RMSE_SGB=(error_test(MCP, PredictedMCP_SGB))$rmse,
            MAE_SGB=(error_test(MCP, PredictedMCP_SGB))$mae,
            RMSE_BN=(error_test(MCP, PredictedMCP_BN))$rmse,
            MAE_BN=(error_test(MCP,PredictedMCP_BN))$mae,
            RMSE_ARIMA=(error_test(MCP, PredictedMCP_ARIMA))$rmse,
            MAE_ARIMA=(error_test(MCP,PredictedMCP_ARIMA))$mae)

SeasonalPerformance=as.data.frame(SeasonalPerformance)
SeasonalPerformance$season=as.factor(SeasonalPerformance$season)
str(SeasonalPerformance)
# i=0
# for(i in 0:3){
#   if(i==0){ 
#     seasonalForecast=Forecast%>%filter(season=="Winter")} else if (i==1){
#     seasonalForecast=Forecast%>%filter(season=="Spring")}else if(i==2){
#     seasonalForecast=Forecast%>%filter(season=="Summer")}else if(i==3){
#     seasonalForecast=Forecast%>%filter(season=="Fall")}
#   
#   seasonalResults=error_test(seasonalForecast$MCP,seasonalForecast$PredictedMCP_LR)
#   MAE_LR=c(MAE_LR,seasonalResults$mae) 
#   RMSE_LR=c(RMSE_LR,seasonalResults$rmse) 
#   
#   
#   seasonalResultsDT=error_test(seasonalForecast$MCP,seasonalForecast$PredictedMCP_DT)
#   MAE_DT=c(MAE_DT,seasonalResultsDT$mae) 
#   RMSE_DT=c(RMSE_DT,seasonalResultsDT$rmse) 
#   
#   seasonalResultsSGB=error_test(seasonalForecast$MCP,seasonalForecast$PredictedMCP_SGB)
#   MAE_SGB=c(MAE_SGB,seasonalResultsSGB$mae) 
#   RMSE_SGB=c(RMSE_SGB,seasonalResultsSGB$rmse) 
#   
#   seasonalResultsRF=error_test(seasonalForecast$MCP,seasonalForecast$PredictedMCP_RF)
#   MAE_RF=c(MAE_RF,seasonalResultsRF$mae) 
#   RMSE_RF=c(RMSE_RF,seasonalResultsRF$rmse) 
#   
#   seasonalResultsBN=error_test(seasonalForecast$MCP,seasonalForecast$PredictedMCP_BN)
#   MAE_BN=c(MAE_BN,seasonalResultsBN$mae) 
#   RMSE_BN=c(RMSE_BN,seasonalResultsBN$rmse) 
#   
# }
# 
# RMSE_SARIMA<-c(9.122256,11.148063,9.238145,5.226218)
# 
# MAE_SARIMA<-c(5.951492,7.935285,5.052711,3.042565	)
# 
# 
# PlotDataMAE=data.frame(Season=c("Winter","Spring","Summer","Fall"),`Linear Regression`=MAE_LR,`Decision Tree`=MAE_DT,`Stochastic Gradient Boosting`=MAE_SGB,`Random Forest`=MAE_RF,`Bayesian Network`=MAE_BN,`Seasonal ARIMA`=MAE_SARIMA)
# PlotDataRMSE=data.frame(Season=c("Winter","Spring","Summer","Fall"),`Linear Regression`=RMSE_LR,`Decision Tree`=RMSE_DT,`Stochastic Gradient Boosting`=RMSE_SGB,`Random Forest`=RMSE_RF,`Bayesian Network`=RMSE_BN,`Seasonal ARIMA`=RMSE_SARIMA)
# 
# BoxPlotDataMAE<-PlotDataMAE%>%pivot_longer(.,cols=-c(Season),names_to="Model Type",values_to="MAE")
# BoxPlotDataRMSE<-PlotDataRMSE%>%pivot_longer(.,cols=-c(Season),names_to="Model Type",values_to="RMSE")
# 
# BoxPlotData=merge(BoxPlotDataMAE,BoxPlotDataRMSE,by=c("Season","Model Type"))
# BoxPlotData=BoxPlotData%>%pivot_longer(.,cols=c(MAE,RMSE),names_to="Metric Type",values_to="Performance Value")
## BOX PLOTS

SeasonalBoxPlotRMSE=SeasonalPerformance%>%select(-c(MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA))%>%
  pivot_longer(.,cols=-c(Date,season),names_to="RMSE Model",values_to="RMSE Value")

SeasonalBoxPlotMAE=SeasonalPerformance%>%select(Date,season,MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA)%>%
  pivot_longer(.,cols=-c(Date,season),names_to="MAE Model",values_to="MAE Value")

str(SeasonalBoxPlotRMSE)

ggplot(SeasonalBoxPlotRMSE, aes(x=season, y=`RMSE Value`,fill=as.factor(`RMSE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Seasonal Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")
  
ggplot(SeasonalBoxPlotMAE, aes(x=season, y=`MAE Value`,fill=as.factor(`MAE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Seasonal Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")

# ggplot(BoxPlotData)+
#   geom_col(aes(x=Season, y=`Performance Value`, fill=`Metric Type`),position="dodge")+
#   facet_wrap(~`Model Type`)+
#   theme(legend.position = "none")+
#   theme_ipsum()+
#   ggtitle("Comparison of the Models",
#           subtitle = "Seasonal Analysis")
# 

############### Daily 


Forecast=Forecast%>%mutate(day=weekdays(as.Date(Date)))
str(Forecast)
Forecast$day=as.factor(Forecast$day)


WeekdayPerformance=Forecast%>%group_by(day,Date)%>%
  summarise(RMSE_LR=(error_test(MCP,PredictedMCP_LR))$rmse,
            MAE_LR=(error_test(MCP, PredictedMCP_LR))$mae,
            RMSE_DT=(error_test(MCP, PredictedMCP_DT))$rmse,
            MAE_DT=(error_test(MCP, PredictedMCP_DT))$mae,
            RMSE_RF=(error_test(MCP, PredictedMCP_RF))$rmse,
            MAE_RF=(error_test(MCP, PredictedMCP_RF))$mae,
            RMSE_SGB=(error_test(MCP, PredictedMCP_SGB))$rmse,
            MAE_SGB=(error_test(MCP, PredictedMCP_SGB))$mae,
            RMSE_BN=(error_test(MCP, PredictedMCP_BN))$rmse,
            MAE_BN=(error_test(MCP,PredictedMCP_BN))$mae,
            RMSE_ARIMA=(error_test(MCP, PredictedMCP_ARIMA))$rmse,
            MAE_ARIMA=(error_test(MCP,PredictedMCP_ARIMA))$mae)

WeekdayPerformance=as.data.frame(WeekdayPerformance)
WeekdayPerformance$day=as.factor(WeekdayPerformance$day)

WeekdayBoxPlotRMSE=WeekdayPerformance%>%select(-c(MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA))%>%
  pivot_longer(.,cols=-c(Date,day),names_to="RMSE Model",values_to="RMSE Value")

WeekdayBoxPlotMAE=WeekdayPerformance%>%select(Date,day,MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA)%>%
  pivot_longer(.,cols=-c(Date,day),names_to="MAE Model",values_to="MAE Value")

str(SeasonalBoxPlotRMSE)

ggplot(WeekdayBoxPlotRMSE, aes(x=factor(day, level = c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar")), 
                               y=`RMSE Value`,fill=as.factor(`RMSE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Weekday Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")

ggplot(WeekdayBoxPlotMAE, aes(x=factor(day, level = c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar")),
                              y=`MAE Value`,fill=as.factor(`MAE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Weekday Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")





# 
# i="Pazartesi"
# for(i in c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar")){
# 
#   dailyForecast=Forecast%>%filter(day==i)
#   
#   dailyResults=error_test(dailyForecast$MCP,dailyForecast$PredictedMCP_LR)
#   MAE_LR=c(MAE_LR,dailyResults$mae) 
#   RMSE_LR=c(RMSE_LR,dailyResults$rmse) 
#   
#   
#   dailyResultsDT=error_test(dailyForecast$MCP,dailyForecast$PredictedMCP_DT)
#   MAE_DT=c(MAE_DT,dailyResultsDT$mae) 
#   RMSE_DT=c(RMSE_DT,dailyResultsDT$rmse) 
#   
#   dailyResultsSGB=error_test(dailyForecast$MCP,dailyForecast$PredictedMCP_SGB)
#   MAE_SGB=c(MAE_SGB,dailyResultsSGB$mae) 
#   RMSE_SGB=c(RMSE_SGB,dailyResultsSGB$rmse) 
#   
#   dailyResultsRF=error_test(dailyForecast$MCP,dailyForecast$PredictedMCP_RF)
#   MAE_RF=c(MAE_RF,dailyResultsRF$mae) 
#   RMSE_RF=c(RMSE_RF,dailyResultsRF$rmse) 
#   
#   dailyResultsBN=error_test(dailyForecast$MCP,dailyForecast$PredictedMCP_BN)
#   MAE_BN=c(MAE_BN,dailyResultsBN$mae) 
#   RMSE_BN=c(RMSE_BN,dailyResultsBN$rmse) 
#   
# }
# 
# 
# 
# MAE_SARIMA<-c(6.292719, 5.440183, 4.831688, 4.768204, 4.184657, 4.698125, 8.283714)
# RMSE_SARIMA<-c( 9.639277,  8.810122,  8.146877,  7.920222 , 6.999907,  7.469568 ,12.520976)
# 
# 
# PlotDataMAE=data.frame(Weekday=c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar"),`Linear Regression`=MAE_LR,`Decision Tree`=MAE_DT,`Stochastic Gradient Boosting`=MAE_SGB,`Random Forest`=MAE_RF,`Bayesian Network`=MAE_BN,`Seasonal ARIMA`=MAE_SARIMA)
# PlotDataRMSE=data.frame(Weekday=c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar"),`Linear Regression`=RMSE_LR,`Decision Tree`=RMSE_DT,`Stochastic Gradient Boosting`=RMSE_SGB,`Random Forest`=RMSE_RF,`Bayesian Network`=RMSE_BN,`Seasonal ARIMA`=RMSE_SARIMA)
# 
# BoxPlotDataMAE<-PlotDataMAE%>%pivot_longer(.,cols=-c(Weekday),names_to="Model Type",values_to="MAE")
# BoxPlotDataRMSE<-PlotDataRMSE%>%pivot_longer(.,cols=-c(Weekday),names_to="Model Type",values_to="RMSE")
# 
# BoxPlotData=merge(BoxPlotDataMAE,BoxPlotDataRMSE,by=c("Weekday","Model Type"))
# BoxPlotData=BoxPlotData%>%pivot_longer(.,cols=c(MAE,RMSE),names_to="Metric Type",values_to="Performance Value")
# 
# BoxPlotData=BoxPlotData %>%
#   mutate(category =  factor(Weekday, levels = c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar"))) %>%
#   arrange(category)    
# 
# write.csv(BoxPlotData,"BoxPlotData.csv")
# str(BoxPlotData)
# ## BOX PLOTS
# 
# 
# ggplot(BoxPlotData, aes(x=`Model Type`, y=`Performance Value`,fill=`Metric Type`)) + 
#   geom_boxplot() +
#   theme()+
#   theme_ipsum()+
#   ggtitle("Comparison of the Models",
#           subtitle = "Weekday Analysis")
# 
# 
# ggplot(BoxPlotData)+
#   geom_col(aes(x=factor(Weekday, level = c("Pazartesi","Salý","Çarþamba","Perþembe","Cuma","Cumartesi","Pazar")), y=`Performance Value`, fill=`Metric Type`),position="dodge")+
#   facet_wrap(~`Model Type`)+
#   theme(legend.position = "none")+
#   theme_ipsum()+
#   ggtitle("Comparison of the Models",
#           subtitle = "Weekday Analysis")+
#   theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
# 
# write.csv(Forecast, "Forecast.csv")
# #Forecast$PredictedMCP_BN
# ggplot()+
#   geom_line(data=BNDataset%>%filter(MCP<200), aes(x=Date, y=MCP),color="green")+
#   geom_line(data=Forecast, aes(x=Date,y=PredictedMCP_BN),color="red")
# 
# ggplot()+
#   geom_line(data=BNDataset%>%filter(MCP<200, Date<"2019-02-01"), aes(x=Date, y=MCP),color="green")+
#   geom_line(data=Forecast%>%filter(Date<"2019-02-01"), aes(x=Date,y=PredictedMCP_BN),color="red")

##### MONTHLY ANALYSIS



Forecast=Forecast%>%mutate(month=month(as.Date(Date)))
str(Forecast)
Forecast$month=as.factor(Forecast$month)


MonthlyPerformance=Forecast%>%group_by(month,Date)%>%
  summarise(RMSE_LR=(error_test(MCP,PredictedMCP_LR))$rmse,
            MAE_LR=(error_test(MCP, PredictedMCP_LR))$mae,
            RMSE_DT=(error_test(MCP, PredictedMCP_DT))$rmse,
            MAE_DT=(error_test(MCP, PredictedMCP_DT))$mae,
            RMSE_RF=(error_test(MCP, PredictedMCP_RF))$rmse,
            MAE_RF=(error_test(MCP, PredictedMCP_RF))$mae,
            RMSE_SGB=(error_test(MCP, PredictedMCP_SGB))$rmse,
            MAE_SGB=(error_test(MCP, PredictedMCP_SGB))$mae,
            RMSE_BN=(error_test(MCP, PredictedMCP_BN))$rmse,
            MAE_BN=(error_test(MCP,PredictedMCP_BN))$mae,
            RMSE_ARIMA=(error_test(MCP, PredictedMCP_ARIMA))$rmse,
            MAE_ARIMA=(error_test(MCP,PredictedMCP_ARIMA))$mae)

MonthlyPerformance=as.data.frame(MonthlyPerformance)
MonthlyPerformance$month=as.factor(MonthlyPerformance$month)

MonthlyBoxPlotRMSE=MonthlyPerformance%>%select(-c(MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA))%>%
  pivot_longer(.,cols=-c(Date,month),names_to="RMSE Model",values_to="RMSE Value")

MonthlyBoxPlotMAE=MonthlyPerformance%>%select(Date,month,MAE_LR,MAE_DT,MAE_RF,MAE_SGB,MAE_BN,MAE_ARIMA)%>%
  pivot_longer(.,cols=-c(Date,month),names_to="MAE Model",values_to="MAE Value")

str(MonthlyBoxPlotMAE)

ggplot(MonthlyBoxPlotRMSE, aes(x=month,
                               y=`RMSE Value`,fill=as.factor(`RMSE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Monthly Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")

ggplot(MonthlyBoxPlotMAE, aes(x=month,
                              y=`MAE Value`,fill=as.factor(`MAE Model`))) + 
  geom_boxplot() +
  theme_ipsum()+
  ggtitle("Comparison of the Models",
          subtitle = "Monthly Analysis")+
  scale_fill_brewer(name = "Models", labels = c("Bayesian Network",
                                                "Decision Tree",
                                                "Linear Regression",
                                                "Random Forest",
                                                "Stochastic Gradient Boosting",
                                                "Seasonal ARIMA"),
                    palette="BuPu")



