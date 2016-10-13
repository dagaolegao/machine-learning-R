Data<- read.csv("table.csv") #GE's stock from 2015-5-1 to 2016-5-1
head(Data,30)
colnames(Data)<- c("time","open","high","low","end","volume")
Close<- Data[,5]
#  MACD's function
macd<-function(Stock,short_period,
               long_period,dea_period) {
  #  Stock is end price 
  #  short_period is short-term
  #  long_periodd is long-term
  #  dea_period
  length_stock<-length(Stock)
  EMA_short<-rep(0,length_stock)
  EMA_long<-rep(0,length_stock)
  stock_diff<-rep(0,length_stock)
  stock_dea<-rep(0,length_stock)
  stock_macd<-rep(0,length_stock)
  #  initialize he primeter
  EMA_short[1]<-Stock[1]
  EMA_long[1]<-Stock[1]
  stock_diff[1]<-0
  stock_dea[1]<-0
  stock_macd[1]<-0
  #  calculate the MACD
  for (t in 2:length_stock) {
    EMA_short[t]<-Stock[t]*2/(short_period+1)+
      EMA_short[t-1]*(short_period-1)/(short_period+1)
    EMA_long[t]<-Stock[t]*2/(long_period+1)+
      EMA_long[t-1]*(long_period-1)/(long_period+1)
    stock_diff[t]<-EMA_short[t]-EMA_long[t]
    stock_dea[t]<-stock_diff[t]*2/(dea_period+1)+
      stock_dea[t-1]*(dea_period-1)/(dea_period+1)
    stock_macd[t]<-2*(stock_diff[t]-stock_dea[t])
  }
  return(stock_macd)
}

#  The explantory is the Macd for last one day and two day  
#  trasfer the prince today as 1 or 0 to represent the rise or down of the stock 
short_period<-5
long_period<-25
dea_period<-9
Macd<-macd(Close,short_period,long_period,dea_period)
Diff<-diff(Close)
Diff<-ifelse(Diff>0,1,0)
#  up or down at T (the response variable)
Diff.T<-Diff
Diff.T<-c(NA,Diff.T)
#  up or down at T-1(the explantory variable)
Diff.T1<-Diff[-length(Diff)]
Diff.T1<-c(NA,NA,Diff.T1)
#  up or down at T-2(the explantory variable)
Diff.T2<-Diff[-c((length(Diff)-1),length(Diff))]
Diff.T2<-c(NA,NA,NA,Diff.T2)
#  Macd at T-1(the explantory variable)
Macd.T1<-Macd[-length(Macd)]
Macd.T1<-c(NA,Macd.T1)
#  Macd at T-2(the explantory variable)
Macd.T2<-Macd[-c((length(Macd)-1),length(Macd))]
Macd.T2<-c(NA,NA,Macd.T2)
#  dataset
N<-length(Close)
Data.train<-data.frame(
  y=as.factor(Diff.T[c((N-250):(N-100))]),
  x1=Diff.T1[c((N-250):(N-100))],
  x2=Diff.T2[c((N-250):(N-100))],
  x3=Macd.T1[c((N-250):(N-100))],
  x4=Macd.T2[c((N-250):(N-100))])
Data.test<-data.frame(
  x1=Diff.T1[c((N-99):(N))],
  x2=Diff.T2[c((N-99):(N))],
  x3=Macd.T1[c((N-99):(N))],
  x4=Macd.T2[c((N-99):(N))])

# use SVM for modeling
Data.train$y<- as.numeric(Data.train$y)
library(e1071)
DataSvmModel<- svm(y~x1+x2+x3+x4, data=Data.train)
summary(DataSvmModel)
plot(DataSvmModel,Data.train)
# predict
Predict<-predict(DataSvmModel,Data.test,
                 probability=TRUE)
#  up or down for prediction
Predict<-as.numeric(as.vector(Predict))
#  build the transaction and backtesting  
cost<-0.001 #assume the trasaction cost
Length<-length(Predict)
Trading.Return<-rep(0,Length)
Close.test<-Close[c((N-99):N)]
for (i in 1:Length) {
  if (Predict[i]==1) {
    #  up 
    Trading.Return[i]<-
      Close.test[i+1]*(1-cost)-Close.test[i]*(1+cost)
  } else {
    #  down
    Trading.Return[i]<-
      Close.test[i]*(1-cost)-Close.test[i+1]*(1+cost)
  }
  
}
#  cummulative sum of return
Trading.Return<-cumsum(Trading.Return)
Trading.Return<- Trading.Return[-100]
#  function to calculate the highest retreat ratio.
Retreat_Ratio<-function(stock_return1) {
  N<-length(stock_return1)
  RetraceRatio<-rep(0,N)
  for (i in 2:N) {
    C<-max(stock_return1[1:i])
    if (C==stock_return1[i]) {
      RetraceRatio[i]<-0
    } else {
      RetraceRatio[i]<-(stock_return1[i]-C)/C
    }
  }
  return(RetraceRatio)
}
retreat_ratio<-Retreat_Ratio(Trading.Return)
plot(Trading.Return)
Length<- length(retreat_ratio)
#retreat ratio plot

c1<-c(c(0:Length),c(Length:0))
c2<-c(c(0,retreat_ratio),rep(0,Length+1))
plot(c1,c2,type='n',xlab='Time',
     ylab='Retreat Ratio')
polygon(c1,c2,col='orange',border='yellow')
title('Retreat Ratio')

#  #######################################################
#  #######################################################
