library(DMwR2)
library(quantmod)

gspc<-getSymbols("^GSPC",from="1970-01-02",to="2018-04-20",auto.assign = F)
#function to sun large absolute returns, above target margin i.e 25%
T.ind<-function(quotes,tgt.margin=0.025,n.days=10){
  #returning vector or array or list by applying a function to margins of array or matrix
  v<-apply(HLC(quotes),1,mean)
  #extracting the close quotes
  v[1]<-Cl(quotes)[1]
  r<-matrix(NA,ncol = n.days,nrow = NROW(quotes))
  #advancing stock series i.e HLC over n.days period
  for(x in 1:n.days){
    #calculating k-period percentage diff within one series
    perdiff<-(Delt(v,k=x))
    #new series by advancing forward n.days period
    r[,x]=Next(perdiff,x)
  }
  x<-apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}
#creating financial charts for the last three months
#bar represent high low prices and box the close open amplitude, orange-decline, green-rise
candleChart(last(gspc,"3 months"),theme = "white",TA=NULL)

avg.price<-function(p) {
  apply(HLC(p),1,mean)
}
#addressing new plotting function for average price and for returns
addAvg.price<-newTA(FUN = avg.price,col=1,legend= "AVG PRICE")
addT.ind<-newTA(FUN = T.ind,color="red",legend="tgtRET")
addAvg.price(on=1)
addT.ind()


library(TTR)
#feature selection phase-feature wrapping
my.ATR <- function(x) ATR(HLC(x))[,'atr']
my.smi<-function(x)  SMI(HLC(x))[,"SMI"]
my.adx <- function(x) ADX(HLC(x))[,'ADX']
my.Aroon<-function(x)  aroon(cbind(Hi(x),Lo(x)))$oscillator
my.BB <- function(x) BBands(HLC(x))[, "pctB"]
my.chaikinVol<-function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[,1]
my.clv <- function(x) EMA(CLV(HLC(x)))[, 1]
my.EMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
my.MACD <-function(x) MACD(Cl(x))[, 2]  
my.MFI <-function(x) MFI(HLC(x),Vo(x))
my.SAR <-function(x) SAR(cbind(Hi(x),Cl(x)))[, 1]
my.VOLT <-function(x) volatility(OHLC(x),calc = "garman")[, 1]


library(randomForest)
#non fitted model
model<-specifyModel(T.ind(gspc)~Delt(Cl(gspc),k=1:10)+my.ATR(gspc)+my.Aroon(gspc)+my.adx(gspc)+CMO(Cl(gspc))+my.BB(gspc)+EMA(Delt(Cl(gspc)))+my.chaikinVol(gspc)+my.clv(gspc)+my.EMV(gspc)+my.MACD(gspc)+my.MFI(gspc)+my.SAR(gspc)+my.smi(gspc)+my.VOLT(gspc)+runSD(Cl(gspc))+runMean(Cl(gspc)))

set.seed(1234)

rf<-buildModel(model,method = "randomForest",training.per = c("1995-01-01","2005-12-03"),ntree=1000,importance=T)                    
#variance importance plot
varImpPlot(rf@fitted.model,type = 1)
imp<-importance(rf@fitted.model)
row.names(imp)[which(imp>30)]

model<-specifyModel(T.ind(gspc)~Delt(Cl(gspc),k=1:10)+my.ATR(gspc)+my.Aroon(gspc)+my.adx(gspc)+my.EMV(gspc)+my.MACD(gspc)+my.MFI(gspc)+my.SAR(gspc)+my.smi(gspc)+my.VOLT(gspc)+runSD(Cl(gspc))+runMean(Cl(gspc)))

#Regression
Tdata.train <- as.data.frame(modelData(model,data.window=c('1970-01-02','2005-12-30')))
Tdata.eval <- na.omit(as.data.frame(modelData(model,data.window=c('2006-01-01','2018-04-19'))))
T.form <-as.formula('T.ind.gspc ~ .')

#classification
buy.thres<- 0.1
sell.thres<- -0.1
Tdata.trainclass<-cbind(sig=trading.signals(Tdata.train["T.ind.gspc"],buy.thres,sell.thres),Tdata.train[,-1])
Tdata.evalclass<-cbind(sig=trading.signals(Tdata.eval["T.ind.gspc"],buy.thres,sell.thres),Tdata.eval[,-1])
Tformclass<-as.formula("sig ~ .")

#building the neural net 1
set.seed(1234)
library(nnet)
#preparing a data frame where list of Tdata.train is passed through T.ind function
#the last column of Tdata.train is negatively scaled
norm.data <- data.frame(T.ind.gspc=Tdata.train[[1]],scale(Tdata.train[,-1]))
#fitting the neural network (back-propagation algo)
nn <- nnet(T.form, norm.data[1:1000, ], size = 10, decay = 0.01,maxit = 1000, linout = TRUE, trace = FALSE)
#data are fed to the  nueral net and predictions are done
preds <- predict(nn, norm.data[1001:2000,])
#predicted numeric value is been chaged to trading signals (3 type:s[sell],b[buy],h[hold])
signs.nn<-trading.signals(preds,0.1,-0.1)
#The training data converted to signals s,h,b on the basis if thresold value
true.signs.nn<-trading.signals(Tdata.train[1001:2000,"T.ind.gspc"],0.1,-0.1)
#calculate the values of precisionand recall of a set of predicted signals
#if we feed the jth input Xj to nn then whether Yj is obtained without any noise
#the preds and trues should be of same length
sigs.PR(signs.nn,true.signs.nn)
"""
precision    recall
s   0.1863905 0.3579545
b   0.1691649 0.4906832
s+b 0.1763975 0.4213650
"""
#the precision and recall scores are a bit low means on feeding xj input pure yj output will not be recalled 

#building the neural net2 on the basis of classification
set.seed(1234)
#clasiifier into s ,h b , taking the sig column into concern which is mainly the classes
norm.data <-data.frame(Signal=Tdata.trainclass$sig,scale(Tdata.train[,-1]))
#feeding the data to the neural net
nn<-nnet(Signal~.,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
#doing the predictions according to class type
#doing a multiple output for each single test case
preds<-predict(nn,norm.data[1001:2000,],type = "class")
sigs.PR(preds,norm.data[1001:2000,1])
"""
    precision    recall
s   0.2500000 0.2215909
b   0.2200000 0.1366460
s+b 0.2617188 0.1988131
"""
#The precission and recall scores are still the same

#SVM(Support Vector Machine)
#doing the regression task using package e1071
set.seed(1234)
library(e1071)
#constructing a svm object
svmo<-svm(T.form,Tdata.train[1:1000,],gamma=0.001,cost=100)
#doing the prediction using svm object
svm.preds<-predict(svmo,Tdata.train[1001:2000,])
#with the help of trading signals() converting the predicted numerical value into signal class
sigs.svm<-trading.signals(svm.preds,0.1,-0.1)
#converting the Training set into class signals
true.sigs.svm<-trading.signals(Tdata.train[1001:2000,"T.ind.gspc"],0.1,-0.1)
#finding precision and recall
sigs.PR(sigs.svm,true.sigs.svm)
"""
precision     recall
s   0.5714286 0.06818182
b   0.3225806 0.06211180
s+b 0.4230769 0.06528190
"""
#Support Vector Machine
#doing the classification task using the package kernlab
library(kernlab)
#making a kernal support vector object and feeding with train set data, cost=10
ksvmo<-ksvm(sig~.,Tdata.trainclass[1:1000,],C=10)
#predicting 
ksvm.preds<-predict(ksvmo,Tdata.trainclass[1001:2000,])
#finding the precission and recall of signals of train set class and predictions
sigs.PR(ksvm.preds,Tdata.trainclass[1001:2000,1])
"""
  precision    recall
s   0.3023256 0.3693182
b   0.3069307 0.1925466
s+b 0.3037975 0.2848665
"""

#Multivariate Additive Regression Spline
#MARS model
library(earth)
#building a MARS model object
e <- earth(T.form,Tdata.train[1:1000,])
#running the model to do predictions
e.preds <- predict(e,Tdata.train[1001:2000,])
#converting the MARS numeric prediction data into signals
sigs.e <- trading.signals(e.preds,0.1,-0.1)
#building the true signals of the train set
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.gspc'],0.1,-0.1)
#finding the recall and precision
sigs.PR(sigs.e,true.sigs)
"""
    precision    recall
s   0.2222222 0.1931818
b   0.2462312 0.3043478
s+b 0.2357955 0.2462908

"""
#implementing trading strategies with the model, a long position when prices are going up and short postions when going down,
#long positions-buying at price p at time t and selling at time t+x for profit
#short positions-selling at price p now and in future price goes down so buying it

#BUILDING THE TRADING STIMULATOR
#writing a trading policy function where there is more than one positon for the stocks to open and no aging in closing

policy2<-function(signals,market,opened.pos,money,bet=0.2,exp.prof=0.025,max.loss=0.05)
{
  d<-NROW(market)
  orders<-NULL
  n0s<-NROW(opened.pos)
  #if there is no open of any stock and the signal is in hold then return null
  if(!n0s && signals[d]=='h')
    return(orders)
  #lets check if we can open new position
  #for long postion (buy at price p at time t and sell at time t+x to profit)
  if(signals[d]=='b')
  {
    #Cl(market) is extracting and transforming market column in OHLC time series
    quant<-round(bet*money/Cl(market)[d],0)
    if(quant>0)
    orders<-rbind(orders,data.frame(order=c(1,-1,-1),order.type=c(1,2,3),
      val=c(quant,Cl(market)[d]*(1+exp.prof),Cl(market)[d]*(1-max.loss)),
                action=c('open','close','close'),posID=c(NA,NA,NA)))
    }
  else if(signals[d]=='s')
    {
  need2buy<-sum(opened.pos[opened.pos[,'pos.type']==-1,
                        "N.Stocks"])*Cl(market)[d]
  quant<-round(bet*(money-need2buy)/Cl(market)[d],0)
  if(quant>0)
    orders<-rbind(orders,data.frame(order=c(-1,1,1),order.type=c(1,2,3),
    val=c(quant,Cl(market)[d]*(1+exp.prof),Cl(market)[d]*(1-max.loss)),
    action=c('open','close','close'),posID=c(NA,NA,NA)))
    }
    orders
}

#TRY AND TEST THE TRADING STIMULATOR
#train and test periods
start<-1
len.train<-1000
len.test<-500
train<-start:(start+len.train-1)
test<-(start+len.train):(start+len.train+len.test-1)
#obtaining quotes for the testing period
data("GSPC")
date<-rownames(Tdata.train[start+len.train,])
marketTP<-gspc[paste(date,'/',sep=' ')][1:len.test]
#learning the model and obtaining its signal predictions for test period
library(e1071)
s<-svm(T.form,Tdata.train[train,],cost=10,gamma = 0.01)
p<-predict(s,Tdata.train[test,])
tssig<-trading.signals(p,0.1,-0.1)
#now using the simulated trader for testing period
simulated_trader=trading.simulator(marketTP,signals = tssig,
policy.func = 'policy',policy.pars = list(exp.prof=0.05,bet=0.2))
simulated_trader

policy.1 <- function(signals,market,opened.pos,money,bet=0.2,hold.time=10,
                    exp.prof=0.025, max.loss= 0.05)
d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do
  if (!nOs && signals[d] == 'h') return(orders)
  #First lets check if we can open new positions
   #i) long positions
    if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/Cl(market)[d],0)
    if (quant > 0)
    orders <- rbind(orders,
    data.frame(order=c(1,-1,-1),order.type=c(1,2,3),
    val = c(quant,
    Cl(market)[d]*(1+exp.prof),
    Cl(market)[d]*(1-max.los)),
  action = c('open','close','close'),
    posID = c(NA,NA,NA)))
   # ii) short positions
    } else if (signals[d] == 's' && !nOs) {
   # this is the nr of stocks we already need to buy
  # because of currently opened short positions
  need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                             "N.stocks"])*Cl(market)[d]
    quant <- round(bet*(money-need2buy)/Cl(market)[d],0)
        if (quant > 0)
       orders <- rbind(orders,
        data.frame(order=c(-1,1,1),order.type=c(1,2,3),
        val = c(quant,
    Cl(market)[d]*(1-exp.prof),
      Cl(market)[d]*(1+max.loss)),
    action = c('open','close','close'),
          posID = c(NA,NA,NA)
            )
            )}
      # Now lets check if we need to close positions
      # because their holding time is over
      if (nOs)
      for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
      orders <- rbind(orders,
      data.frame(order=-opened.pos[i,'pos.type'],
      order.type=1,
      val = NA,
      action = 'close',
      posID = rownames(opened.pos)[i]
      ))}
      orders
      }

