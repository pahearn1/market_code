library(quantmod)
symbols=c("EWW")
getSymbols(symbols,from="2015-10-31",to="2017-10-31")
startT="2015-10-31"
endT="2017-10-31"
rangeT=paste(startT,"::",endT,sep="")
tEWW=EWW[,6][rangeT]
tEWW_log=log(tEWW)
tEWW_log_ret=diff(tEWW_log)
plot(tEWW_log_ret)
hist(tEWW_log_ret,breaks=100,col='orange')
plot(tEWW_log)
hist(tEWW_log,breaks=100,col='orange')

sd_EWWret=sd(tEWW_log_ret[403:503]) #last 100days vol
sd_EWWret*252^.5
########################
symbols=c("EEM")
getSymbols(symbols,from="2015-11-01",to="2017-11-01")
startT="2015-11-01"
endT="2017-11-01"
rangeT=paste(startT,"::",endT,sep="")
tEEM=EEM[,6][rangeT]
tEEM_log=log(tEEM)
tEEM_log_ret=diff(tEEM_log)
plot(tEEM_log_ret)
hist(tEEM_log_ret,breaks=100,col='orange')
plot(tEEM_log)
hist(tEEM_log,breaks=100,col='orange')

sd_EEMret=sd(tEEM_log_ret[403:503]) #last 100days vol
sd_EEMret*252^.5
########################
symbols=c("FXI")
getSymbols(symbols,from="2015-11-03",to="2017-11-03")
startT="2015-11-03"
endT="2017-11-03"
rangeT=paste(startT,"::",endT,sep="")
tFXI=FXI[,6][rangeT]
tFXI_log=log(tFXI)
tFXI_log_ret=diff(tFXI_log)
plot(tFXI_log_ret)
hist(tFXI_log_ret,breaks=100,col='orange')
#plot(tFXI_log)
#hist(tFXI_log,breaks=100,col='orange')
hist(tFXI_log_ret[2:503],breaks=25,col='orange')
sd_FXIret=sd(tFXI_log_ret[2:503]) #last 100days vol
sd_FXIret*252^.5

########################
symbols=c("HYG")
getSymbols(symbols,from="2015-11-06",to="2017-11-06")
startT="2015-11-06"
endT="2017-11-06"
rangeT=paste(startT,"::",endT,sep="")
tHYG=HYG[,6][rangeT]
tHYG_log=log(tHYG)
tHYG_log_ret=diff(tHYG_log)
plot(tHYG_log_ret*252^.5)
hist(tHYG_log_ret,breaks=100,col='orange')
#plot(tHYG_log)
#hist(tHYG_log,breaks=100,col='orange')
hist(tHYG_log_ret[2:503]*252^.5,breaks=25,col='orange')
sd_HYGret=sd(tHYG_log_ret[2:503]) #last 100days vol
sd_HYGret*252^.5
mean(tHYG_log_ret[2:503])*252^.5
