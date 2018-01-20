###############################################################
runs <- 10000
premium = 1840
current_px = 49.40
days = 62
#UPDATE MRKT PX, VOLS AND DTM
generate.path <- function(){
  #days <- 76
  sigma = .15/252^.5
  changes <- rnorm(days,mean=1-.0001438,sd=max(rnorm(days, mean = sigma, sd = sigma/2), 0.01))
  sample.path <- cumprod(c(current_px,changes))
  #closing.price <- sample.path[days+1] #+1 because we add the opening price
  #return(closing.price)
}

all_paths = replicate(runs,generate.path())

#Payoff
call_sold_shares = 2000
call_purchased_shares = 2000
put_sold_shares = 0
put_purchased_shares = 0

call_sold_strike = 53
call_purchased_strike = 56
put_sold_strike = 50
put_purchased_strike = 47

call_sold_payoff = ifelse(all_paths > call_sold_strike, (call_sold_strike - all_paths)*call_sold_shares,0)
call_purchased_payoff = ifelse(all_paths > call_purchased_strike, (all_paths - call_purchased_strike)*call_purchased_shares,0)

put_sold_payoff = ifelse(all_paths < put_sold_strike, (all_paths - put_sold_strike)*put_sold_shares,0)
put_purchased_payoff = ifelse(all_paths < put_purchased_strike, (put_purchased_strike - all_paths)*put_purchased_shares,0)

total_payoff = premium + call_purchased_payoff + call_sold_payoff + put_purchased_payoff + put_sold_payoff
hist(total_payoff[days+1,],breaks = 100)
mean(total_payoff[days+1,])
summary(total_payoff[days+1,])

#matplot(all_paths,type = "l")
#closing_price = all_paths[75,] #(need code for "last" column" AGREE PATHS WITH "DAYS"
#hist(closing_price,breaks = 100,col = "green")
#summary(closing_price)

##################################################
# FXI
runs <- 10000
premium = -3400
current_px = 46.28
days = 224
#UPDATE MRKT PX, VOLS AND DTM
generate.path <- function(){
  #days <- 224
  sigma = .1567/252^.5
  changes <- rnorm(days,mean=1+.0004713,sd=max(rnorm(days, mean = sigma, sd = sigma/2), 0.01))
  sample.path <- cumprod(c(current_px,changes))
  #closing.price <- sample.path[days+1] #+1 because we add the opening price
  #return(closing.price)
}

all_paths = replicate(runs,generate.path())

#Payoff
call_sold_shares = 0
call_purchased_shares = 0
put_sold_shares = 3000
put_purchased_shares = 3000

call_sold_strike = 0
call_purchased_strike = 0
put_sold_strike = 35
put_purchased_strike = 42

call_sold_payoff = ifelse(all_paths > call_sold_strike, (call_sold_strike - all_paths)*call_sold_shares,0)
call_purchased_payoff = ifelse(all_paths > call_purchased_strike, (all_paths - call_purchased_strike)*call_purchased_shares,0)

put_sold_payoff = ifelse(all_paths < put_sold_strike, (all_paths - put_sold_strike)*put_sold_shares,0)
put_purchased_payoff = ifelse(all_paths < put_purchased_strike, (put_purchased_strike - all_paths)*put_purchased_shares,0)

total_payoff = premium + call_purchased_payoff + call_sold_payoff + put_purchased_payoff + put_sold_payoff
hist(total_payoff[days+1,],breaks = 100)
#boxplot(total_payoff[days+1,])
#matplot(all_paths,type = "l")
mean(total_payoff[days+1,])
summary(total_payoff[days+1,])
###################################################################
# EEM
runs <- 10000
premium = 4540
current_px = 45.88
days = 212
#UPDATE MRKT PX, VOLS AND DTM
generate.path <- function(){
  #days <- 224
  sigma = .118/252^.5
  changes <- rnorm(days,mean=1+.0005845331,sd=max(rnorm(days, mean = sigma, sd = sigma/2), 0.01))
  sample.path <- cumprod(c(current_px,changes))
  #closing.price <- sample.path[days+1] #+1 because we add the opening price
  #return(closing.price)
}

all_paths = replicate(runs,generate.path())

#Payoff
call_sold_shares = 2000
call_purchased_shares = 2000
put_sold_shares = 2000
put_purchased_shares = 0

call_sold_strike = 46
call_purchased_strike = 49
put_sold_strike = 40
put_purchased_strike = 0

call_sold_payoff = ifelse(all_paths > call_sold_strike, (call_sold_strike - all_paths)*call_sold_shares,0)
call_purchased_payoff = ifelse(all_paths > call_purchased_strike, (all_paths - call_purchased_strike)*call_purchased_shares,0)

put_sold_payoff = ifelse(all_paths < put_sold_strike, (all_paths - put_sold_strike)*put_sold_shares,0)
put_purchased_payoff = ifelse(all_paths < put_purchased_strike, (put_purchased_strike - all_paths)*put_purchased_shares,0)

total_payoff = premium + call_purchased_payoff + call_sold_payoff + put_purchased_payoff + put_sold_payoff
hist(total_payoff[days+1,],breaks = 100)
#boxplot(total_payoff[days+1,])
#matplot(all_paths,type = "l")
summary(total_payoff[days+1,])
##################################################
# HYG
runs <- 10000
premium = -3400
current_px = 86.68
days = 212
#UPDATE MRKT PX, VOLS AND DTM
generate.path <- function(){
  #days <- 224
  sigma = .08/252^.5
  changes <- rnorm(days,mean=1+.0000,sd=max(rnorm(days, mean = sigma, sd = sigma/2), 0.01))
  sample.path <- cumprod(c(current_px,changes))
  #closing.price <- sample.path[days+1] #+1 because we add the opening price
  #return(closing.price)
}

all_paths = replicate(runs,generate.path())


#Payoff
call_sold_shares = 0
call_purchased_shares = 0
put_sold_shares = 0
put_purchased_shares = 2000

call_sold_strike = 0
call_purchased_strike = 0
put_sold_strike = 0
put_purchased_strike = 85

call_sold_payoff = ifelse(all_paths > call_sold_strike, (call_sold_strike - all_paths)*call_sold_shares,0)
call_purchased_payoff = ifelse(all_paths > call_purchased_strike, (all_paths - call_purchased_strike)*call_purchased_shares,0)

put_sold_payoff = ifelse(all_paths < put_sold_strike, (all_paths - put_sold_strike)*put_sold_shares,0)
put_purchased_payoff = ifelse(all_paths < put_purchased_strike, (put_purchased_strike - all_paths)*put_purchased_shares,0)

total_payoff = premium + call_purchased_payoff + call_sold_payoff + put_purchased_payoff + put_sold_payoff
hist(total_payoff[days+1,],breaks = 100)
#boxplot(total_payoff[days+1,])
#matplot(all_paths,type = "l")
summary(total_payoff[days+1,])