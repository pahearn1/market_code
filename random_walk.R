
# Load the required packages.
library(PerformanceAnalytics)
library(xts)


#' @title Sample a random disturbance from either a normal distribution with a 
#' constant standard deviation (Geometric Brownian Motion model) or from a 
#' distribution with a stochastic standard deviation (Stochastic Volatility GBM)
#' 
#' @description Given a long run random disturbance mean, mu, and a standard 
#' deviation, sigma, sample a random disturbance from either a standard normal 
#' distribution with mean mu and standard deviation sigma, or sample a random 
#' disturbance from a normal distribution with mean mu and a stochastic standard 
#' deviation sampled from a normal distribution centered around sigma with a 
#' standard deviation equal to half of sigma. This is maxed out at 0.01 because
#' a negative standard deviation is incoherent. 
#' 
#' @details When the stochastic volatility option is enabled, the resulting 
#' distribution of means for the random disturbance has much fatter tails. This 
#' can be illustrated with the compareRandomDisturbances function.
#' 
#' @param mu double :: The expected value of the disturbance (should be zero).
#' @param sigma double :: The standard deviation of the disturbance.
#' 
randomDisturbance <- function(mu = 0.0, sigma = 0.45, 
                              stochastic.volatility = TRUE) {
  if (!stochastic.volatility) {
    return (rnorm(1, mean = mu, sd = sigma))
  } else {
    # sigma.stochastic <- rlnorm(1, meanlog = log(sigma), sdlog = sigma/2)
    sigma.stochastic <- max(rnorm(1, mean = sigma, sd = sigma/2), 0.01)
    return (rnorm(1, mean = mu, sd = sigma.stochastic))
  }
}


#' @title Step forward one unit of time in the log price process.
#' @description This step function is used to move from state t / t-1 in the log 
#' price process to t+1 / t. This involves adding a static drift component to the 
#' previous price and adding a random disturbance sampled from some distribution.
#' 
#' @param Xt1 double :: The current price in the log price process.
#' @param mu double :: The drift component of returns.
#' @param rd double :: A random disturbance in the force.
#' @param dt double :: This variable controls time. It is set to 1/252 by default
#' @return Xt2 double :: The next price in the log price process.
#' 
logPriceStep <- function(Xt1, mu, rd, dt = 0.003968254) {
  Xt2 <- Xt1 + (mu * dt) + (rd * sqrt(dt))
  return (Xt2)
}


#' @title Generate a log price process.
#' @description This method is used to generate a log price process which 
#' simulates the log price of an asset starting with the value X0. The process 
#' is controlled by a number of parameters including mu, the average daily 
#' return, rd.mu the average daily random disturbance, rd.sigma, the volatility 
#' of the random disturbance, and dt, time. The method works by iteratively 
#' sampling a new log price using the logPriceStep function.
#' 
#' @param t int :: The number of days worth of prices to simulate.
#' @param X0 int :: The starting price of the asset. Set to 1.0.
#' @param mu double :: The drift component of returns.
#' @param rd.mu double :: The average daily disturbance to be added.
#' @param rd.sigma double :: The volatility of daily disturbances to be added.
#' @param dt double :: Time. This variable is set to 1/252 by default.
#' @return X vector :: A simulated log price process.
#' 
logPriceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, 
                            rd.mu = 0.0, rd.sigma = 0.45, 
                            dt = 0.003968254, stochastic.volatility = TRUE) {
  X <- rep(X0, t)
  for (ti in 2:t) {
    rd <- randomDisturbance(rd.mu, rd.sigma, stochastic.volatility)
    X[ti] <- logPriceStep(X[ti - 1], mu, rd, dt)
  }
  
  return(X)
}


#' @title Generate a discrete price process.
#' @description This method exponentiates a log price process and returns it.
#' @inheritParams logPriceProcess
#' 
priceProcess <- function(t = 252, X0 = 1.0, mu = 0.1062, 
                         rd.mu = 0.0, rd.sigma = 0.45, 
                         dt = 0.003968254, stochastic.volatility = TRUE) {
  return(exp(logPriceProcess(t, log(X0), mu, 
                             rd.mu, rd.sigma, 
                             dt, stochastic.volatility)))
}


#' @title Generate an XTS object containing multiple discrete price processes.
#' @description This method constructs an xts object which contains multiple 
#' discrete price processes as simulated using the priceProcess function. These 
#' price processes are named S1, S2, ... , Sn.
#' 
#' @param n int :: The number of price processes to simulate.
#' @inheritParams logPriceProcess
#' 
priceProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, 
                           rd.mu = 0.0, rd.sigma = 0.45, 
                           dt = 0.003968254, stochastic.volatility = TRUE) {
  processes <- xtsProcess(priceProcess(t, X0, mu, 
                                       rd.mu, rd.sigma, 
                                       dt, stochastic.volatility), "S1")
  if (n > 1) for (i in 2:n)
    processes <- merge.xts(processes, 
                           xtsProcess(priceProcess(t, X0, mu, rd.mu, rd.sigma, 
                                                   dt, stochastic.volatility), 
                                      paste("S", i, sep = '')))
  return(processes)
}


#' @title Generate an XTS object containing daily returns for multiple discrete 
#' price processes.
#' 
#' @description This method constructs an xts object which contains multiple 
#' daily return processes as simulated using the priceProcess function. These 
#' price processes are named S1, S2, ... , Sn.
#' 
returnProcesses <- function(n, t = 252, X0 = 1.0, mu = 0.1062, 
                            rd.mu = 0.0, rd.sigma = 0.45, dt = 0.003968254, 
                            stochastic.volatility = TRUE, method = "discrete") {
  prices <- priceProcesses(n, t, X0, mu, rd.mu, rd.sigma, dt, stochastic.volatility)
  return(Return.calculate(prices, method = method))
}


#' @title Convert a time series into an xts time series starting from today and 
#' going forward into the future.
#' 
#' @description This method just converts a time series (a vector of prices or 
#' returns) into an xts object.
#' 
#' @param ts vector :: The vector containing the time series values.
#' @param ts.name character :: The name of the time series.
#' 
xtsProcess <- function(ts, ts.name) {
  dates <- seq.Date(Sys.Date(), Sys.Date() + (length(ts) - 1), 1)
  ts.xts <- xts(ts, order.by = dates, col.names = ts.name)
  colnames(ts.xts) <- ts.name
  return(ts.xts)
}