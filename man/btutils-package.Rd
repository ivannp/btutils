\name{btutils-package}
\alias{btutils-package}
\alias{btutils}
\docType{package}
\title{
Utilities to Facilitate Back-Testing of Trading Strategies
}
\description{
A collection of utilities to help back testing of trading
strategies. Mostly written in C++, thus, speed is not an issue.
}
\details{
\tabular{ll}{
Package: \tab btutils\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2013-05-07\cr
}
Best place to start:\cr \code{\link{simple.trade.indicator}}\cr
  \code{\link{processTrades}}\cr
}
\author{
Ivan Popivanov

Maintainer: Ivan Popivanov <ivan *dot* popivanov *at* gmail *dot* com>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{package}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
require(quantmod)
require(btutils)

# Download some data from Yahoo
spy = getSymbols("SPY", auto.assign=FALSE)

# Compute the indicator for a 200 day moving average
spy.macd = MACD(Cl(spy), nFast=1, nSlow=200)[,1]
spy.indicator = ifelse(spy.macd < 0, 0, 1)

spy.trades = trade.indicator(OHLC(spy), spy.indicator, stop.loss=0.02)
spy.rets = ROC(Cl(spy), n=1, type="discrete")
ec = calculate.returns(spy.rets, spy.trades)
}