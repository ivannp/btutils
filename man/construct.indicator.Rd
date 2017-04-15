\name{construct.indicator}
\alias{construct.indicator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Constructs a Trading Indicator Combining Entries and Exits
}
\description{
Constructs an indicator (\code{1} for long, \code{-1} for short, \code{0} otherwise). The
entries and the exits are provided as boolean vectors, separetely for long and short positions.
}
\usage{
construct.indicator(long.entries, long.exits, short.entries, short.exits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{long.entries}{
A boolean vector indicating (TRUE) the long entries
}
  \item{long.exits}{
A boolean vector indicating (TRUE) the long exits
}
  \item{short.entries}{
A boolean vector indicating (TRUE) the short entries
}
  \item{short.exits}{
A boolean vector indicating (TRUE) the short exits
}
}
\details{
Designed to maintain a single position only.

Implemented in C++ - fast.
}
\value{
An object of the same class as \code{long.entries} containing a single column. The positions are
encoded as \code{1}, \code{-1} and \code{0} for long, short and none, respectively.
}
\author{
Ivan Popivanov
}
\examples{
## Long entries when RSI2 > 90, long exits when RSI2 < 30
## Short entries when RSI2 < 10, short exits when RSI > 70

require(quantmod)

spy = getSymbols("SPY", auto.assign=FALSE)
rsi = RSI(Cl(spy), n=2)
lentries = !is.na(rsi) & rsi < 10
lexits = !is.na(rsi) & rsi > 70
sentries = !is.na(rsi) & rsi > 90
sexits = !is.na(rsi) & rsi < 30
indicator = construct.indicator(lentries, lexits, sentries, sexits)
}
