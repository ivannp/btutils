#  Copyright (c) 2013-2014, Ivan Popivanov
#  
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#  
#      Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#  
#      Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Constant declarations
EXIT_ON_LAST           =  0
STOP_LIMIT_ON_OPEN     =  1
STOP_LIMIT_ON_HIGH     =  2
STOP_LIMIT_ON_LOW      =  3
STOP_LIMIT_ON_CLOSE    =  4
STOP_TRAILING_ON_OPEN  =  5
STOP_TRAILING_ON_HIGH  =  6
STOP_TRAILING_ON_LOW   =  7
STOP_TRAILING_ON_CLOSE =  8
PROFIT_TARGET_ON_OPEN  =  9
PROFIT_TARGET_ON_HIGH  = 10
PROFIT_TARGET_ON_LOW   = 11
PROFIT_TARGET_ON_CLOSE = 12
MAX_DAYS_LIMIT         = 13

# entry - the trade's entry
# exit - the trade's exit
# pos - long (1) or short (-1)
# stop.loss - the stop loss, NA if none
# stop.trailing - a trailing stop, NA if none
# profit.target - a profit targe, NA if none
# max.days - the maximum number of days for this trade, 0 if none
#
# if both stop.loss and stop.trailing are specified, the stop.trailing is used
process.trade = function(
                     op,
                     hi,
                     lo,
                     cl,
                     entry,
                     exit,
                     pos,
                     stop.loss=NA,
                     stop.trailing=NA,
                     profit.target=NA,
                     max.days=0,
                     tick.size=0.01) {
   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   tmp = op[,1]
   
   ibeg = tmp[entry, which.i=T]
   iend = tmp[exit, which.i=T]

   return(process.trade.interface(
               op, hi, lo, cl,
               ibeg, iend, pos,
               stopLoss=stop.loss,
               stopTrailing=stop.trailing,
               profitTarget=profit.target,
               maxDays=max.days,
               tickSize=tick.size))
}

# trades is a data frame - easier to extract the vectors in R. the format is:
#     entry | exit | position | stop.loss | stop.trailing | profit.target | max.days
# where:
#     entry - the trade's entry
#     exit - the trade's exit
#     position - long (1) or short (-1)
#     stop.loss - the stop loss, NA if none
#     stop.trailing - a trailing stop, NA if none
#     profit.target - a profit targe, NA if none
#     max.days - maximum days to stay in the trade, less or equal to 0 if none
# if both stop.loss and stop.trailing are specified, the stop.trailing is used
process.trades = function(ohlc, trades, tick.size=0.01) {
   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   ibeg = ohlc[trades[,1], which.i=T]
   iend = ohlc[trades[,2], which.i=T]
   
   stopifnot(NCOL(trades) >= 3)

   if(NCOL(trades) < 4) {
      # Append a stop loss column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }

   if(NCOL(trades) < 5) {
      # Append a trailing stop column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }

   if(NCOL(trades) < 6) {
      # Append a profit target column
      trades = cbind(trades, rep(NA, NROW(trades)))
   }
   
   if(NCOL(trades) < 7) {
      # Append a max days column
      trades = cbind(trades, rep(0, NROW(trades)))
   }
   
   res = process.trades.interface(
               ohlc,          # OHLC
               ibeg,          # start index
               iend,          # end index
               trades[,3],    # position
               trades[,4],    # stop loss
               trades[,5],    # stop trailing
               trades[,6],    # profit target
               trades[,7],    # max days
               tick.size)

   # print(head(res))
   res = data.frame(res)
   
   # convert back from ordinary indexes to time indexes
   ohlc.index = index(ohlc)
   res[,1] = ohlc.index[res[,1]]
   res[,2] = ohlc.index[res[,2]]

   return(res)
}

# given an indicator (weights) as an xts, returns trades as a data frame:
#     entry | exit | position
trades.from.indicator = function(indicator) {
   res = trades.from.indicator.interface(indicator)
   res = data.frame(res)
   indicator.index = index(indicator)
   res[,1] = indicator.index[res[,1]]
   res[,2] = indicator.index[res[,2]]
   return(res)
}

# trades an indicator with the same stop/profit settings for all trades
trade.indicator = function(ohlc, indicator, stop.loss=NA, stop.trailing=NA, profit.target=NA, max.days=0) {
   trades = trades.from.indicator(indicator)
   trades[,4] = rep(stop.loss, nrow(trades))
   trades[,5] = rep(stop.trailing, nrow(trades))
   trades[,6] = rep(profit.target, nrow(trades))
   trades[,7] = rep(max.days, nrow(trades))
   colnames(trades) = c("Entry", "Exit", "Position", "StopLoss", "StopTrailing", "ProfitTarget", "MaxDays")
   res = process.trades(ohlc, trades)
   return(res)
}

calculate.returns = function(prices, trades, in.dollars=FALSE) {

   # It's a common mistake to call calculate.returns with ohlc, don't "fix" it
   stopifnot(NCOL(prices) == 1)
   
   # To compute the returns, we need the following columns from the trades data frame:
   #     * start index
   #     * end index
   #     * position
   #     * exit price

   # the lower level c++ interface uses ordinary indexes for the trade's entry and exit
   ibeg = prices[trades[,1], which.i=T]
   iend = prices[trades[,2], which.i=T]

   return(reclass(calculate.returns.interface(prices, ibeg, iend, as.integer(trades[,3]), as.numeric(trades[,7]), in.dollars), prices))
}