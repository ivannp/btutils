require(quantmod)
require(RUnit)

require(btutils)

load("unitTests/drm.RData")

test.process.trade.long = function() {
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1)
   
   # 0 - exit on the last day
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "001: Bad exit.reason", tolerance=0)

   gain = as.numeric(Cl(drm[5225])) / as.numeric(Cl(drm[5205])) - 1
   checkEqualsNumeric(gain, df$gain, "002: Bad gain", tolerance=0)
   
   # For this specific subsequence, we know that the min and the max are not on the first Close
   max.price = as.numeric(max(Hi(drm[5206:5225])))
   min.price = as.numeric(min(Lo(drm[5206:5225])))
   mae = min.price / as.numeric(Cl(drm[5205])) - 1
   mfe = max.price / as.numeric(Cl(drm[5205])) - 1
   checkEqualsNumeric(mae, df$mae, tolerance=0.00001)
   checkEqualsNumeric(mfe, df$mfe, tolerance=0.00001)
   checkEqualsNumeric(min.price, df$min.price)
   checkEqualsNumeric(max.price, df$max.price)
   
   # Add a stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.05)
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "003: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(as.numeric(max(Hi(drm[5206:5225]))), df$max.price, "003a: Bad max.price")
   checkEqualsNumeric(as.numeric(min(Lo(drm[5206:5225]))), df$min.price, "003b: Bad min.price")
   
   # Add a stop loss which is hit at the low
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.01)
   
   # 3 - stop limit on low
   checkEqualsNumeric(STOP_LIMIT_ON_LOW, df$exit.reason, "004: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(-0.01, round(df$gain, 4), "005: Bad gain")
   checkEqualsNumeric(167.22, df$exit.price, "005a: Bad exit.price")
   checkEqualsNumeric(167.22, df$min.price, "005a: Bad min.price")
   checkEqualsNumeric(168.91, df$max.price, "005b: Bad max.price")
   checkEqualsNumeric(5206, df$exit.index, "006: Bad exit.index")
   
   # Add a stop loss which is hit at the open
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, 0.007)
   
   # 1 - stop limit on open
   checkEqualsNumeric(STOP_LIMIT_ON_OPEN, df$exit.reason, "007: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(as.numeric(Op(drm[5206])) / as.numeric(Cl(drm[5205])) - 1, df$gain, "008: Bad gain")
   checkEqualsNumeric(5206, df$exit.index, "009: Bad exit.index")
   
   # Add a trailing stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, 1, NA, 0.05)
   
   # 0 - exit on the last day
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "010: Bad exit.reason", tolerance=0)
   
   # Add a trailing stop loss which is hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, 0.05)

   # 6 - stop trailing on low
   checkEqualsNumeric(STOP_TRAILING_ON_LOW, df$exit.reason, "011: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5213, df$exit.index, "012: Bad exit.index", tolerance=0)
   checkEqualsNumeric(164.92, df$exit.price, "013: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(-0.0067, round(df$gain, 4), "014: Bad gain")
   checkEqualsNumeric(-0.0067, round(df$mae, 4), "015: Bad mae")
   checkEqualsNumeric(0.04553, round(df$mfe, 5), "016: Bad mfe")
   
   # Add a profit target which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, NA, 0.1)
   
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "017: Bad exit.reason", tolerance=0)
   
   # Add a profit target which is hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, NA, 0.04)
   
#    print(df$exit.reason)
#    print(df$exit.index)
#    print(df$exit.price)
#    print(df$min.price)
#    print(df$max.price)
#    print(df$gain)
#    print(df$mae)
#    print(df$mfe)
   
   # Profit target on High
   checkEqualsNumeric(PROFIT_TARGET_ON_HIGH, df$exit.reason, "018: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5198, df$exit.index, "019: Bad exit.index", tolerance=0)
   checkEqualsNumeric(172.68, df$exit.price, "020: Bad exit.price", tolerance=0)
   checkEqualsNumeric(166.04, df$min.price, "020a: Bad min.price", tolerance=0)
   checkEqualsNumeric(172.68, df$max.price, "020b: Bad max.price", tolerance=0)
   checkEqualsNumeric(0.04, round(df$gain, 4), "021: Bad gain")
   checkEqualsNumeric(0, round(df$mae, 4), "022: Bad mae")
   checkEqualsNumeric(0.04, round(df$mfe, 2), "023: Bad mfe")
   
   # Same profit target together with a trailing stop
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, 0.05, 0.04)
   
   # Profit target on High
   checkEqualsNumeric(PROFIT_TARGET_ON_HIGH, df$exit.reason, "024: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5198, df$exit.index, "025: Bad exit.index", tolerance=0)
   checkEqualsNumeric(172.6816, df$exit.price, "026: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(0.04, round(df$gain, 4), "027: Bad gain")
   checkEqualsNumeric(0, round(df$mae, 4), "028: Bad mae")
   checkEqualsNumeric(0.04, round(df$mfe, 2), "029: Bad mfe")

   # Increase the profit target together and we hit the trailing stop
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, 0.05, 0.05)
   

   # Profit target on High
   checkEqualsNumeric(STOP_TRAILING_ON_LOW, df$exit.reason, "024: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5213, df$exit.index, "030: Bad exit.index", tolerance=0)
   checkEqualsNumeric(164.92, df$exit.price, "031: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(-0.0067, round(df$gain, 4), "032: Bad gain")
   checkEqualsNumeric(-0.0067, round(df$mae, 4), "033: Bad mae")
   checkEqualsNumeric(0.04553, round(df$mfe, 5), "034: Bad mfe")

   # Profit target, a trailing stop and limit on the number of days in
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, 1, NA, 0.05, 0.04, 2)
   
   # Max days in reached
   checkEqualsNumeric(MAX_DAYS_LIMIT, df$exit.reason, "035: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5192, df$exit.index, "036: Bad exit.index", tolerance=0)
   checkEqualsNumeric(168.87, df$exit.price, "037: Bad exit.price")
   checkEqualsNumeric(0.0170, round(df$gain, 4), "038: Bad gain")
   checkEqualsNumeric(0, round(df$mae, 4), "039: Bad mae")
   checkEqualsNumeric(0.0172, round(df$mfe, 4), "040: Bad mfe")
}

test.process.trade.short = function() {
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, -1)
   
   # 0 - exit on the last day
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "001: Bad exit.reason", tolerance=0)
   
   gain = 1 - as.numeric(Cl(drm[5225])) / as.numeric(Cl(drm[5205]))
   checkEqualsNumeric(gain, df$gain, "002: Bad gain", tolerance=0)
   
   # For this specific subsequence, we know that the min and the max are not on the first Close
   max.price = as.numeric(max(Hi(drm[5206:5225])))
   min.price = as.numeric(min(Lo(drm[5206:5225])))
   mae = 1 - max.price / as.numeric(Cl(drm[5205]))
   mfe = 1 - min.price / as.numeric(Cl(drm[5205]))
   checkEqualsNumeric(mae, df$mae, tolerance=0.00001)
   checkEqualsNumeric(mfe, df$mfe, tolerance=0.00001)
   
   # Add a stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, -1, 0.05)
   # Exit on the last day
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "003: Bad exit.reason", tolerance=0)
   
   # Add a stop loss which is hit at the High
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, -1, 0.04)
   checkEqualsNumeric(STOP_LIMIT_ON_HIGH, df$exit.reason, "004: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(-0.04, round(df$gain, 2), "005: Bad gain")
   checkEqualsNumeric(175.67, df$exit.price, "005a: Bad exit.price")
   checkEqualsNumeric(164.53, df$min.price, "005b: Bad min.price")
   checkEqualsNumeric(175.67, df$max.price, "005c: Bad max.price")
   checkEqualsNumeric(5222, df$exit.index, "006: Bad exit.index")
   
   # Add a stop loss which is hit at the open
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, -1, 0.028)
   checkEqualsNumeric(STOP_LIMIT_ON_OPEN, df$exit.reason, "007: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(1 - as.numeric(Op(drm[5220])) / as.numeric(Cl(drm[5205])), df$gain, "008: Bad gain")
   checkEqualsNumeric(5220, df$exit.index, "009: Bad exit.index")
   
   # Add a trailing stop loss which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5205, 5225, -1, NA, 0.07)
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "010: Bad exit.reason", tolerance=0)
   
   # Add a trailing stop loss which is hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5197, 5225, -1, NA, 0.05)
   checkEqualsNumeric(STOP_TRAILING_ON_HIGH, df$exit.reason, "011: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5219, df$exit.index, "012: Bad exit.index", tolerance=0)
   checkEqualsNumeric(172.7565, df$exit.price, "013: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(-0.0099, round(df$gain, 4), "014: Bad gain")
   checkEqualsNumeric(-0.0148, round(df$mae, 4), "015: Bad mae")
   checkEqualsNumeric(0.0382, round(df$mfe, 4), "016: Bad mfe")
   
   # Add a profit target which is not hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5190, 5225, -1, NA, NA, 0.05)
   checkEqualsNumeric(EXIT_ON_LAST, df$exit.reason, "017: Bad exit.reason", tolerance=0)
   
   # Add a profit target which is hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5198, 5225, -1, NA, NA, 0.03)
   # Profit target on Open
   checkEqualsNumeric(PROFIT_TARGET_ON_OPEN, df$exit.reason, "018: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5206, df$exit.index, "019: Bad exit.index", tolerance=0)
   checkEqualsNumeric(167.48, df$exit.price, "020: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(0.0322, round(df$gain, 4), "021: Bad gain")
   checkEqualsNumeric(-0.0032, round(df$mae, 4), "022: Bad mae")
   checkEqualsNumeric(0.0322, round(df$mfe, 4), "023: Bad mfe")

   # Add a profit target which is hit
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5198, 5225, -1, NA, NA, 0.049)
   # Profit target on Low
   checkEqualsNumeric(PROFIT_TARGET_ON_LOW, df$exit.reason, "024: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5213, df$exit.index, "025: Bad exit.index", tolerance=0)
   checkEqualsNumeric(164.57, round(df$exit.price, 2), "026: Bad exit.price")
   checkEqualsNumeric(0.049, round(df$gain, 5), "027: Bad gain")
   checkEqualsNumeric(-0.0032, round(df$mae, 4), "028: Bad mae")
   checkEqualsNumeric(0.049, round(df$mfe, 5), "029: Bad mfe")
   
   #       print(df$exit.reason)
   #       print(df$exit.index)
   #       print(df$exit.price)
   #       print(df$gain)
   #       print(df$mae)
   #       print(df$mfe)
   
   # Profit target together with a trailing stop
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5197, 5225, -1, NA, 0.05, 0.05)
   # Stop trailing on High
   checkEqualsNumeric(STOP_TRAILING_ON_HIGH, df$exit.reason, "030: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5219, df$exit.index, "031: Bad exit.index", tolerance=0)
   checkEqualsNumeric(172.7565, df$exit.price, "032: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(-0.0099, round(df$gain, 4), "033: Bad gain")
   checkEqualsNumeric(-0.0148, round(df$mae, 4), "034: Bad mae")
   checkEqualsNumeric(0.0382, round(df$mfe, 4), "035: Bad mfe")
   
   # Profit target together with a trailing stop
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5198, 5225, -1, NA, -0.05, 0.03)
   # Profit target on Open
   checkEqualsNumeric(PROFIT_TARGET_ON_OPEN, df$exit.reason, "036: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5206, df$exit.index, "037: Bad exit.index", tolerance=0)
   checkEqualsNumeric(167.48, df$exit.price, "038: Bad exit.price", tolerance=0.001)
   checkEqualsNumeric(0.0322, round(df$gain, 4), "039: Bad gain")
   checkEqualsNumeric(-0.0032, round(df$mae, 4), "040: Bad mae")
   checkEqualsNumeric(0.0322, round(df$mfe, 4), "041: Bad mfe")
   
   # Profit target, a trailing stop and limit on the number of days in
   df = process.trade(Op(drm), Hi(drm), Lo(drm), Cl(drm), 5198, 5225, -1, NA, -0.05, 0.03, 7)
   # Profit target on Open
   checkEqualsNumeric(MAX_DAYS_LIMIT, df$exit.reason, "042: Bad exit.reason", tolerance=0)
   checkEqualsNumeric(5205, df$exit.index, "043: Bad exit.index", tolerance=0)
   checkEqualsNumeric(Cl(drm)[5205], df$exit.price, "044: Bad exit.price")
   checkEqualsNumeric(0.0239, round(df$gain, 4), "045: Bad gain")
   checkEqualsNumeric(-0.0032, round(df$mae, 4), "046: Bad mae")
   checkEqualsNumeric(0.0265, round(df$mfe, 4), "047: Bad mfe")
}

test.calculate.returns = function() {
   drm.rets = ROC(Cl(drm), n=1, type="discrete")
   drm.macd = MACD(Cl(drm), nFast=1, nSlow=200)[,1]
   drm.indicator = ifelse(drm.macd < 0, 0, 1)
   res1 = na.trim(lag.xts(drm.indicator) * drm.rets)
   
   drm.trades = trades.from.indicator(drm.indicator)
   drm.ptrades = process.trades(drm, drm.trades)
   res2 = calculate.returns(Cl(drm), drm.ptrades)
   
   mm = merge(res1, res2, all=F)
   checkEqualsNumeric(round(mm[,1], 6), round(mm[,2], 6), "001: Results don't match")

   # Add a stop loss column
   drm.trades = cbind(drm.trades, rep(0.02, NROW(drm.trades)))
   drm.ptrades = process.trades(drm, drm.trades)
   res2 = calculate.returns(Cl(drm), drm.ptrades)

   # Verify that some returns on the last trade day
   # are not the same as the returns for that day.
   rr = res2[drm.ptrades[,"Exit"]]
   mm = merge(round(res1, 4), round(rr, 4), all=F)
   checkTrue(any(mm[,1] != mm[,2]))
}