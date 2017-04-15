require(quantmod)
require(RUnit)

require(btutils)

load("unitTests/indicator.RData")

test.cap.trade.duration = function() {
   # Noop
   checkEqualsNumeric(cap.trade.duration(indicator), indicator)
   
   # No shorts
   ind = cap.trade.duration(indicator, short.max.cap=0)
   checkTrue(all(ind >= 0, na.rm=T))
   
   # No longs
   ind = cap.trade.duration(indicator, long.max.cap=0)
   checkTrue(all(ind <= 0, na.rm=T))
   
   # Limit max for short positions
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, short.max.cap=2))))
   checkTrue(all(rr$lengths[which(rr$values==-1, arr.ind=T)] <= 2, na.rm=T))
   
   # Limit max for long positions
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.max.cap=2))))
   checkTrue(all(rr$lengths[which(rr$values==1, arr.ind=T)] <= 2, na.rm=T))
   
   # Limit max for both
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.max.cap=2, short.max.cap=3))))
   checkTrue(all(rr$lengths[which(rr$values==1, arr.ind=T)] <= 2, na.rm=T))
   checkTrue(all(rr$lengths[which(rr$values==-1, arr.ind=T)] <= 3, na.rm=T))

   # Limit min for both
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.min.cap=6, short.min.cap=5))))
   checkTrue(all(rr$lengths[head(which(rr$values==1, arr.ind=T), -1)] >= 6, na.rm=T))
   checkTrue(all(rr$lengths[head(which(rr$values==-1, arr.ind=T), -1)] >= 5, na.rm=T))

   # Limit min for longs - hand verified result
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.min.cap=6))))
   checkTrue(all(rr$lengths[head(which(rr$values==1, arr.ind=T), -1)] >= 6, na.rm=T))
   rr.lengths = rr$lengths[which(!is.na(rr$values), arr.ind=T)]
   checkEqualsNumeric(rr.lengths[1:12], c(16, 3, 6, 5, 6, 9, 6, 2, 4, 13, 3, 6), tolerance=0)
   rr.values = rr$values[which(!is.na(rr$values), arr.ind=T)]
   checkEqualsNumeric(rr.values[1:12], c(1, -1, 1, -1, 1, 0, 1, 0, -1, 1, -1, 1), tolerance=0)

   # Same as above, but using wait.new.signal = FALSE
   rr = rle(sign(as.numeric(cap.trade.duration(indicator, long.min.cap=6, wait.new.signal=F))))
   checkTrue(all(rr$lengths[head(which(rr$values==1, arr.ind=T), -1)] >= 6, na.rm=T))
   rr.lengths = rr$lengths[which(!is.na(rr$values), arr.ind=T)]
   checkEqualsNumeric(rr.lengths[1:11], c(16, 3, 6, 5, 6, 9, 8, 4, 13, 3, 6), tolerance=0)
   rr.values = rr$values[which(!is.na(rr$values), arr.ind=T)]
   checkEqualsNumeric(rr.values[1:11], c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1), tolerance=0)
}

test.indicator.from.trendline = function() {
   trendline = c(1, 2, 3, 2, 3, 1, 2)
   checkEqualsNumeric(indicator.from.trendline(trendline), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 1")

   thresholds = rep(0, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 2")

   thresholds = rep(1, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, -1, 1, -1, 1), tolerance=0, msg=" *** test 3")

   thresholds = rep(1.1, NROW(trendline))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 1, 1, 1, 1, -1, -1), tolerance=0, msg=" *** test 4")

   trendline = c(NA, NA, NA, 1, 2, 3, 2, 3, 1, 2)
   thresholds = rep(1.1, NROW(trendline))
   # print(indicator.from.trendline(trendline, thresholds))
   checkEqualsNumeric(indicator.from.trendline(trendline, thresholds), c(0, 0, 0, 0, 1, 1, 1, 1, -1, -1), tolerance=0, msg=" *** test 5")
}