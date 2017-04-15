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

cap.trade.duration = function(
                        indicator,
                        short.min.cap=-1, long.min.cap=-1, 
                        short.max.cap=-1, long.max.cap=-1,
                        wait.new.signal=TRUE) {
   stopifnot(short.min.cap == -1 || short.max.cap == -1 || (short.max.cap >= short.min.cap))
   stopifnot(long.min.cap == -1 || long.max.cap == -1 || (long.max.cap >= long.min.cap))
   return(reclass(cap.trade.duration.interface(indicator, short.min.cap, long.min.cap, short.max.cap, long.max.cap, wait.new.signal), indicator))
}

construct.indicator = function(long.entries, long.exits, short.entries, short.exits) {
   return(reclass(construct.indicator.interface(long.entries, long.exits, short.entries, short.exits), long.entries))
}

indicator.from.trendline = function(trendline, thresholds) {
   if(missing(thresholds)) {
      thresholds = rep(0, NROW(trendline))
   }

   return(reclass(indicator.from.trendline.interface(trendline, thresholds), trendline))
}

zig.zag = function(prices, changes, percent=T) {
   return(reclass(data.frame(zig.zag.interface(prices,changes,percent)),prices))
}

returns.rsi = function(returns, n=14) {
   up = returns
   which.dn = which(up < 0)
   dn = up*0
   dn[which.dn] = -up[which.dn]
   up[which.dn] = 0
   
   mavg.up = runMean(up, n=n)
   mavg.dn = runMean(dn, n=n)
   
   rsi = 100*mavg.up/(mavg.up + mavg.dn)
   
   return(reclass(rsi,returns))
}