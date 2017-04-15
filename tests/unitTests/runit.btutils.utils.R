require(quantmod)
require(RUnit)

require(btutils)

test.locf = function() {
   # Noop
   checkEqualsNumeric(locf(seq(1, 100)), seq(1, 100))
   checkEqualsNumeric(locf(seq(1, 100), value=0), seq(1, 100))
   
   checkEqualsNumeric(locf(c(NA, NA, 0, 1, 1, 0, 0, 2), value=0), c(NA, NA, 0, 1, 1, 1, 1, 2))
   checkEqualsNumeric(locf(c(NA, NA, 0, 1, 1, 0, 0), value=5), c(NA, NA, 0, 1, 1, 0, 0))
   
   checkEqualsNumeric(locf(c(NA, NA, 0, 1, 1, NA, 0)), na.locf(c(NA, NA, 0, 1, 1, NA, 0), na.rm=F))
   
   checkEqualsNumeric(
      locf(cbind(c(NA, NA, 0, 1, 1, NA, 0), c(NA, 0, 0, NA, 1, 1, 1)), na.rm=T),
      cbind(c(0, 1, 1, 1, 0), c(0, 0, 1, 1, 1)))

   checkEqualsNumeric(
      locf(cbind(c(NA, NA, 0, 1, 1, NA, 0), c(NA, 0, 0, NA, 1, 1, 1))),
      cbind(c(NA, NA, 0, 1, 1, 1, 0), c(NA, 0, 0, 0, 1, 1, 1)))
}

test.leading.nas = function() {
   checkEqualsNumeric(leading.nas(rep(0, 10)), 0)
   checkEqualsNumeric(leading.nas(c(NA, rep(0, 10))), 1)
}