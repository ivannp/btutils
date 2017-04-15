//  Copyright (c) 2013-2014, Ivan Popivanov
//  
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//  
//      Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//  
//      Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in
//      the documentation and/or other materials provided with the
//      distribution.
//  
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
//  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
//  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
//  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
//  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
//  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
//  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
//  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
//  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <vector>
#include <cmath>
#include <cassert>

#include "common.h"

using namespace Rcpp;

#define EXIT_ON_LAST             0
#define STOP_LIMIT_ON_OPEN       1
#define STOP_LIMIT_ON_HIGH       2
#define STOP_LIMIT_ON_LOW        3
#define STOP_LIMIT_ON_CLOSE      4
#define STOP_TRAILING_ON_OPEN    5
#define STOP_TRAILING_ON_HIGH    6
#define STOP_TRAILING_ON_LOW     7
#define STOP_TRAILING_ON_CLOSE   8
#define PROFIT_TARGET_ON_OPEN    9
#define PROFIT_TARGET_ON_HIGH   10
#define PROFIT_TARGET_ON_LOW    11
#define PROFIT_TARGET_ON_CLOSE  12
#define MAX_DAYS_LIMIT          13

// #define DEBUG

#ifdef DEBUG
namespace
{
   char buf[4096];
}

void debugMessageFunc(const char * str)
{
   FILE * file = fopen("/home/ivannp/ttt/debug.txt", "a");
   if(file != NULL)
   {
      fprintf(file, "%s\n", str);
      fclose(file);
   }
}

#define DEBUG_MSG(ss) debugMessageFunc((ss))
#else
#define DEBUG_MSG(ss)
#endif

struct TradeLocals {
   double entryPrice;
   double stopPrice;
   double targetPrice;
   double minPrice;
   double maxPrice;
   
   double stopLoss;
   double stopTrailing;
   double profitTarget;
   
   double tickSize;
   
   bool hasStopLoss;
   bool hasStopTrailing;
   bool hasProfitTarget;
   
   TradeLocals() :
      hasStopLoss(false),
      hasStopTrailing(false),
      hasProfitTarget(false)
   {}
};

inline bool processShort(
   double op,
   double hi,
   double lo,
   double cl,
   TradeLocals & locals,
   double & exitPrice,
   int & exitReason) {

   // Process the Open first
   if(locals.hasStopTrailing) {
      if(op >= locals.stopPrice) {
         exitPrice = op;
         exitReason = STOP_TRAILING_ON_OPEN;
   
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      } 
   } else if(locals.hasStopLoss) {
      if(op >= locals.stopPrice) {
         exitPrice = op;
         exitReason = STOP_LIMIT_ON_OPEN;
         
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      }
   }
   
   // Profit target is checked after stop orders
   if(locals.hasProfitTarget) {
      if(op <= locals.targetPrice) {
         exitPrice = op;
         exitReason = PROFIT_TARGET_ON_OPEN;
                                    
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      }
   }
   
   // The position is still on, update a trailing stop with the open
   if(locals.hasStopTrailing && op <= locals.minPrice) {
      locals.minPrice = op;
      locals.stopPrice =
         roundAny(locals.minPrice*(1.0 + std::abs(locals.stopTrailing)), locals.tickSize);
   }

   // Process the "internal" part of the bar
   if(locals.hasStopTrailing) {
      // Check the high
      if(hi >= locals.stopPrice) {
         exitPrice = locals.stopPrice;
         exitReason = STOP_TRAILING_ON_HIGH;
         
         // Update max price. We are making the assumption that the high happened
         // before the low. Thus, we don't want to update the min price.
         locals.maxPrice = std::max(locals.maxPrice, locals.stopPrice);
         
         return true;
      }
   } else if(locals.hasStopLoss) {
      if(hi >= locals.stopPrice) {
         exitPrice = locals.stopPrice;
         exitReason = STOP_LIMIT_ON_HIGH;

         // Update min and max price
         locals.minPrice = std::min(lo, locals.minPrice);
         locals.maxPrice = std::max(locals.stopPrice, locals.maxPrice);
         
         return true;
      }
   }
   
   // Profit target is checked after stop orders
   if(locals.hasProfitTarget) {
      if(lo <= locals.targetPrice) {
         exitPrice = locals.targetPrice;
         exitReason = PROFIT_TARGET_ON_LOW;
                                    
         // Update min and max price
         locals.minPrice = std::min(locals.targetPrice, locals.minPrice);
         locals.maxPrice = std::max(hi, locals.maxPrice);

         return true;
      }
   }
   
   // The position is still on, update a trailing stop with the low
   if(locals.hasStopTrailing && lo < locals.minPrice) {
      locals.minPrice = lo;
      locals.stopPrice =
         roundAny(locals.minPrice*(1.0 + std::abs(locals.stopTrailing)), locals.tickSize);
   }
   
   // We have seen the Hi/Low - update min/maxPrice
   locals.minPrice = std::min(lo, locals.minPrice);
   locals.maxPrice = std::max(hi, locals.maxPrice);
   
   // Finally process the Close
   if(locals.hasStopTrailing) {
      if(cl >= locals.stopPrice) {
         exitPrice = cl;
         exitReason = STOP_TRAILING_ON_CLOSE;
   
         return true;
      } 
   } else if(locals.hasStopLoss) {
      if(cl >= locals.stopPrice) {
         exitPrice = cl;
         exitReason = STOP_LIMIT_ON_CLOSE;

         return true;
      }
   }
   
   // Finally process the Close for a stop trailing order. The stop trailing might
   // have been updated by the Low, thus, we need one more check at the Close.
   if(locals.hasProfitTarget) {
      if(cl <= locals.targetPrice) {
         exitPrice = cl;
         exitReason = PROFIT_TARGET_ON_CLOSE;

         return true;
      }
   }
   
   return false;
}

inline bool processLong(
   double op,
   double hi,
   double lo,
   double cl,
   TradeLocals & locals,
   double & exitPrice,
   int & exitReason) {

   // Process the Open first
   if(locals.hasStopTrailing) {
      if(op <= locals.stopPrice) {
         exitPrice = op;
         exitReason = STOP_TRAILING_ON_OPEN;
   
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      } 
   } else if(locals.hasStopLoss) {
      if(op <= locals.stopPrice) {
         exitPrice = op;
         exitReason = STOP_LIMIT_ON_OPEN;
         
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      }
   }
   
   // Profit target is checked after stop orders
   if(locals.hasProfitTarget) {
      if(op >= locals.targetPrice) {
         exitPrice = op;
         exitReason = PROFIT_TARGET_ON_OPEN;
                                    
         // Update min and max price
         locals.minPrice = std::min(op, locals.minPrice);
         locals.maxPrice = std::max(op, locals.maxPrice);

         return true;
      }
   }
   
   // The position is still on, update a trailing stop with the Open
   if(locals.hasStopTrailing && op > locals.maxPrice) {
      locals.maxPrice = op;
      locals.stopPrice =
         roundAny(locals.maxPrice*(1.0 - std::abs(locals.stopTrailing)), locals.tickSize);
   }

   // Process the "internal" part of the bar
   if(locals.hasStopTrailing) {
      // Check the high
      if(lo <= locals.stopPrice) {
         exitPrice = locals.stopPrice;
         exitReason = STOP_TRAILING_ON_LOW;
         
         // Update min price. We are making the assumption that the low happened
         // before the high. Thus, we don't want to update the max price.
         locals.minPrice = std::min(locals.minPrice, locals.stopPrice);
         
         return true;
      }
   } else if(locals.hasStopLoss) {
      if(lo <= locals.stopPrice) {
         exitPrice = locals.stopPrice;
         exitReason = STOP_LIMIT_ON_LOW;

         // Update min and max price
         locals.minPrice = std::min(locals.stopPrice, locals.minPrice);
         locals.maxPrice = std::max(hi, locals.maxPrice);
         
         return true;
      }
   }
   
   // Profit target is checked after stop orders
   if(locals.hasProfitTarget) {
      if(hi >= locals.targetPrice) {
         exitPrice = locals.targetPrice;
         exitReason = PROFIT_TARGET_ON_HIGH;
                                    
         // Update min and max price
         locals.minPrice = std::min(lo, locals.minPrice);
         locals.maxPrice = std::max(locals.targetPrice, locals.maxPrice);

         return true;
      }
   }
   
   // The position is still on, update a trailing stop with the High
   if(locals.hasStopTrailing && hi > locals.maxPrice) {
      locals.maxPrice = hi;
      locals.stopPrice =
         roundAny(locals.maxPrice*(1.0 - std::abs(locals.stopTrailing)), locals.tickSize);
   }
   
   // We have seen the Hi/Low - update min/maxPrice
   locals.minPrice = std::min(lo, locals.minPrice);
   locals.maxPrice = std::max(hi, locals.maxPrice);
   
   // Finally process the Close for a stop trailing order. The stop trailing might
   // have been updated by the High, thus, we need one more check at the Close.
   if(locals.hasStopTrailing) {
      if(cl <= locals.stopPrice) {
         exitPrice = cl;
         exitReason = STOP_TRAILING_ON_CLOSE;
   
         return true;
      } 
   }

   return false;
}

// The actual workhorse used by the interface functions
void processTrade(
         const std::vector<double> & op,
         const std::vector<double> & hi,
         const std::vector<double> & lo,
         const std::vector<double> & cl,
         int ibeg,
         int iend,
         int pos,
         double stopLoss,
         double stopTrailing,
         double profitTarget,
         int maxDays,
         double tickSize,
         int & exitIndex,
         double & exitPrice,
         int & exitReason,
         double & gain,
         double & minPrice,
         double & maxPrice,
         double & mae,  // maximum adverse excursion
         double & mfe)  // maximum favorable excursion
{
   int ii;
   
   TradeLocals locals;
   locals.hasStopLoss = false;
   locals.hasStopTrailing = false;
   locals.hasProfitTarget = false;
   locals.tickSize = tickSize;

   // Currently positions are initiated only at the close
   locals.minPrice = locals.maxPrice = locals.entryPrice = cl[ibeg];
   
   if(pos < 0) {
      // Short position
      if(!isNA(stopTrailing)) {
         locals.hasStopTrailing = true;
         locals.stopTrailing = stopTrailing;
         locals.stopPrice = roundAny(locals.entryPrice*(1.0 + std::abs(stopTrailing)), tickSize);
      } else if(!isNA(stopLoss)) {
         locals.hasStopLoss = true;
         locals.stopLoss = stopLoss;
         locals.stopPrice = roundAny(locals.entryPrice*(1.0 + std::abs(stopLoss)), tickSize);
      }

      if(!isNA(profitTarget)) {
         locals.targetPrice = roundAny(locals.entryPrice*(1.0 - std::abs(profitTarget)), tickSize);
         locals.profitTarget = profitTarget;
         locals.hasProfitTarget = true;
      }
      
      for(ii = ibeg + 1; ii <= iend; ++ii) {
         if(processShort(op[ii], hi[ii], lo[ii], cl[ii], locals, exitPrice, exitReason)) break;

         // Maximum days for the trade reached
         if(maxDays > 0 && (ii - ibeg) == maxDays) {
            exitPrice = cl[ii];
            exitReason = MAX_DAYS_LIMIT;
            
            break;
         }
      }
      
      if(ii > iend) {
         exitPrice = cl[iend];
         exitReason = EXIT_ON_LAST;
         
         ii = iend;
      }

      gain = 1.0 - exitPrice / locals.entryPrice;

      mae = 1.0 - locals.maxPrice / locals.entryPrice;
      mfe = 1.0 - locals.minPrice / locals.entryPrice;
      
      minPrice = locals.minPrice;
      maxPrice = locals.maxPrice;
   } else {
      // Long position
      if(!isNA(stopTrailing)) {
         locals.hasStopTrailing = true;
         locals.stopTrailing = stopTrailing;
         locals.stopPrice = roundAny(locals.entryPrice*(1.0 - std::abs(stopTrailing)), tickSize);
      } else if(!isNA(stopLoss)) {
         locals.hasStopLoss = true;
         locals.stopLoss = stopLoss;
         locals.stopPrice = roundAny(locals.entryPrice*(1.0 - std::abs(stopLoss)), tickSize);
      }

      if(!isNA(profitTarget)) {
         locals.hasProfitTarget = true;
         locals.profitTarget = profitTarget;
         locals.targetPrice = roundAny(locals.entryPrice*(1.0 + std::abs(profitTarget)), tickSize);
      }
      
      for(ii = ibeg + 1; ii <= iend; ++ii) {
         if(processLong(op[ii], hi[ii], lo[ii], cl[ii], locals, exitPrice, exitReason)) break;

         // Maximum days for the trade reached
         if(maxDays > 0 && (ii - ibeg) == maxDays) {
            exitPrice = cl[ii];
            exitReason = MAX_DAYS_LIMIT;
            
            break;
         }
      }

      if(ii > iend) {
         exitPrice = cl[iend];
         exitReason = EXIT_ON_LAST;
         
         ii = iend;
      }

      gain = exitPrice / locals.entryPrice - 1.0;

      mae = locals.minPrice / locals.entryPrice - 1.0;
      mfe = locals.maxPrice / locals.entryPrice - 1.0;
      
      minPrice = locals.minPrice;
      maxPrice = locals.maxPrice;
   }

   exitIndex = ii;
}

// [[Rcpp::export("process.trade.interface")]]
Rcpp::List processTradeInterface(
               SEXP opIn,
               SEXP hiIn,
               SEXP loIn,
               SEXP clIn,
               int ibeg,
               int iend,
               int pos,
               double stopLoss,
               double stopTrailing,
               double profitTarget,
               int maxDays,
               double tickSize)
{
   std::vector<double> op = Rcpp::as< std::vector<double> >(opIn);
   std::vector<double> hi = Rcpp::as< std::vector<double> >(hiIn);
   std::vector<double> lo = Rcpp::as< std::vector<double> >(loIn);
   std::vector<double> cl = Rcpp::as< std::vector<double> >(clIn);

   double exitPrice, minPrice, maxPrice;
   double gain, mae, mfe;
   int exitIndex;
   int exitReason;
   
   // Call the actuall function to do the work. ibeg and iend are 0 based in cpp and 1 based in R.
   processTrade(
      op, hi, lo, cl,
      ibeg-1, iend-1, pos, stopLoss, stopTrailing, profitTarget, maxDays, tickSize,
      exitIndex, exitPrice, exitReason, gain, minPrice, maxPrice, mae, mfe);
   
   // Build and return the result
   return Rcpp::List::create(
                        Rcpp::Named("exit.index") = exitIndex+1,
                        Rcpp::Named("exit.price") = exitPrice,
                        Rcpp::Named("exit.reason") = exitReason,
                        Rcpp::Named("gain") = gain,
                        Rcpp::Named("min.price") = minPrice,
                        Rcpp::Named("max.price") = maxPrice,
                        Rcpp::Named("mae") = mae,
                        Rcpp::Named("mfe") = mfe);
}

void processTrades(
         const std::vector<double> & op,
         const std::vector<double> & hi,
         const std::vector<double> & lo,
         const std::vector<double> & cl,
         const std::vector<int> & ibeg,
         const std::vector<int> & iend,
         const std::vector<int> & position,
         const std::vector<double> & stopLoss,
         const std::vector<double> & stopTrailing,
         const std::vector<double> & profitTarget,
         const std::vector<int> & maxDays,
         double tickSize,
         std::vector<int> & iendOut,
         std::vector<double> & exitPriceOut,
         std::vector<double> & gainOut,
         std::vector<double> & minPriceOut,
         std::vector<double> & maxPriceOut,
         std::vector<double> & maeOut,
         std::vector<double> & mfeOut,
         std::vector<int> & exitReasonOut )
{
   DEBUG_MSG("processTrades: entered");

   // The number of rows in the output is known, reserve the space required.
   iendOut.resize(0);
   iendOut.reserve(ibeg.size());

   exitPriceOut.resize(0);
   exitPriceOut.reserve(ibeg.size());
   
   minPriceOut.resize(0);
   minPriceOut.reserve(ibeg.size());
   
   maxPriceOut.resize(0);
   maxPriceOut.reserve(ibeg.size());

   gainOut.resize(0);
   gainOut.reserve(ibeg.size());

   maeOut.resize(0);
   maeOut.reserve(ibeg.size());

   mfeOut.resize(0);
   mfeOut.reserve(ibeg.size());

   exitReasonOut.resize(0);
   exitReasonOut.reserve(ibeg.size());

   for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii )
   {
      double exitPrice, minPrice, maxPrice;
      double gain;
      double mae;
      double mfe;
      int exitIndex;
      int exitReason;

      processTrade(
            op, hi, lo, cl,
            ibeg[ii], iend[ii], position[ii], stopLoss[ii], stopTrailing[ii], profitTarget[ii], maxDays[ii], tickSize,
            exitIndex, exitPrice, exitReason, gain, minPrice, maxPrice, mae, mfe);
      // snprintf(buf, sizeof(buf), "%d: exitIndex = %d, exitPrice = %f, exitReason = %d, gain = %f, mae = %f, mfe = %f", 
      //         ii, exitIndex, exitPrice, exitReason, gain, mae, mfe);
      // DEBUG_MSG(buf);

      iendOut.push_back(exitIndex);
      exitPriceOut.push_back(exitPrice);
      minPriceOut.push_back(minPrice);
      maxPriceOut.push_back(maxPrice);
      gainOut.push_back(gain);
      maeOut.push_back(mae);
      mfeOut.push_back(mfe);
      exitReasonOut.push_back(exitReason);
   }
   DEBUG_MSG("processTrades: exited");
}

// [[Rcpp::export("process.trades.interface")]]
Rcpp::List processTradesInterface(
                     SEXP ohlcIn,
                     SEXP ibegsIn,
                     SEXP iendsIn,
                     SEXP positionIn,
                     SEXP stopLossIn,
                     SEXP stopTrailingIn,
                     SEXP profitTargetIn,
                     SEXP maxDaysIn,
                     double tickSize)
{
   DEBUG_MSG("processTradesInterface: entered");
   std::vector<int> ibeg = Rcpp::as< std::vector<int> >( ibegsIn );
   std::vector<int> iend = Rcpp::as< std::vector<int> >( iendsIn );
   std::vector<int> position = Rcpp::as< std::vector<int> >( positionIn );
   std::vector<double> stopLoss = Rcpp::as< std::vector<double> >( stopLossIn );
   std::vector<double> stopTrailing = Rcpp::as< std::vector<double> >( stopTrailingIn );
   std::vector<double> profitTarget = Rcpp::as< std::vector<double> >( profitTargetIn );
   std::vector<int> maxDays  = Rcpp::as< std::vector<int> >( maxDaysIn );

   // Convert ohlc into std vectors
   Rcpp::NumericMatrix ohlcMatrix(ohlcIn);
   std::vector<double> op;
   std::vector<double> hi;
   std::vector<double> lo;
   std::vector<double> cl;
   
   int rows = ohlcMatrix.nrow();
   
   op.reserve(rows);
   hi.reserve(rows);
   lo.reserve(rows);
   cl.reserve(rows);
   
   for(int ii = 0; ii < rows; ++ii)
   {
      op.push_back(ohlcMatrix(ii, 0));
      hi.push_back(ohlcMatrix(ii, 1));
      lo.push_back(ohlcMatrix(ii, 2));
      cl.push_back(ohlcMatrix(ii, 3));

   }
   
   assert(false);
   assert(ibeg.size() == iend.size());

   // vectors in c++ are zero based and in R are one based. convert
   // to the c++ format before calling the workhorse function.
   for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii)
   {
      ibeg[ii] -= 1;
      iend[ii] -= 1;
   }

   // Allocate the output vectors
   std::vector<int> iendOut;
   std::vector<double> exitPrice;
   std::vector<double> minPrice;
   std::vector<double> maxPrice;
   std::vector<double> gain;
   std::vector<double> mae;
   std::vector<double> mfe;
   std::vector<int> reason;

   // Call the c++ function doing the actual work
   processTrades(
         op, hi, lo, cl,
         ibeg, iend, position, stopLoss, stopTrailing, profitTarget, maxDays, tickSize,
         iendOut, exitPrice, gain, minPrice, maxPrice, mae, mfe, reason);

   /* Just some values for testing
   for(int ii = 0; ii < ibeg.size(); ++ii )
   {
      iendOut.push_back(iend[ii]);
      exitPrice.push_back(0.0);
      gain.push_back(0.0);
      mae.push_back(0.0);
      mfe.push_back(0.0);
      reason.push_back(0.0);
   }
   */

   // vectors in c++ are zero based and in R are one based.
   // convert to the R format on the way out.
   for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii )
   {
      ibeg[ii] += 1;
      iend[ii] += 1;
      iendOut[ii] += 1;
   }
   
   assert(ibeg.size() == iend.size());
   
   DEBUG_MSG("processTradesInterface: exited");
   /*
   return Rcpp::List::create(
               Rcpp::Named("Entry") = Rcpp::IntegerVector(ibeg.begin(), ibeg.end()),
               Rcpp::Named("Exit") = Rcpp::IntegerVector(iendOut.begin(),iendOut.end()),
               Rcpp::Named("Position") = Rcpp::IntegerVector(position.begin(), position.end()),
               Rcpp::Named("StopLoss") = Rcpp::NumericVector(stopLoss.begin(), stopLoss.end()),
               Rcpp::Named("StopTrailing") = Rcpp::NumericVector(stopTrailing.begin(), stopTrailing.end()),
               Rcpp::Named("ProfitTarget") = Rcpp::NumericVector(profitTarget.begin(), profitTarget.end()),
               Rcpp::Named("ExitPrice") = Rcpp::NumericVector(exitPrice.begin(), exitPrice.end()),
               Rcpp::Named("Gain") = Rcpp::NumericVector(gain.begin(), gain.end()),
               Rcpp::Named("MAE") = Rcpp::NumericVector(mae.begin(), mae.end()),
               Rcpp::Named("MFE") = Rcpp::NumericVector(mfe.begin(), mfe.end()),
               Rcpp::Named("Reason") = Rcpp::IntegerVector(reason.begin(), reason.end()));
   */
   return Rcpp::DataFrame::create(
               Rcpp::Named("Entry") = ibeg,
               Rcpp::Named("Exit") = iendOut,
               Rcpp::Named("Position") = position,
               Rcpp::Named("StopLoss") = stopLoss,
               Rcpp::Named("StopTrailing") = stopTrailing,
               Rcpp::Named("ProfitTarget") = profitTarget,
               Rcpp::Named("ExitPrice") = exitPrice,
               Rcpp::Named("Gain") = gain,
               Rcpp::Named("MinPrice") = minPrice,
               Rcpp::Named("MaxPrice") = maxPrice,
               Rcpp::Named("MAE") = mae,
               Rcpp::Named("MFE") = mfe,
               Rcpp::Named("Reason") = reason);
}

void tradesFromIndicator(
         const std::vector<double> & indicator,
         std::vector<int> & ibeg,
         std::vector<int> & iend,
         std::vector<int> & position)
{
   // The last index needs special processing
   int lastId = indicator.size() - 1;
   
   int ii = 0;
   // Skipt starting NAs
   while(ii < lastId && isNA(indicator[ii])) ++ii;
   
   if(ii < lastId) {
      // Process the first element
      if(indicator[ii] != 0.0)
      {
         ibeg.push_back(ii);
         position.push_back(indicator[ii]);
      }
      
      ++ii;
      
      for(; ii < lastId; ++ii)
      {
         if(indicator[ii] != indicator[ii-1])
         {
            if(indicator[ii-1] != 0.0)
            {
               // Close the open position
               iend.push_back(ii);
            }
            
            if(indicator[ii] != 0.0)
            {
               // Open a new position
               ibeg.push_back(ii);
               position.push_back(indicator[ii]);
            }
         }
      }
   }

   // On the last index we only close an existing open position
   if(ibeg.size() > iend.size())
   {
      iend.push_back(lastId);
   }
   
   assert(iend.size() == ibeg.size());
}

// [[Rcpp::export("trades.from.indicator.interface")]]
Rcpp::List tradesFromIndicatorInterface(SEXP indicatorIn)
{
   std::vector<double> indicator = Rcpp::as< std::vector<double> >( indicatorIn );
   std::vector<int> ibeg;
   std::vector<int> iend;
   std::vector<int> position;
   tradesFromIndicator(indicator, ibeg, iend, position);
   
   // vectors in c++ are zero based and in R are one based.
   // convert to the R format on the way out.
   for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii)
   {
      ++ibeg[ii];
      ++iend[ii];
   }
   
   return Rcpp::List::create(
               Rcpp::Named("Entry") = Rcpp::IntegerVector(ibeg.begin(), ibeg.end()),
               Rcpp::Named("Exit") = Rcpp::IntegerVector(iend.begin(), iend.end()),
               Rcpp::Named("Position") = Rcpp::IntegerVector(position.begin(), position.end()));
}

void calculateReturns(
         const std::vector<double> & cl,
         const std::vector<int> & ibeg,
         const std::vector<int> & iend,
         const std::vector<int> & position,
         const std::vector<double> & exitPrice,
         bool inDollars,
         std::vector<double> & returns)
{
   returns.resize(cl.size(), 0.0);

   if(!inDollars) {
      // Cycle through the trades
      for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii) {
         // Process the last bar of a trade separately - it needs special attention.
         for(int jj = ibeg[ii] + 1; jj < iend[ii]; ++jj) {
            returns[jj] = (cl[jj] / cl[jj-1] - 1.0)*position[ii];
         }
   
         // For the last bar use the exit price
         returns[iend[ii]] = (exitPrice[ii] / cl[iend[ii]-1] - 1.0)*position[ii];
      }
   } else {
      // Calculate the returns in dollars - useful for trading futures.
      for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii) {
         // Process the last bar of a trade separately - it needs special attention.
         for(int jj = ibeg[ii] + 1; jj < iend[ii]; ++jj) {
            returns[jj] = (cl[jj] - cl[jj-1])*position[ii];
         }
   
         // For the last bar use the exit price
         returns[iend[ii]] = (exitPrice[ii] - cl[iend[ii]-1])*position[ii];
      }
   }
}

// [[Rcpp::export("calculate.returns.interface")]]
Rcpp::NumericVector calculateReturnsInterface(
                        SEXP clIn,
                        SEXP ibegIn,
                        SEXP iendIn,
                        SEXP positionIn,
                        SEXP exitPriceIn,
                        bool inDollars)
{
   // Convert inputs into std vectors
   std::vector<double> cl = Rcpp::as< std::vector<double> >(clIn);
   std::vector<int> ibeg = Rcpp::as< std::vector<int> >(ibegIn);
   std::vector<int> iend = Rcpp::as< std::vector<int> >(iendIn);
   std::vector<int> position = Rcpp::as< std::vector<int> >(positionIn);
   std::vector<double> exitPrice = Rcpp::as< std::vector<double> >(exitPriceIn);
 
   // c++ uses 0 based indexes
   for(std::vector<int>::size_type ii = 0; ii < ibeg.size(); ++ii)
   {
      ibeg[ii] -= 1;
      iend[ii] -= 1;
   }
   
   std::vector<double> result;
   calculateReturns(cl, ibeg, iend, position, exitPrice, inDollars, result);

   return Rcpp::NumericVector(result.begin(), result.end());
}