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

#include <Rcpp.h>
#include "common.h"

using namespace Rcpp;

void capTradeDuration(
         std::vector<double> & indicator,
         int shortMinCap,
         int longMinCap,
         int shortMaxCap,
         int longMaxCap,
         bool waitNewSignal)
{
   if(shortMaxCap < 0 && longMaxCap < 0 && shortMinCap < 0 && longMinCap < 0) return;

   std::vector<double>::size_type ii = 0;

   // Skip leading NAs
   while(ii < indicator.size() && isNA(indicator[ii])) ++ii;
   
   while(ii < indicator.size()) {
      // Find the beginning of a position
      while(ii < indicator.size() && indicator[ii] == 0) ++ii;
      
      if(ii == indicator.size()) break;
      
      // Apply caps to this position
      int ss = sign(indicator[ii]);
      int minCap, maxCap;
      if(ss == -1) {
         minCap = shortMinCap;
         maxCap = shortMaxCap;
      } else if(ss == 1) {
         minCap = longMinCap;
         maxCap = longMaxCap;
      }

      if(minCap != -1 || maxCap != -1) {
         int daysIn = 1;
         bool done = false;
         int prevIndSign = -10;  // An impossible value if we are satisfying minCap
         while(ii < indicator.size() && daysIn <= minCap) {
            int indSign = sign(indicator[ii]);

            // Remember that the position changed, thus, we are done once minCap is satisfied
            if(!done && indSign != ss) done = true;

            // Remember the original indicator value before we overwrite it. Also
            // notice, that when we can only extend the indicator with 1 or -1.
            prevIndSign = indSign;
            if(indSign != ss) indicator[ii] = ss;

            ++daysIn;
            ++ii;
         }

         if(done && waitNewSignal) {
            // We have satisfied minCap and we need to wait for a new signal
            while(ii < indicator.size() && sign(indicator[ii]) == prevIndSign ) {
               indicator[ii] = 0;
               ++ii;
            }
         }

         if(!done || !waitNewSignal) {
            while(ii < indicator.size() && sign(indicator[ii]) == ss) {
               // Update the indicator if duration is over maxCap
               if(maxCap > -1 && daysIn > maxCap) indicator[ii] = 0;
               
               ++daysIn;
               ++ii;
            }
         }
      } else {
         while(ii < indicator.size() && sign(indicator[ii]) == ss) ++ii;
      }
   }
}

// [[Rcpp::export("cap.trade.duration.interface")]]
Rcpp::NumericVector capTradeDurationInterface(
                        SEXP indicatorIn,
                        int shortMinCap,
                        int longMinCap,
                        int shortMaxCap,
                        int longMaxCap,
                        bool waitNewSignal)
{
   // Convert ohlc into std vectors
   std::vector<double> indicator = Rcpp::as< std::vector<double> >(indicatorIn);
   capTradeDuration(
         indicator,
         shortMinCap,
         longMinCap,
         shortMaxCap,
         longMaxCap,
         waitNewSignal);

   return Rcpp::NumericVector(indicator.begin(), indicator.end());
}

void constructIndicator(
         const std::vector<bool> & longEntries,
         const std::vector<bool> & longExits,
         const std::vector<bool> & shortEntries,
         const std::vector<bool> & shortExits,
         std::vector<double> & indicator)
{
   indicator.resize(longEntries.size(), 0.0);

   std::vector<double>::size_type ii = 0;

   while(ii < indicator.size() && !longEntries[ii] && !shortEntries[ii]) ++ii;

   int pos = 0;
   while(ii < indicator.size()) {
      switch(pos) {
         case -1:
            if(longEntries[ii]) pos = 1;
            else if(shortExits[ii]) pos = 0;
            break;
            
         case 0:
            if(longEntries[ii]) pos = 1;
            else if(shortEntries[ii]) pos = -1;
            break;
            
         case 1:
            if(shortEntries[ii]) pos = -1;
            else if(longExits[ii]) pos = 0;
            break;
      }
      
      indicator[ii++] = pos;
   }
}

// [[Rcpp::export("construct.indicator.interface")]]
Rcpp::NumericVector constructIndicatorInterface(SEXP longEntriesIn, SEXP longExitsIn, SEXP shortEntriesIn, SEXP shortExitsIn)
{
   std::vector<bool> longEntries = Rcpp::as<std::vector<bool> >(longEntriesIn);
   std::vector<bool> longExits  = Rcpp::as<std::vector<bool> >(longExitsIn);
   std::vector<bool> shortEntries = Rcpp::as<std::vector<bool> >(shortEntriesIn);
   std::vector<bool> shortExits  = Rcpp::as<std::vector<bool> >(shortExitsIn);
   
   std::vector<double> indicator;
   constructIndicator(longEntries, longExits, shortEntries, shortExits, indicator);

   return Rcpp::NumericVector(indicator.begin(), indicator.end());
}

void indicatorFromTrendline(const std::vector<double> & trendline, const std::vector<double> & thresholds, std::vector<int> & indicator)
{
   indicator.resize(trendline.size(), 0);

   std::vector<double>::size_type ii = 0;
   while(ii < trendline.size() && (isNA(trendline[ii]) || isNA(thresholds[ii]))) {
      ++ii;
   }

   ++ii;

   if(ii >= trendline.size()) return;

   int id = ii;
   int direction = sign(trendline[ii] - trendline[ii-1]);
   double threshold = trendline[ii] - thresholds[ii]*direction;
   indicator[ii] = direction;
   for(++ii; ii < trendline.size(); ++ii) {
      if(direction == -1) {
         if(trendline[ii] <= trendline[id]) {
            // A new minimum, reset
            id = ii;
            threshold = trendline[ii] + thresholds[ii];
         } else if(trendline[ii] >= threshold) {
            // Trend reversal
            id = ii;
            threshold = trendline[ii] - thresholds[ii];
            direction = 1;
         }
      } else if(direction == 1) {
         if(trendline[ii] >= trendline[id]) {
            // A new maximum, reset
            id = ii;
            threshold = trendline[ii] - thresholds[ii];
         } else if(trendline[ii] <= threshold) {
            // Trend reversal
            id = ii;
            threshold = trendline[ii] + thresholds[ii];
            direction = -1;
         }
      } else {
         if(trendline[ii] > trendline[ii-1]) {
            id = ii;
            direction = 1;
            threshold = trendline[ii] - thresholds[ii];
         } else if(trendline[ii] < trendline[ii-1]) {
            id = ii;
            direction = -1;
            threshold = trendline[ii] + thresholds[ii];
         }
      }
      indicator[ii] = direction;
   }
}

// [[Rcpp::export("indicator.from.trendline.interface")]]
Rcpp::NumericVector indicatorFromTrendlineInterface(SEXP trendlineIn, SEXP thresholdsIn)
{
   std::vector<double> trendline = Rcpp::as<std::vector<double> >(trendlineIn);
   std::vector<double> thresholds  = Rcpp::as<std::vector<double> >(thresholdsIn);
   
   std::vector<int> indicator;
   indicatorFromTrendline(trendline, thresholds, indicator);

   return Rcpp::NumericVector(indicator.begin(), indicator.end());
}

void zigZag(
         const std::vector<double> & close,
         const std::vector<double> & changes,
         bool percent,
         std::vector<int> & indicator,
         std::vector<double> & inflections,
         std::vector<double> & targets,
         std::vector<double> & corrections,
         std::vector<int> & age)
{
   int len = close.size();
   
   indicator.resize(len, 0);
   inflections.resize(len, NA_REAL);
   corrections.resize(len, 0);
   targets.resize(len, NA_REAL);
   age.resize(len, 0);
   
   int ii = 0;
   // Skip all NAs in the changes vector
   while(ii < len && isNA(changes[ii])) ++ii;
   
   if(ii >= len) return;
   
   int state = 0;
   int jj = ii;
   double target = changes[jj];
   
   // Find the first up or down state
   for(++ii; ii < len; ++ii) {
      if(percent) {
         double pct = close[ii]/close[jj] - 1.0;
         if(pct > target) {
            state = 1;
            break;
         }
         
         pct = 1.0 - close[ii]/close[jj];
         if(pct > target) {
            state = -1;
            break;
         }
      } else {
         double cash = close[ii] - close[jj];
         if(cash > target) {
            state = 1;
            break;
         }
         
         cash = close[jj] - close[ii];
         if(cash > target) {
            state = -1;
            break;
         }
      }
   }
   
   if(ii < len) {
      jj = ii;
      target = changes[jj];
      inflections[ii] = close[ii];
      indicator[ii] = state;
      targets[ii] = target;
   }
   
   // The main loop
   for(++ii; ii < len; ++ii) {
      if(state == 1) {
         if(close[ii] >= close[jj]) {
            indicator[ii] = 1;
            age[ii] = age[ii-1] + 1;
            inflections[ii] = inflections[ii-1];
            target = changes[ii];
            targets[ii] = changes[ii];
            jj = ii;
         } else {
            bool newTrend = false;
            double change;
            if(percent) {
               change = 1.0 - close[ii]/close[jj];
               if(change > target) {
                  newTrend = true;
               }
            } else {
               change = close[jj] - close[ii];
               if(change > target) {
                  newTrend = true;
               }
            }
            
            if(newTrend) {
               // Change in state
               for(int kk = jj + 1; kk < ii; ++kk) {
                  indicator[kk] = 1;
               }
               indicator[ii] = -1;
               age[ii] = 0;
               inflections[ii] = close[ii];
               state = -1;
               jj = ii;
               targets[ii] = changes[jj];
               target = changes[jj];
            } else {
               indicator[ii] = 1;
               age[ii] = age[ii-1] + 1;
               inflections[ii] = inflections[ii-1];
               corrections[ii] = change;
               targets[ii] = targets[ii-1];
            }
         }
      } else {
         if(close[ii] <= close[jj]) {
            indicator[ii] = -1;
            age[ii] = age[ii-1] + 1;
            inflections[ii] = inflections[ii-1];
            target = changes[ii];
            targets[ii] = changes[ii];
            jj = ii;
         } else {
            bool newTrend = false;
            double change;
            if(percent) {
               change = close[ii]/close[jj] - 1.0;
               if(change > target) {
                  newTrend = true;
               }
            } else {
               change = close[ii] - close[jj];
               if(change > target) {
                  newTrend = true;
               }
            }
            
            if(newTrend) {
               // Change in state
               for(int kk = jj + 1; kk < ii; ++kk) {
                  indicator[kk] = -1;
               }
               indicator[ii] = 1;
               age[ii] = 0;
               inflections[ii] = close[ii];
               state = 1;
               jj = ii;
               target = changes[jj];
               targets[ii] = target;
            } else {
               indicator[ii] = -1;
               age[ii] = age[ii-1] + 1;
               inflections[ii] = inflections[ii-1];
               corrections[ii] = change;
               targets[ii] = targets[ii-1];
            }
         }
      }
   }
}

// [[Rcpp::export("zig.zag.interface")]]
Rcpp::List zigZagInterface(SEXP pricesIn, SEXP changesIn, bool percent)
{
   std::vector<double> prices = Rcpp::as<std::vector<double> >(pricesIn);
   std::vector<double> changes = Rcpp::as<std::vector<double> >(changesIn);
   
   std::vector<int> indicator;
   std::vector<double> inflections;
   std::vector<double> corrections;
   std::vector<double> targets;
   std::vector<int> age;
   
   zigZag(prices, changes, percent, indicator, inflections, targets, corrections, age);
   
   return Rcpp::List::create(
               Rcpp::Named("indicator") = Rcpp::IntegerVector(indicator.begin(), indicator.end()),
               Rcpp::Named("inflections") = Rcpp::NumericVector(inflections.begin(), inflections.end()),
               Rcpp::Named("targets") = Rcpp::NumericVector(targets.begin(), targets.end()),
               Rcpp::Named("corrections") = Rcpp::NumericVector(corrections.begin(), corrections.end()),
               Rcpp::Named("age") = Rcpp::IntegerVector(age.begin(), age.end()));
}