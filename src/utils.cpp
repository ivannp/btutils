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

void locf(std::vector<double> & v, double value) {
   if(!isNA(value)) {
      for(std::vector<double>::size_type ii = 1; ii < v.size(); ++ii) {
         if(!isNA(v[ii-1]) && v[ii] == value) v[ii] = v[ii-1];
      }
   } else {
      // na.locf behaviour
      for(std::vector<double>::size_type ii = 1; ii < v.size(); ++ii) {
         if(isNA(v[ii]) && !isNA(v[ii-1])) v[ii] = v[ii-1];
      }
   }
}

// [[Rcpp::export("locf.interface")]]
Rcpp::NumericVector locfInterface(SEXP vin, double value)
{
   std::vector<double> v = Rcpp::as< std::vector<double> >(vin);
   locf(v, value);

   return Rcpp::NumericVector(v.begin(), v.end());
}

// [[Rcpp::export("leading.nas.interface")]]
double leadingNAs(SEXP vin)
{
   std::vector<double> v = Rcpp::as< std::vector<double> >(vin);
   std::vector<double>::size_type ii = 0;
   while(ii < v.size() && isNA(v[ii])) ++ii;

   return ii;
}

void laguerreFilter(const std::vector<double> & prices, double gamma, std::vector<double> & out)
{
   out.resize(prices.size());

   std::vector<double> l0(prices.size(), 0.0);
   std::vector<double> l1(prices.size(), 0.0);
   std::vector<double> l2(prices.size(), 0.0);
   std::vector<double> l3(prices.size(), 0.0);
   
   for(int jj = 1; jj < 4; ++jj) l0[jj] = (1.0 - gamma)*prices[jj] + gamma*l0[jj-1];
   for(int jj = 2; jj < 4; ++jj) l1[jj] = -gamma*l0[jj] + l0[jj-1] + gamma*l1[jj-1];
   l2[3] = -gamma*l1[3] + l1[2] + gamma*l2[2];
   
   for(std::vector<double>::size_type jj = 4; jj < prices.size(); ++jj) {
      l0[jj] = (1.0 - gamma)*prices[jj] + gamma*l0[jj-1];
      l1[jj] = -gamma*l0[jj] + l0[jj-1] + gamma*l1[jj-1];
      l2[jj] = -gamma*l1[jj] + l1[jj-1] + gamma*l2[jj-1];
      l3[jj] = -gamma*l2[jj] + l2[jj-1] + gamma*l3[jj-1];
   }
   
   for(std::vector<double>::size_type jj = 0; jj < prices.size(); ++jj) out[jj] = (l0[jj] + 2.0*l1[jj] + 2.0*l2[jj] + l3[jj]) / 6.0;
}

// [[Rcpp::export("laguerre.filter.interface")]]
Rcpp::NumericVector laguerreFilterInterface(SEXP vin, double gamma)
{
   std::vector<double> v = Rcpp::as< std::vector<double> >(vin);
   std::vector<double> vout;
   
   laguerreFilter(v, gamma, vout);

   return Rcpp::NumericVector(vout.begin(), vout.end());
}

void laguerreRSI(const std::vector<double> & prices, double gamma, std::vector<double> & rsi)
{
   rsi.resize(prices.size());

   std::vector<double> l0(prices.size(), 0.0);
   std::vector<double> l1(prices.size(), 0.0);
   std::vector<double> l2(prices.size(), 0.0);
   std::vector<double> l3(prices.size(), 0.0);
   
   for(int jj = 1; jj < 4; ++jj) l0[jj] = (1.0 - gamma)*prices[jj] + gamma*l0[jj-1];
   for(int jj = 2; jj < 4; ++jj) l1[jj] = -gamma*l0[jj] + l0[jj-1] + gamma*l1[jj-1];
   l2[3] = -gamma*l1[3] + l1[2] + gamma*l2[2];
   
   for(std::vector<double>::size_type jj = 4; jj < prices.size(); ++jj) {
      l0[jj] = (1.0 - gamma)*prices[jj] + gamma*l0[jj-1];
      l1[jj] = -gamma*l0[jj] + l0[jj-1] + gamma*l1[jj-1];
      l2[jj] = -gamma*l1[jj] + l1[jj-1] + gamma*l2[jj-1];
      l3[jj] = -gamma*l2[jj] + l2[jj-1] + gamma*l3[jj-1];
   }
   
   for(std::vector<double>::size_type jj = 0; jj < prices.size(); ++jj) {
      double cu = 0.0;
      double cd = 0.0;

      if(l0[jj] > l1[jj]) cu = l0[jj] - l1[jj];
      else cd = l1[jj] - l0[jj];

      if(l1[jj] > l2[jj]) cu += l1[jj] - l2[jj];
      else cd += l2[jj] - l1[jj];

      if(l2[jj] > l3[jj]) cu += l2[jj] - l3[jj];
      else cd += l3[jj] - l2[jj];
      
      if((cu + cd) > 0.0) rsi[jj] = cu / (cu + cd);
   }
}

// [[Rcpp::export("laguerre.rsi.interface")]]
Rcpp::NumericVector laguerreRSIInterface(SEXP vin, double gamma)
{
   std::vector<double> v = Rcpp::as< std::vector<double> >(vin);
   std::vector<double> rsi;
   
   laguerreRSI(v, gamma, rsi);

   return Rcpp::NumericVector(rsi.begin(), rsi.end());
}