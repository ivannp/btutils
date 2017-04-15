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

#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#include <Rcpp.h>

// This needs to be changed if the c++ code is used outside R.
// Is it better to use !R_finite() instead of R_IsNA()?
inline bool isNA(double d) { return R_IsNA(d); }

inline double roundAny(double d, double accuracy)
{
   return ::round(d/accuracy)*accuracy;
}

inline double floorAny(double d, double accuracy)
{
   return std::floor(d/accuracy)*accuracy;
}

inline double ceilAny(double d, double accuracy)
{
   return std::ceil(d/accuracy)*accuracy;
}

template <typename T> inline int sign(T t) {
   return (T(0) < t) - (t < T(0));
}

#endif // COMMON_H_INCLUDED