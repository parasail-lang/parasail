pragma Ada_2012;
pragma Style_Checks (Off);
pragma Warnings ("U");

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

package x86_64_linux_gnu_bits_mathcalls_h is

  -- Prototype declarations for math functions; helper file for <math.h>.
  --   Copyright (C) 1996-2014 Free Software Foundation, Inc.
  --   This file is part of the GNU C Library.
  --   The GNU C Library is free software; you can redistribute it and/or
  --   modify it under the terms of the GNU Lesser General Public
  --   License as published by the Free Software Foundation; either
  --   version 2.1 of the License, or (at your option) any later version.
  --   The GNU C Library is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  --   Lesser General Public License for more details.
  --   You should have received a copy of the GNU Lesser General Public
  --   License along with the GNU C Library; if not, see
  --   <http://www.gnu.org/licenses/>.   

  -- NOTE: Because of the special way this file is used by <math.h>, this
  --   file must NOT be protected from multiple inclusion as header files
  --   usually are.
  --   This file provides prototype declarations for the math functions.
  --   Most functions are declared using the macro:
  --   __MATHCALL (NAME,[_r], (ARGS...));
  --   This means there is a function `NAME' returning `double' and a function
  --   `NAMEf' returning `float'.  Each place `_Mdouble_' appears in the
  --   prototype, that is actually `double' in the prototype for `NAME' and
  --   `float' in the prototype for `NAMEf'.  Reentrant variant functions are
  --   called `NAME_r' and `NAMEf_r'.
  --   Functions returning other types like `int' are declared using the macro:
  --   __MATHDECL (TYPE, NAME,[_r], (ARGS...));
  --   This is just like __MATHCALL but for a function returning `TYPE'
  --   instead of `_Mdouble_'.  In all of these cases, there is still
  --   both a `NAME' and a `NAMEf' that takes `float' arguments.
  --   Note that there must be no whitespace before the argument passed for
  --   NAME, to make token pasting work with -traditional.   

  -- Trigonometric functions.   
  -- Arc cosine of X.   
   function acos (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "acos";

   function acosf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "acosf";

   function acosl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:54
   with Import => True, 
        Convention => C, 
        External_Name => "acosl";

   --  skipped func __acos

   --  skipped func __acosf

   --  skipped func __acosl

  -- Arc sine of X.   
   function asin (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "asin";

   function asinf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "asinf";

   function asinl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:56
   with Import => True, 
        Convention => C, 
        External_Name => "asinl";

   --  skipped func __asin

   --  skipped func __asinf

   --  skipped func __asinl

  -- Arc tangent of X.   
   function atan (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "atan";

   function atanf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "atanf";

   function atanl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:58
   with Import => True, 
        Convention => C, 
        External_Name => "atanl";

   --  skipped func __atan

   --  skipped func __atanf

   --  skipped func __atanl

  -- Arc tangent of Y/X.   
   function atan2 (uu_y : double; uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "atan2";

   function atan2f (uu_y : float; uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "atan2f";

   function atan2l (uu_y : long_double; uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:60
   with Import => True, 
        Convention => C, 
        External_Name => "atan2l";

   --  skipped func __atan2

   --  skipped func __atan2f

   --  skipped func __atan2l

  -- Cosine of X.   
   function cos (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "cos";

   function cosf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "cosf";

   function cosl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:63
   with Import => True, 
        Convention => C, 
        External_Name => "cosl";

   --  skipped func __cos

   --  skipped func __cosf

   --  skipped func __cosl

  -- Sine of X.   
   function sin (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "sin";

   function sinf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "sinf";

   function sinl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:65
   with Import => True, 
        Convention => C, 
        External_Name => "sinl";

   --  skipped func __sin

   --  skipped func __sinf

   --  skipped func __sinl

  -- Tangent of X.   
   function tan (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "tan";

   function tanf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "tanf";

   function tanl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:67
   with Import => True, 
        Convention => C, 
        External_Name => "tanl";

   --  skipped func __tan

   --  skipped func __tanf

   --  skipped func __tanl

  -- Hyperbolic functions.   
  -- Hyperbolic cosine of X.   
   function cosh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "cosh";

   function coshf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "coshf";

   function coshl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:72
   with Import => True, 
        Convention => C, 
        External_Name => "coshl";

   --  skipped func __cosh

   --  skipped func __coshf

   --  skipped func __coshl

  -- Hyperbolic sine of X.   
   function sinh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "sinh";

   function sinhf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "sinhf";

   function sinhl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:74
   with Import => True, 
        Convention => C, 
        External_Name => "sinhl";

   --  skipped func __sinh

   --  skipped func __sinhf

   --  skipped func __sinhl

  -- Hyperbolic tangent of X.   
   function tanh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "tanh";

   function tanhf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "tanhf";

   function tanhl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:76
   with Import => True, 
        Convention => C, 
        External_Name => "tanhl";

   --  skipped func __tanh

   --  skipped func __tanhf

   --  skipped func __tanhl

  -- Cosine and sine of X.   
   procedure sincos
     (uu_x : double;
      uu_sinx : access double;
      uu_cosx : access double)  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "sincos";

   procedure sincosf
     (uu_x : float;
      uu_sinx : access float;
      uu_cosx : access float)  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "sincosf";

   procedure sincosl
     (uu_x : long_double;
      uu_sinx : access long_double;
      uu_cosx : access long_double)  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:81
   with Import => True, 
        Convention => C, 
        External_Name => "sincosl";

   --  skipped func __sincos

   --  skipped func __sincosf

   --  skipped func __sincosl

  -- Hyperbolic arc cosine of X.   
   function acosh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "acosh";

   function acoshf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "acoshf";

   function acoshl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:88
   with Import => True, 
        Convention => C, 
        External_Name => "acoshl";

   --  skipped func __acosh

   --  skipped func __acoshf

   --  skipped func __acoshl

  -- Hyperbolic arc sine of X.   
   function asinh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "asinh";

   function asinhf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "asinhf";

   function asinhl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:90
   with Import => True, 
        Convention => C, 
        External_Name => "asinhl";

   --  skipped func __asinh

   --  skipped func __asinhf

   --  skipped func __asinhl

  -- Hyperbolic arc tangent of X.   
   function atanh (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "atanh";

   function atanhf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "atanhf";

   function atanhl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:92
   with Import => True, 
        Convention => C, 
        External_Name => "atanhl";

   --  skipped func __atanh

   --  skipped func __atanhf

   --  skipped func __atanhl

  -- Exponential and logarithmic functions.   
  -- Exponential function of X.   
   function exp (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "exp";

   function expf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "expf";

   function expl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:100
   with Import => True, 
        Convention => C, 
        External_Name => "expl";

   --  skipped func __exp

   --  skipped func __expf

   --  skipped func __expl

  -- Break VALUE into a normalized fraction and an integral power of 2.   
   function frexp (uu_x : double; uu_exponent : access int) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "frexp";

   function frexpf (uu_x : float; uu_exponent : access int) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "frexpf";

   function frexpl (uu_x : long_double; uu_exponent : access int) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:103
   with Import => True, 
        Convention => C, 
        External_Name => "frexpl";

   --  skipped func __frexp

   --  skipped func __frexpf

   --  skipped func __frexpl

  -- X times (two to the EXP power).   
   function ldexp (uu_x : double; uu_exponent : int) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "ldexp";

   function ldexpf (uu_x : float; uu_exponent : int) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "ldexpf";

   function ldexpl (uu_x : long_double; uu_exponent : int) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:106
   with Import => True, 
        Convention => C, 
        External_Name => "ldexpl";

   --  skipped func __ldexp

   --  skipped func __ldexpf

   --  skipped func __ldexpl

  -- Natural logarithm of X.   
   function log (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "log";

   function logf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "logf";

   function logl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:109
   with Import => True, 
        Convention => C, 
        External_Name => "logl";

   --  skipped func __log

   --  skipped func __logf

   --  skipped func __logl

  -- Base-ten logarithm of X.   
   function log10 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "log10";

   function log10f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "log10f";

   function log10l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:112
   with Import => True, 
        Convention => C, 
        External_Name => "log10l";

   --  skipped func __log10

   --  skipped func __log10f

   --  skipped func __log10l

  -- Break VALUE into integral and fractional parts.   
   function modf (uu_x : double; uu_iptr : access double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "modf";

   function modff (uu_x : float; uu_iptr : access float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "modff";

   function modfl (uu_x : long_double; uu_iptr : access long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:115
   with Import => True, 
        Convention => C, 
        External_Name => "modfl";

   --  skipped func __modf

   --  skipped func __modff

   --  skipped func __modfl

  -- A function missing in all standards: compute exponent to base ten.   
   function exp10 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "exp10";

   function exp10f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "exp10f";

   function exp10l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:120
   with Import => True, 
        Convention => C, 
        External_Name => "exp10l";

   --  skipped func __exp10

   --  skipped func __exp10f

   --  skipped func __exp10l

  -- Another name occasionally used.   
   function pow10 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "pow10";

   function pow10f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "pow10f";

   function pow10l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:122
   with Import => True, 
        Convention => C, 
        External_Name => "pow10l";

   --  skipped func __pow10

   --  skipped func __pow10f

   --  skipped func __pow10l

  -- Return exp(X) - 1.   
   function expm1 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "expm1";

   function expm1f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "expm1f";

   function expm1l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:128
   with Import => True, 
        Convention => C, 
        External_Name => "expm1l";

   --  skipped func __expm1

   --  skipped func __expm1f

   --  skipped func __expm1l

  -- Return log(1 + X).   
   function log1p (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "log1p";

   function log1pf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "log1pf";

   function log1pl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:131
   with Import => True, 
        Convention => C, 
        External_Name => "log1pl";

   --  skipped func __log1p

   --  skipped func __log1pf

   --  skipped func __log1pl

  -- Return the base 2 signed integral exponent of X.   
   function logb (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "logb";

   function logbf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "logbf";

   function logbl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:134
   with Import => True, 
        Convention => C, 
        External_Name => "logbl";

   --  skipped func __logb

   --  skipped func __logbf

   --  skipped func __logbl

  -- Compute base-2 exponential of X.   
   function exp2 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "exp2";

   function exp2f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "exp2f";

   function exp2l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:141
   with Import => True, 
        Convention => C, 
        External_Name => "exp2l";

   --  skipped func __exp2

   --  skipped func __exp2f

   --  skipped func __exp2l

  -- Compute base-2 logarithm of X.   
   function log2 (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "log2";

   function log2f (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "log2f";

   function log2l (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:144
   with Import => True, 
        Convention => C, 
        External_Name => "log2l";

   --  skipped func __log2

   --  skipped func __log2f

   --  skipped func __log2l

  -- Power functions.   
  -- Return X to the Y power.   
   function pow (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "pow";

   function powf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "powf";

   function powl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:153
   with Import => True, 
        Convention => C, 
        External_Name => "powl";

   --  skipped func __pow

   --  skipped func __powf

   --  skipped func __powl

  -- Return the square root of X.   
   function sqrt (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "sqrt";

   function sqrtf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "sqrtf";

   function sqrtl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:156
   with Import => True, 
        Convention => C, 
        External_Name => "sqrtl";

   --  skipped func __sqrt

   --  skipped func __sqrtf

   --  skipped func __sqrtl

  -- Return `sqrt(X*X + Y*Y)'.   
   function hypot (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "hypot";

   function hypotf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "hypotf";

   function hypotl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:162
   with Import => True, 
        Convention => C, 
        External_Name => "hypotl";

   --  skipped func __hypot

   --  skipped func __hypotf

   --  skipped func __hypotl

  -- Return the cube root of X.   
   function cbrt (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:169
   with Import => True, 
        Convention => C, 
        External_Name => "cbrt";

   function cbrtf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:169
   with Import => True, 
        Convention => C, 
        External_Name => "cbrtf";

   function cbrtl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:169
   with Import => True, 
        Convention => C, 
        External_Name => "cbrtl";

   --  skipped func __cbrt

   --  skipped func __cbrtf

   --  skipped func __cbrtl

  -- Nearest integer, absolute value, and remainder functions.   
  -- Smallest integral value not less than X.   
   function ceil (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "ceil";

   function ceilf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "ceilf";

   function ceill (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:178
   with Import => True, 
        Convention => C, 
        External_Name => "ceill";

   --  skipped func __ceil

   --  skipped func __ceilf

   --  skipped func __ceill

  -- Absolute value of X.   
   function fabs (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "fabs";

   function fabsf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "fabsf";

   function fabsl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:181
   with Import => True, 
        Convention => C, 
        External_Name => "fabsl";

   --  skipped func __fabs

   --  skipped func __fabsf

   --  skipped func __fabsl

  -- Largest integer not greater than X.   
   function floor (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "floor";

   function floorf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "floorf";

   function floorl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:184
   with Import => True, 
        Convention => C, 
        External_Name => "floorl";

   --  skipped func __floor

   --  skipped func __floorf

   --  skipped func __floorl

  -- Floating-point modulo remainder of X/Y.   
   function fmod (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "fmod";

   function fmodf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "fmodf";

   function fmodl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:187
   with Import => True, 
        Convention => C, 
        External_Name => "fmodl";

   --  skipped func __fmod

   --  skipped func __fmodf

   --  skipped func __fmodl

  -- Return 0 if VALUE is finite or NaN, +1 if it
  --   is +Infinity, -1 if it is -Infinity.   

   --  skipped func __isinf

   --  skipped func __isinff

   --  skipped func __isinfl

  -- Return nonzero if VALUE is finite and not NaN.   
   --  skipped func __finite

   --  skipped func __finitef

   --  skipped func __finitel

  -- Return 0 if VALUE is finite or NaN, +1 if it
  --   is +Infinity, -1 if it is -Infinity.   

   function isinf (uu_value : double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "isinf";

   function isinff (uu_value : float) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "isinff";

   function isinfl (uu_value : long_double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:201
   with Import => True, 
        Convention => C, 
        External_Name => "isinfl";

  -- Return nonzero if VALUE is finite and not NaN.   
   function finite (uu_value : double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "finite";

   function finitef (uu_value : float) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "finitef";

   function finitel (uu_value : long_double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:204
   with Import => True, 
        Convention => C, 
        External_Name => "finitel";

  -- Return the remainder of X/Y.   
   function drem (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:207
   with Import => True, 
        Convention => C, 
        External_Name => "drem";

   function dremf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:207
   with Import => True, 
        Convention => C, 
        External_Name => "dremf";

   function dreml (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:207
   with Import => True, 
        Convention => C, 
        External_Name => "dreml";

   --  skipped func __drem

   --  skipped func __dremf

   --  skipped func __dreml

  -- Return the fractional part of X after dividing out `ilogb (X)'.   
   function significand (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "significand";

   function significandf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "significandf";

   function significandl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:211
   with Import => True, 
        Convention => C, 
        External_Name => "significandl";

   --  skipped func __significand

   --  skipped func __significandf

   --  skipped func __significandl

  -- Return X with its signed changed to Y's.   
   function copysign (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "copysign";

   function copysignf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "copysignf";

   function copysignl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:217
   with Import => True, 
        Convention => C, 
        External_Name => "copysignl";

   --  skipped func __copysign

   --  skipped func __copysignf

   --  skipped func __copysignl

  -- Return representation of qNaN for double type.   
   function nan (uu_tagb : Interfaces.C.Strings.chars_ptr) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "nan";

   function nanf (uu_tagb : Interfaces.C.Strings.chars_ptr) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "nanf";

   function nanl (uu_tagb : Interfaces.C.Strings.chars_ptr) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:224
   with Import => True, 
        Convention => C, 
        External_Name => "nanl";

   --  skipped func __nan

   --  skipped func __nanf

   --  skipped func __nanl

  -- Return nonzero if VALUE is not a number.   
   --  skipped func __isnan

   --  skipped func __isnanf

   --  skipped func __isnanl

  -- Return nonzero if VALUE is not a number.   
   function isnan (uu_value : double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:234
   with Import => True, 
        Convention => C, 
        External_Name => "isnan";

   function isnanf (uu_value : float) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:234
   with Import => True, 
        Convention => C, 
        External_Name => "isnanf";

   function isnanl (uu_value : long_double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:234
   with Import => True, 
        Convention => C, 
        External_Name => "isnanl";

  -- Bessel functions.   
   function j0 (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:237
   with Import => True, 
        Convention => C, 
        External_Name => "j0";

   function j0f (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:237
   with Import => True, 
        Convention => C, 
        External_Name => "j0f";

   function j0l (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:237
   with Import => True, 
        Convention => C, 
        External_Name => "j0l";

   --  skipped func __j0

   --  skipped func __j0f

   --  skipped func __j0l

   function j1 (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "j1";

   function j1f (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "j1f";

   function j1l (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:238
   with Import => True, 
        Convention => C, 
        External_Name => "j1l";

   --  skipped func __j1

   --  skipped func __j1f

   --  skipped func __j1l

   function jn (arg1 : int; arg2 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "jn";

   function jnf (arg1 : int; arg2 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "jnf";

   function jnl (arg1 : int; arg2 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:239
   with Import => True, 
        Convention => C, 
        External_Name => "jnl";

   --  skipped func __jn

   --  skipped func __jnf

   --  skipped func __jnl

   function y0 (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "y0";

   function y0f (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "y0f";

   function y0l (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:240
   with Import => True, 
        Convention => C, 
        External_Name => "y0l";

   --  skipped func __y0

   --  skipped func __y0f

   --  skipped func __y0l

   function y1 (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "y1";

   function y1f (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "y1f";

   function y1l (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "y1l";

   --  skipped func __y1

   --  skipped func __y1f

   --  skipped func __y1l

   function yn (arg1 : int; arg2 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:242
   with Import => True, 
        Convention => C, 
        External_Name => "yn";

   function ynf (arg1 : int; arg2 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:242
   with Import => True, 
        Convention => C, 
        External_Name => "ynf";

   function ynl (arg1 : int; arg2 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:242
   with Import => True, 
        Convention => C, 
        External_Name => "ynl";

   --  skipped func __yn

   --  skipped func __ynf

   --  skipped func __ynl

  -- Error and gamma functions.   
   function erf (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "erf";

   function erff (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "erff";

   function erfl (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "erfl";

   --  skipped func __erf

   --  skipped func __erff

   --  skipped func __erfl

   function erfc (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "erfc";

   function erfcf (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "erfcf";

   function erfcl (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:250
   with Import => True, 
        Convention => C, 
        External_Name => "erfcl";

   --  skipped func __erfc

   --  skipped func __erfcf

   --  skipped func __erfcl

   function lgamma (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:251
   with Import => True, 
        Convention => C, 
        External_Name => "lgamma";

   function lgammaf (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:251
   with Import => True, 
        Convention => C, 
        External_Name => "lgammaf";

   function lgammal (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:251
   with Import => True, 
        Convention => C, 
        External_Name => "lgammal";

   --  skipped func __lgamma

   --  skipped func __lgammaf

   --  skipped func __lgammal

  -- True gamma function.   
   function tgamma (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:258
   with Import => True, 
        Convention => C, 
        External_Name => "tgamma";

   function tgammaf (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:258
   with Import => True, 
        Convention => C, 
        External_Name => "tgammaf";

   function tgammal (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:258
   with Import => True, 
        Convention => C, 
        External_Name => "tgammal";

   --  skipped func __tgamma

   --  skipped func __tgammaf

   --  skipped func __tgammal

  -- Obsolete alias for `lgamma'.   
   function gamma (arg1 : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:264
   with Import => True, 
        Convention => C, 
        External_Name => "gamma";

   function gammaf (arg1 : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:264
   with Import => True, 
        Convention => C, 
        External_Name => "gammaf";

   function gammal (arg1 : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:264
   with Import => True, 
        Convention => C, 
        External_Name => "gammal";

   --  skipped func __gamma

   --  skipped func __gammaf

   --  skipped func __gammal

  -- Reentrant version of lgamma.  This function uses the global variable
  --   `signgam'.  The reentrant version instead takes a pointer and stores
  --   the value through it.   

   function lgamma_r (arg1 : double; uu_signgamp : access int) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "lgamma_r";

   function lgammaf_r (arg1 : float; uu_signgamp : access int) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "lgammaf_r";

   function lgammal_r (arg1 : long_double; uu_signgamp : access int) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:271
   with Import => True, 
        Convention => C, 
        External_Name => "lgammal_r";

   --  skipped func __lgamma_r

   --  skipped func __lgammaf_r

   --  skipped func __lgammal_r

  -- Return the integer nearest X in the direction of the
  --   prevailing rounding mode.   

   function rint (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "rint";

   function rintf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "rintf";

   function rintl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:279
   with Import => True, 
        Convention => C, 
        External_Name => "rintl";

   --  skipped func __rint

   --  skipped func __rintf

   --  skipped func __rintl

  -- Return X + epsilon if X < Y, X - epsilon if X > Y.   
   function nextafter (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "nextafter";

   function nextafterf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "nextafterf";

   function nextafterl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:282
   with Import => True, 
        Convention => C, 
        External_Name => "nextafterl";

   --  skipped func __nextafter

   --  skipped func __nextafterf

   --  skipped func __nextafterl

   function nexttoward (uu_x : double; uu_y : long_double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "nexttoward";

   function nexttowardf (uu_x : float; uu_y : long_double) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "nexttowardf";

   function nexttowardl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:284
   with Import => True, 
        Convention => C, 
        External_Name => "nexttowardl";

   --  skipped func __nexttoward

   --  skipped func __nexttowardf

   --  skipped func __nexttowardl

  -- Return the remainder of integer divison X / Y with infinite precision.   
   function remainder (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:288
   with Import => True, 
        Convention => C, 
        External_Name => "remainder";

   function remainderf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:288
   with Import => True, 
        Convention => C, 
        External_Name => "remainderf";

   function remainderl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:288
   with Import => True, 
        Convention => C, 
        External_Name => "remainderl";

   --  skipped func __remainder

   --  skipped func __remainderf

   --  skipped func __remainderl

  -- Return X times (2 to the Nth power).   
   function scalbn (uu_x : double; uu_n : int) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "scalbn";

   function scalbnf (uu_x : float; uu_n : int) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "scalbnf";

   function scalbnl (uu_x : long_double; uu_n : int) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:292
   with Import => True, 
        Convention => C, 
        External_Name => "scalbnl";

   --  skipped func __scalbn

   --  skipped func __scalbnf

   --  skipped func __scalbnl

  -- Return the binary exponent of X, which must be nonzero.   
   function ilogb (uu_x : double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "ilogb";

   function ilogbf (uu_x : float) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "ilogbf";

   function ilogbl (uu_x : long_double) return int  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:296
   with Import => True, 
        Convention => C, 
        External_Name => "ilogbl";

   --  skipped func __ilogb

   --  skipped func __ilogbf

   --  skipped func __ilogbl

  -- Return X times (2 to the Nth power).   
   function scalbln (uu_x : double; uu_n : long) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "scalbln";

   function scalblnf (uu_x : float; uu_n : long) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "scalblnf";

   function scalblnl (uu_x : long_double; uu_n : long) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:301
   with Import => True, 
        Convention => C, 
        External_Name => "scalblnl";

   --  skipped func __scalbln

   --  skipped func __scalblnf

   --  skipped func __scalblnl

  -- Round X to integral value in floating-point format using current
  --   rounding direction, but do not raise inexact exception.   

   function nearbyint (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "nearbyint";

   function nearbyintf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "nearbyintf";

   function nearbyintl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:305
   with Import => True, 
        Convention => C, 
        External_Name => "nearbyintl";

   --  skipped func __nearbyint

   --  skipped func __nearbyintf

   --  skipped func __nearbyintl

  -- Round X to nearest integral value, rounding halfway cases away from
  --   zero.   

   function round (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "round";

   function roundf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "roundf";

   function roundl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:309
   with Import => True, 
        Convention => C, 
        External_Name => "roundl";

   --  skipped func __round

   --  skipped func __roundf

   --  skipped func __roundl

  -- Round X to the integral value in floating-point format nearest but
  --   not larger in magnitude.   

   function trunc (uu_x : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "trunc";

   function truncf (uu_x : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "truncf";

   function truncl (uu_x : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:313
   with Import => True, 
        Convention => C, 
        External_Name => "truncl";

   --  skipped func __trunc

   --  skipped func __truncf

   --  skipped func __truncl

  -- Compute remainder of X and Y and put in *QUO a value with sign of x/y
  --   and magnitude congruent `mod 2^n' to the magnitude of the integral
  --   quotient x/y, with n >= 3.   

   function remquo
     (uu_x : double;
      uu_y : double;
      uu_quo : access int) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "remquo";

   function remquof
     (uu_x : float;
      uu_y : float;
      uu_quo : access int) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "remquof";

   function remquol
     (uu_x : long_double;
      uu_y : long_double;
      uu_quo : access int) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:318
   with Import => True, 
        Convention => C, 
        External_Name => "remquol";

   --  skipped func __remquo

   --  skipped func __remquof

   --  skipped func __remquol

  -- Conversion functions.   
  -- Round X to nearest integral value according to current rounding
  --   direction.   

   function lrint (uu_x : double) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "lrint";

   function lrintf (uu_x : float) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "lrintf";

   function lrintl (uu_x : long_double) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "lrintl";

   --  skipped func __lrint

   --  skipped func __lrintf

   --  skipped func __lrintl

   function llrint (uu_x : double) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "llrint";

   function llrintf (uu_x : float) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "llrintf";

   function llrintl (uu_x : long_double) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:327
   with Import => True, 
        Convention => C, 
        External_Name => "llrintl";

   --  skipped func __llrint

   --  skipped func __llrintf

   --  skipped func __llrintl

  -- Round X to nearest integral value, rounding halfway cases away from
  --   zero.   

   function lround (uu_x : double) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:331
   with Import => True, 
        Convention => C, 
        External_Name => "lround";

   function lroundf (uu_x : float) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:331
   with Import => True, 
        Convention => C, 
        External_Name => "lroundf";

   function lroundl (uu_x : long_double) return long  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:331
   with Import => True, 
        Convention => C, 
        External_Name => "lroundl";

   --  skipped func __lround

   --  skipped func __lroundf

   --  skipped func __lroundl

   function llround (uu_x : double) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "llround";

   function llroundf (uu_x : float) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "llroundf";

   function llroundl (uu_x : long_double) return Long_Long_Integer  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:333
   with Import => True, 
        Convention => C, 
        External_Name => "llroundl";

   --  skipped func __llround

   --  skipped func __llroundf

   --  skipped func __llroundl

  -- Return positive difference between X and Y.   
   function fdim (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "fdim";

   function fdimf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "fdimf";

   function fdiml (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:337
   with Import => True, 
        Convention => C, 
        External_Name => "fdiml";

   --  skipped func __fdim

   --  skipped func __fdimf

   --  skipped func __fdiml

  -- Return maximum numeric value from X and Y.   
   function fmax (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "fmax";

   function fmaxf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "fmaxf";

   function fmaxl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:340
   with Import => True, 
        Convention => C, 
        External_Name => "fmaxl";

   --  skipped func __fmax

   --  skipped func __fmaxf

   --  skipped func __fmaxl

  -- Return minimum numeric value from X and Y.   
   function fmin (uu_x : double; uu_y : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "fmin";

   function fminf (uu_x : float; uu_y : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "fminf";

   function fminl (uu_x : long_double; uu_y : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "fminl";

   --  skipped func __fmin

   --  skipped func __fminf

   --  skipped func __fminl

  -- Classify given number.   
   --  skipped func __fpclassify

   --  skipped func __fpclassifyf

   --  skipped func __fpclassifyl

  -- Test for negative number.   
   --  skipped func __signbit

   --  skipped func __signbitf

   --  skipped func __signbitl

  -- Multiply-add function computed as a ternary operation.   
   function fma
     (uu_x : double;
      uu_y : double;
      uu_z : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "fma";

   function fmaf
     (uu_x : float;
      uu_y : float;
      uu_z : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "fmaf";

   function fmal
     (uu_x : long_double;
      uu_y : long_double;
      uu_z : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:356
   with Import => True, 
        Convention => C, 
        External_Name => "fmal";

   --  skipped func __fma

   --  skipped func __fmaf

   --  skipped func __fmal

  -- Test for signaling NaN.   
   --  skipped func __issignaling

   --  skipped func __issignalingf

   --  skipped func __issignalingl

  -- Return X times (2 to the Nth power).   
   function scalb (uu_x : double; uu_n : double) return double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "scalb";

   function scalbf (uu_x : float; uu_n : float) return float  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "scalbf";

   function scalbl (uu_x : long_double; uu_n : long_double) return long_double  -- /usr/include/x86_64-linux-gnu/bits/mathcalls.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "scalbl";

   --  skipped func __scalb

   --  skipped func __scalbf

   --  skipped func __scalbl

end x86_64_linux_gnu_bits_mathcalls_h;
