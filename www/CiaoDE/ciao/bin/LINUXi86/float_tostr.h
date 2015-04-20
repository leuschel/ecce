/***************************************************************************
                          float_tostr.h  -  description
                             -------------------
    begin                : Wed Jun 4 2003
    copyright            : (C) 2003 by Edison Mera Menéndez
    email                : edison@clip.dia.fi.upm.es
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef FLOAT_TOSTR_H
#define FLOAT_TOSTR_H

#include "float_consts.h"

#define IEEE754_NEGATIVE(x) \
  (((((unsigned long *)(&x))[IEEE754_INDEX_NEGATIVE]) & IEEE754_MASK_NEGATIVE) >> IEEE754_SHIFT_NEGATIVE)
#define IEEE754_EXPONENT(x) \
  (((((unsigned long *)(&x))[IEEE754_INDEX_EXPONENT]) & IEEE754_MASK_EXPONENT) >> IEEE754_SHIFT_EXPONENT)
#define IEEE754_MANTISSA0(x) \
  (((((unsigned long *)(&x))[IEEE754_INDEX_MANTISSA0]) & IEEE754_MASK_MANTISSA0) >> IEEE754_SHIFT_MANTISSA0)
#define IEEE754_MANTISSA1(x) \
  (((((unsigned long *)(&x))[IEEE754_INDEX_MANTISSA1]) & IEEE754_MASK_MANTISSA1) >> IEEE754_SHIFT_MANTISSA1)

#define IEEE854_NEGATIVE(x) \
  (((((unsigned long *)(&x))[IEEE854_INDEX_NEGATIVE]) & IEEE854_MASK_NEGATIVE) >> IEEE854_SHIFT_NEGATIVE)
#define IEEE854_EXPONENT(x) \
  (((((unsigned long *)(&x))[IEEE854_INDEX_EXPONENT]) & IEEE854_MASK_EXPONENT) >> IEEE854_SHIFT_EXPONENT)
#define IEEE854_MANTISSA0(x) \
  ((((((unsigned long *)(&x))[IEEE854_INDEX_MANTISSA0_0]) & IEEE854_MASK_MANTISSA0_0) << IEEE854_SPLIT_MANTISSA0_0) \
  | (((((unsigned long *)(&x))[IEEE854_INDEX_MANTISSA0_1]) & IEEE854_MASK_MANTISSA0_1) >> IEEE854_SHIFT_MANTISSA0_1))
#define IEEE854_MANTISSA1(x) \
  ((((((unsigned long *)(&x))[IEEE854_INDEX_MANTISSA1_0]) & IEEE854_MASK_MANTISSA1_0) << IEEE854_SPLIT_MANTISSA1_0) \
  | (((((unsigned long *)(&x))[IEEE854_INDEX_MANTISSA1_1]) & IEEE854_MASK_MANTISSA1_1) >> IEEE854_SHIFT_MANTISSA1_1))

char * float_to_string(char* buffer, int precision, char format, double x, int base);

#endif /*FLOAT_TOSTR_H*/
