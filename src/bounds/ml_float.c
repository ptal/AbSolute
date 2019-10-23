/*
   Copyright 2011 Antoine Mine

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
*/

// C implementation for float.ml module.

#include <math.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <fenv.h>

int curr_method = -1;

value ml_float_restore(value dummy)
{
  if (curr_method != -1){
    fesetround(curr_method);
  };
  CAMLparam0();
  CAMLreturn(Val_unit);
}

value ml_float_init(value dummy)
{
  curr_method = fegetround();
  CAMLparam0();
  fesetround(FE_UPWARD);
  CAMLreturn(Val_unit);
}
