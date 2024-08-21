/**
  Implementation of Z module.

  This version uses LibTomMath instead of GMP / MPIR.
  Not all functions are supported.
  Requires LibTomMath 1.2.0.

  This file is part of the Zarith library
  http://forge.ocamlcore.org/projects/zarith .
  It is distributed under LGPL 2 licensing, with static linking exception.
  See the LICENSE file included in the distribution.

  Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
  Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
  a joint laboratory by:
  CNRS (Centre national de la recherche scientifique, France),
  ENS (École normale supérieure, Paris, France),
  INRIA Rocquencourt (Institut national de recherche en informatique, France).

*/


/*---------------------------------------------------
  INCLUDES
  ---------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <limits.h>

#include <tommath.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/callback.h>
#include <caml/intext.h>
#include <caml/hash.h>

#define inline __inline

#ifdef _MSC_VER
#include <float.h>
#include <intrin.h>
#endif

/* The "__has_builtin" special macro from Clang */
#ifdef __has_builtin
#define HAS_BUILTIN(x) __has_builtin(x)
#else
#define HAS_BUILTIN(x) 0
#endif

/* Whether the fast path (arguments and result are small integers)
   has already be handled in OCaml, so that there is no need to
   re-test for it in C functions.
   Applies to: neg, abs, add, sub, mul, div, rem, succ, pred,
   logand, logor, logxor, lognot, shifts, divexact.
*/
#define Z_FAST_PATH_IN_OCAML 1


/*---------------------------------------------------
  DATA STRUCTURES
  ---------------------------------------------------*/


/* bounds of an Ocaml int */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_INT       0x3fffffffffffffff
#define Z_MIN_INT     (-0x4000000000000000)
#else
#define Z_MAX_INT       0x3fffffff
#define Z_MIN_INT     (-0x40000000)
#endif
#define Z_FITS_INT(v)  ((v) >= Z_MIN_INT && (v) <= Z_MAX_INT)

static mp_int z_max_int,    z_min_int;
static mp_int z_max_intnat, z_min_intnat;
static mp_int z_max_int32,  z_min_int32;
static mp_int z_max_int64,  z_min_int64;
static mp_int z_max_intnat_unsigned;
static mp_int z_max_int32_unsigned;
static mp_int z_max_int64_unsigned;

/* greatest/smallest double that can fit in an int */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_INT_FL    0x3ffffffffffffe00
#define Z_MIN_INT_FL    (-0x4000000000000000)
#else
#define Z_MAX_INT_FL    Z_MAX_INT
#define Z_MIN_INT_FL    Z_MIN_INT
#endif

/* safe bounds to avoid overflow in multiplication */
#ifdef ARCH_SIXTYFOUR
#define Z_MAX_HINT  0x3fffffff
#else
#define Z_MAX_HINT  0x3fff
#endif
#define Z_MIN_HINT (-Z_MAX_HINT)
#define Z_FITS_HINT(v) ((v) >= Z_MIN_HINT && (v) <= Z_MAX_HINT)

/* hi bit of OCaml int32, int64 & nativeint */
#define Z_HI_INT32    0x80000000
#define Z_HI_UINT32   0x100000000LL
#define Z_HI_INT64    0x8000000000000000LL
#ifdef ARCH_SIXTYFOUR
#define Z_HI_INTNAT   Z_HI_INT64
#define Z_HI_INT      0x4000000000000000
#define Z_INTNAT_BITS 64
#else
#define Z_HI_INTNAT   Z_HI_INT32
#define Z_HI_INT      0x40000000
#define Z_INTNAT_BITS 32
#endif

/* safe bounds for the length of a base n string fitting in a native
   int. Defined as the result of (n - 2) log_base(2) with n = 64 or
   32.
*/
#ifdef ARCH_SIXTYFOUR
#define Z_BASE16_LENGTH_OP 15
#define Z_BASE10_LENGTH_OP 18
#define Z_BASE8_LENGTH_OP 20
#define Z_BASE2_LENGTH_OP 62
#else
#define Z_BASE16_LENGTH_OP 7
#define Z_BASE10_LENGTH_OP 9
#define Z_BASE8_LENGTH_OP 10
#define Z_BASE2_LENGTH_OP 30
#endif

#ifdef _MSC_VER
#define UNUSED_PARAM
#else
#define UNUSED_PARAM __attribute__((unused))
#endif

#define Z_MP(x) ((mp_int*)Data_custom_val((x)))
#define Z_SIGN(x) (Z_MP((x))->sign)
#define Z_LIMB(x) (Z_MP((x))->dp)

#define Z_ISZERO(x) (Is_long((x)) ? Long_val((x)) == 0 : mp_iszero(Z_MP((x))))
#define Z_ISNEG(x) (Is_long((x)) ? Long_val((x)) < 0 : mp_isneg(Z_MP((x))))


/*---------------------------------------------------
  UTILITIES
  ---------------------------------------------------*/


extern struct custom_operations ml_z_custom_ops;

static void ml_z_raise_overflow()
{
  caml_raise_constant(*caml_named_value("ml_z_overflow"));
}

#define ml_z_raise_divide_by_zero() \
  caml_raise_zero_divide()

#define ml_z_raise_out_of_memory() \
  caml_raise_out_of_memory()

static value ml_z_alloc()
{
  value v;
  v = caml_alloc_custom(&ml_z_custom_ops, sizeof(mp_int), 0, 1);
  if (mp_init(Z_MP(v)) != MP_OKAY)
    ml_z_raise_out_of_memory();
  return v;
}


#ifdef ARCH_SIXTYFOUR
#define MP_INIT_VALUE mp_init_i64
#else
#define MP_INIT_VALUE mp_init_i32
#endif

#define Z_DECL(arg)                             \
  const mp_int *mp_##arg;                       \
  mp_int mp_s_##arg                             \

#define Z_ARG(arg)                                                      \
  if (Is_long(arg)) {                                                   \
    mp_##arg = & mp_s_##arg;                                            \
    if (MP_INIT_VALUE((mp_int*)mp_##arg, Long_val(arg)) != MP_OKAY)     \
      ml_z_raise_out_of_memory();                                       \
  }                                                                     \
  else {                                                                \
    mp_##arg = Z_MP(arg);                                               \
  }

#define Z_REFRESH(arg)                          \
  if (!Is_long(arg))                            \
    mp_##arg = Z_MP(arg);

#define Z_END_ARG(arg)                          \
  if (Is_long(arg)) {                           \
    mp_clear((mp_int*)mp_##arg);                \
  }

static value ml_z_reduce(value r)
{
  if (mp_cmp(Z_MP(r), &z_min_int) >= 0 &&
      mp_cmp(Z_MP(r), &z_max_int) <= 0) {
    /* can be represented in a value, we free the mp_int */
    intnat x = mp_get_i64(Z_MP(r));
    mp_clear(Z_MP(r));
    return Val_long(x);
  }
  return r;
}


/*---------------------------------------------------
  CONVERSION FUNCTIONS
  ---------------------------------------------------*/

CAMLprim value ml_z_of_int(value v)
{
  return v;
}

CAMLprim value ml_z_of_nativeint(value v)
{
  intnat x;
  value r;
  x = Nativeint_val(v);
  if (Z_FITS_INT(x)) return Val_long(x);
  r = ml_z_alloc();
#ifdef ARCH_SIXTYFOUR
  mp_set_i64(Z_MP(r), x);
#else
  mp_set_i32(Z_MP(r), x);
#endif
  return r;
}

CAMLprim value ml_z_of_int32(value v)
{
  int32_t x;
  value r;
  x = Int32_val(v);
#ifdef ARCH_SIXTYFOUR
  return Val_long(x);
#else
  if (Z_FITS_INT(x)) return Val_long(x);
#endif
  r = ml_z_alloc();
  mp_set_i32(Z_MP(r), x);
  return r;
}

CAMLprim value ml_z_of_int64(value v)
{
  int64_t x;
  value r;
  x = Int64_val(v);
  if (Z_FITS_INT(x)) return Val_long(x);
  r = ml_z_alloc();
  mp_set_i64(Z_MP(r), x);
  return r;
}

CAMLprim value ml_z_of_float(value v)
{
  double x;
  value r;
  x = Double_val(v);
  if (x >= Z_MIN_INT_FL && x <= Z_MAX_INT_FL) return Val_long((intnat) x);
  r = ml_z_alloc();
  if (mp_set_double(Z_MP(r), x) != MP_OKAY) {
    mp_clear(Z_MP(r));
    ml_z_raise_overflow();
  }
  return r;
}

CAMLprim value ml_z_of_substring_base(value b, value v, value offset, value length)
{
  CAMLparam1(v);
  CAMLlocal1(r);
  intnat ofs = Long_val(offset);
  intnat len = Long_val(length);
  /* make sure the ofs/length make sense */
  if (ofs < 0
      || len < 0
      || (intnat)caml_string_length(v) < ofs + len)
    caml_invalid_argument("Z.of_substring_base: invalid offset or length");
  /* process the string */
  const char *d = String_val(v) + ofs;
  const char *end = d + len;
  ptrdiff_t i, j, sz;
  int sign = 0;
  intnat base = Long_val(b);
  /* We allow [d] to advance beyond [end] while parsing the prefix:
     sign, base, and/or leading zeros.
     This simplifies the code, and reading these locations is safe since
     we don't progress beyond a terminating null character.
     At the end of the prefix, if we ran past the end, we return 0.
  */
  /* get optional sign */
  if (*d == '-') { sign = 1; d++; }
  if (*d == '+') d++;
  /* get optional base */
  if (!base) {
    base = 10;
    if (*d == '0') {
      d++;
      if (*d == 'o' || *d == 'O') { base = 8; d++; }
      else if (*d == 'x' || *d == 'X') { base = 16; d++; }
      else if (*d == 'b' || *d == 'B') { base = 2; d++; }
      else {
        /* The leading zero is not part of a base prefix. This is an
           important distinction for the check below looking at
           leading underscore
         */
        d--; }
    }
  }
  if (base < 2 || base > 16)
    caml_invalid_argument("Z.of_substring_base: base must be between 2 and 16");
  /* we do not allow leading underscore */
  if (*d == '_')
    caml_invalid_argument("Z.of_substring_base: invalid digit");
  while (*d == '0' || *d == '_') d++;
  /* sz is the length of the substring that has not been consumed above. */
  sz = end - d;
  if (sz <= 0) {
    /* "+", "-", "0x" are parsed as 0. */
    r = Val_long(0);
  }
  /* Process common cases (fits into a native integer) */
  else if ((base == 10 && sz <= Z_BASE10_LENGTH_OP)
        || (base == 16 && sz <= Z_BASE16_LENGTH_OP)
        || (base == 8  && sz <= Z_BASE8_LENGTH_OP)
        || (base == 2  && sz <= Z_BASE2_LENGTH_OP)) {
      intnat ret = 0;
      for (i = 0; i < sz; i++) {
        int digit = 0;
        if (d[i] == '_') continue;
        if (d[i] >= '0' && d[i] <= '9') digit = d[i] - '0';
        else if (d[i] >= 'a' && d[i] <= 'f') digit = d[i] - 'a' + 10;
        else if (d[i] >= 'A' && d[i] <= 'F') digit = d[i] - 'A' + 10;
        else caml_invalid_argument("Z.of_substring_base: invalid digit");
        if (digit >= base)
          caml_invalid_argument("Z.of_substring_base: invalid digit");
        ret = ret * base + digit;
      }
      r = Val_long(ret * (sign ? -1 : 1));
  } else {
    /* copy the substring, ignoring underscores */
    char* dd = (char*)malloc(sz + 1);
    if (!dd) caml_raise_out_of_memory();
    for (i = 0, j = 0; i < sz; i++) {
      if (d[i] == '_') continue;
      dd[j++] = d[i];
    }
    dd[j] = 0;
    r = ml_z_alloc();
    if (mp_read_radix(Z_MP(r), dd, base) != MP_OKAY) {
      free(dd);
      mp_clear(Z_MP(r));
      caml_invalid_argument("Z.of_substring_base: invalid string");
    }
    free(dd);
    if (sign) {
      if (mp_neg(Z_MP(r), Z_MP(r)) != MP_OKAY) {
        mp_clear(Z_MP(r));
        caml_failwith("Z.of_substring_base: internal error");
      }
    }
    r = ml_z_reduce(r);
  }
  CAMLreturn(r);
}

CAMLprim value ml_z_to_int(value v)
{
  if (Is_long(v)) return v;
  if (mp_cmp(Z_MP(v), &z_min_int) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int) <= 0) {
#ifdef ARCH_SIXTYFOUR
    return Val_long(mp_get_i64(Z_MP(v)));
#else
    return Val_long(mp_get_i32(Z_MP(v)));
#endif
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_nativeint(value v)
{
  if (Is_long(v)) return caml_copy_nativeint(Long_val(v));
  if (mp_cmp(Z_MP(v), &z_min_intnat) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_intnat) <= 0) {
#ifdef ARCH_SIXTYFOUR
    return caml_copy_nativeint(mp_get_i64(Z_MP(v)));
#else
    return caml_copy_nativeint(mp_get_i32(Z_MP(v)));
#endif
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_int32(value v)
{
  if (Is_long(v)) {
    intnat x = Long_val(v);
#ifdef ARCH_SIXTYFOUR
    if (x >= (intnat)Z_HI_INT32 || x < -(intnat)Z_HI_INT32)
      ml_z_raise_overflow();
#endif
    return caml_copy_int32(x);
  }
  if (mp_cmp(Z_MP(v), &z_min_int32) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int32) <= 0) {
    return caml_copy_int32(mp_get_i32(Z_MP(v)));
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_int64(value v)
{
  if (Is_long(v)) return caml_copy_int64(Long_val(v));
  if (mp_cmp(Z_MP(v), &z_min_int64) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int64) <= 0) {
    return caml_copy_int64(mp_get_i64(Z_MP(v)));
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_nativeint_unsigned(value v)
{
  if (Is_long(v)) {
    if (Long_val(v) < 0)
      ml_z_raise_overflow();
    return caml_copy_nativeint(Long_val(v));
  }
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_intnat_unsigned) <= 0) {
#ifdef ARCH_SIXTYFOUR
    return caml_copy_nativeint(mp_get_u64(Z_MP(v)));
#else
    return caml_copy_nativeint(mp_get_u32(Z_MP(v)));
#endif
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_int32_unsigned(value v)
{
  if (Is_long(v)) {
    intnat x = Long_val(v);
#ifdef ARCH_SIXTYFOUR
    if (x >= (intnat)Z_HI_UINT32 || x < 0)
      ml_z_raise_overflow();
#endif
    return caml_copy_int32(x);
  }
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_int32_unsigned) <= 0) {
    return caml_copy_int32(mp_get_u32(Z_MP(v)));
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_to_int64_unsigned(value v)
{
  if (Is_long(v)) {
    if (Long_val(v) < 0)
      ml_z_raise_overflow();
    return caml_copy_int64(Long_val(v));
  }
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_int64_unsigned) <= 0) {
    return caml_copy_int64(mp_get_u64(Z_MP(v)));
  }
  ml_z_raise_overflow();
  return 0;
}

CAMLprim value ml_z_format(value f, value v)
{
  CAMLparam2(f,v);
  Z_DECL(v);
  char* buf, *dst;
  int sz = 0;
  size_t i, max_size, size_dst = 0;
  value r;
  const char* fmt = String_val(f);
  int base = 10;     /* base */
  int cas = 0;       /* uppercase X / lowercase x */
  ptrdiff_t width = 0;
  int alt = 0;       /* alternate # */
  int dir = 0;       /* right / left adjusted */
  char sign = 0;     /* sign char */
  char pad = ' ';    /* padding char */
  char *prefix = "";
  Z_ARG(v);

  /* parse format */
  while (*fmt == '%') fmt++;
  for (; ; fmt++) {
    if (*fmt == '#') alt = 1;
    else if (*fmt == '0') pad = '0';
    else if (*fmt == '-') dir = 1;
    else if (*fmt == ' ' || *fmt == '+') sign = *fmt;
    else break;
  }
  if (mp_isneg(mp_v)) sign = '-';
  for (;*fmt>='0' && *fmt<='9';fmt++)
    width = 10*width + *fmt-'0';
  switch (*fmt) {
  case 'i': case 'd': case 'u': break;
  case 'b': base = 2; if (alt) prefix = "0b"; break;
  case 'o': base = 8; if (alt) prefix = "0o"; break;
  case 'x': base = 16; if (alt) prefix = "0x"; cas = 1; break;
  case 'X': base = 16; if (alt) prefix = "0X"; break;
  default: Z_END_ARG(v); caml_invalid_argument("Z.format: invalid format");
  }
  if (dir) pad = ' ';

  /* get number of digits (can be overapproximated) */
  if (mp_radix_size(mp_v, base, &sz) != MP_OKAY || sz == 0) {
    Z_END_ARG(v);
    caml_failwith("Z.format: internal error");
  }

  /* we need space for sign + prefix + digits + 1 + padding + terminal 0 */
  max_size = 1 + 2 + sz + 1 + 2*width + 1;
  buf = (char*) malloc(max_size);
  if (!buf) caml_raise_out_of_memory();
  dst = buf + 1 + 2 + width;

  /* get digits */
  if (mp_to_radix (mp_v, dst, sz, &size_dst, base) != MP_OKAY ||
      size_dst == 0 ||
      dst + size_dst >= buf + max_size) {
    Z_END_ARG(v);
    free(buf);
    caml_failwith("Z.format: internal error");
  }
  size_dst--;

  /* undo sign */
  if (mp_isneg(mp_v)) {
    dst++;
    size_dst--;
  }

  /* lower-case conversion */
  if (cas) {
    for (i = 0; i < size_dst; i++)
      if (dst[i] >= 'A')
        dst[i] += ('a' - 'A');
  }

  /* add prefix, sign & padding */
  if (pad == ' ') {
    if (dir) {
      /* left alignment */
      for (i = strlen(prefix); i > 0; i--, size_dst++)
        *(--dst) = prefix[i-1];
      if (sign) { *(--dst) = sign; size_dst++; }
      for (; (ptrdiff_t)size_dst < width; size_dst++)
        dst[size_dst] = pad;
    }
    else {
      /* right alignment, space padding */
      for (i = strlen(prefix); i > 0; i--, size_dst++)
        *(--dst) = prefix[i-1];
      if (sign) { *(--dst) = sign; size_dst++; }
      for (; (ptrdiff_t)size_dst < width; size_dst++) *(--dst) = pad;
    }
  }
  else {
    /* right alignment, non-space padding */
    width -= strlen(prefix) + (sign ? 1 : 0);
    for (; (ptrdiff_t)size_dst < width; size_dst++) *(--dst) = pad;
    for (i = strlen(prefix); i > 0; i--, size_dst++)
      *(--dst) = prefix[i-1];
    if (sign) { *(--dst) = sign; size_dst++; }
  }
  dst[size_dst] = 0;
  if (dst < buf || dst + size_dst >= buf + max_size)
    caml_failwith("Z.format: internal error");
  r = caml_copy_string(dst);
  free(buf);
  Z_END_ARG(v);
  CAMLreturn(r);
}

/* common part to ml_z_extract and ml_z_extract_internal */
void ml_z_extract_internal(mp_int* dst, value arg, uintnat o, uintnat l) {
  Z_DECL(arg);
  size_t sz, i;
  mp_int rem;

  sz = (l + MP_DIGIT_BIT - 1) / MP_DIGIT_BIT;

  if (mp_init(&rem) != MP_OKAY)
    ml_z_raise_out_of_memory();

  /* shift */
  Z_ARG(arg);
  if (mp_div_2d(mp_arg, o, dst, &rem) != MP_OKAY ||
      mp_grow(dst, sz) != MP_OKAY) {
    mp_clear(&rem);
    Z_END_ARG(arg);
    caml_failwith("Z.extract: internal error");
  }

  /* 0-pad */
  for (i = dst->used; i < sz; i++)
    dst->dp[i] = 0;
  dst->used = sz;
  dst->sign = MP_ZPOS;

  /* 2's complement */
  if (mp_isneg(mp_arg)) {
    for (i = 0; i < sz; i++)
      dst->dp[i] = (~dst->dp[i]) & MP_MASK;
    if (mp_iszero(&rem)) {
      /* all shifted-out bits are 0 */
      if (mp_incr(dst) != MP_OKAY) {
        mp_clear(&rem);
        Z_END_ARG(arg);
        caml_failwith("Z.extract: internal error");
      }
      /* in case of overflow in incr, ignore the new digit */
      dst->used = sz;
   }
  }

  /* mask out high bits */
  l %= MP_DIGIT_BIT;
  if (l) dst->dp[sz-1] &= MP_MASK >> (MP_DIGIT_BIT - l);
  mp_clamp(dst);
  mp_clear(&rem);
  Z_END_ARG(arg);
}


CAMLprim value ml_z_extract(value arg, value off, value len)
{
  CAMLparam1(arg);
  CAMLlocal1(r);
  r = ml_z_alloc();
  ml_z_extract_internal(Z_MP(r), arg, (uintnat)Long_val(off), (uintnat)Long_val(len));
  r = ml_z_reduce(r);
  CAMLreturn(r);
}

/* version without OCaml allocation */
CAMLprim value ml_z_extract_small(value arg, value off, value len)
{
  mp_int r;
  if (mp_init(&r) != MP_OKAY)
    ml_z_raise_out_of_memory();

  ml_z_extract_internal(&r, arg, (uintnat)Long_val(off), (uintnat)Long_val(len));

  if (mp_cmp(&r, &z_min_int) < 0 ||
      mp_cmp(&r, &z_max_int) > 0)
    /* The result should fit in an integer */
    caml_failwith("Z.extract: internal error");
  intnat x = mp_get_i64(&r);
  mp_clear(&r);
  return Val_long(x);
}

CAMLprim value ml_z_to_bits(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(r);
  Z_DECL(arg);
  size_t sz;
  Z_ARG(arg);
  sz = mp_pack_count(mp_arg, 0, 1);
  r = caml_alloc_string(sz);
  if (mp_pack((void*)String_val(r), sz, NULL, MP_LSB_FIRST, 1, MP_NATIVE_ENDIAN, 0, mp_arg) != MP_OKAY) {
    caml_failwith("Z.to_bits: internal error");
  }
  Z_END_ARG(arg);
  CAMLreturn(r);
}

CAMLprim value ml_z_of_bits(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(r);
  r = ml_z_alloc();
  if (mp_unpack(Z_MP(r), caml_string_length(arg), MP_LSB_FIRST, 1, MP_NATIVE_ENDIAN, 0, String_val(arg)) != MP_OKAY) {
    caml_failwith("Z.of_bits: internal error");
  }
  r = ml_z_reduce(r);
  CAMLreturn(r);
}


/*---------------------------------------------------
  TESTS AND COMPARISONS
  ---------------------------------------------------*/

CAMLprim value ml_z_compare(value arg1, value arg2)
{
  /* Value-equal small integers are equal.
     Pointer-equal big integers are equal as well. */
  if (arg1 == arg2) return Val_long(0);
  if (Is_long(arg2)) {
    if (Is_long(arg1)) {
      return arg1 > arg2 ? Val_long(1) : Val_long(-1);
    } else {
      /* Either arg1 is positive and arg1 > Z_MAX_INT >= arg2 -> result +1
             or arg1 is negative and arg1 < Z_MIN_INT <= arg2 -> result -1 */
      return Z_SIGN(arg1) ? Val_long(-1) : Val_long(1);
    }
  }
  else if (Is_long(arg1)) {
    /* Either arg2 is positive and arg2 > Z_MAX_INT >= arg1 -> result -1
           or arg2 is negative and arg2 < Z_MIN_INT <= arg1 -> result +1 */
    return Z_SIGN(arg2) ? Val_long(1) : Val_long(-1);
  }
  else {
    /* slow path */
    mp_ord r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    Z_ARG(arg1);
    Z_ARG(arg2);
    r = mp_cmp(mp_arg1, mp_arg2);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    /* we assume MP_LT==-1, MP_EQ==0, MP_GT==1 */
    return Val_long(r);
  }
}


CAMLprim value ml_z_equal(value arg1, value arg2)
{
  mp_ord r;
  Z_DECL(arg1);
  Z_DECL(arg2);
  /* Value-equal small integers are equal.
     Pointer-equal big integers are equal as well. */
  if (arg1 == arg2) return Val_true;
  /* If both arg1 and arg2 are small integers but failed the equality
     test above, they are different.
     If one of arg1/arg2 is a small integer and the other is a big integer,
     they are different: one is in the range [Z_MIN_INT,Z_MAX_INT]
     and the other is outside this range. */
  if (Is_long(arg2) || Is_long(arg1)) return Val_false;
  /* slow path */
  Z_ARG(arg1);
  Z_ARG(arg2);
  r = mp_cmp(mp_arg1, mp_arg2);
  Z_END_ARG(arg1);
  Z_END_ARG(arg2);
  return (r == MP_EQ) ? Val_true : Val_false;
}

CAMLprim value ml_z_sign(value arg)
{
  if (Is_long(arg)) {
    if (arg > Val_long(0)) return Val_long(1);
    else if (arg < Val_long(0)) return Val_long(-1);
    else return Val_long(0);
  }
  else {
    /* zero is a small integer, treated above */
    if (mp_isneg(Z_MP(arg))) return Val_long(-1);
    return Val_long(1);
  }
}

CAMLprim value ml_z_fits_int(value v)
{
  if (Is_long(v)) return Val_true;
  if (mp_cmp(Z_MP(v), &z_min_int) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int) <= 0)
    return Val_true;
  return Val_false;
}

CAMLprim value ml_z_fits_nativeint(value v)
{
  if (Is_long(v)) return Val_true;
  if (mp_cmp(Z_MP(v), &z_min_intnat) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_intnat) <= 0)
    return Val_true;
  return Val_false;
}

CAMLprim value ml_z_fits_int32(value v)
{
  if (Is_long(v)) {
#ifdef ARCH_SIXTYFOUR
    intnat x = Long_val(v);
    if (x >= (intnat)Z_HI_INT32 || x < -(intnat)Z_HI_INT32)
      return Val_false;
#endif
    return Val_true;
  }
  if (mp_cmp(Z_MP(v), &z_min_int32) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int32) <= 0)
    return Val_true;
  return Val_false;
}


CAMLprim value ml_z_fits_int64(value v)
{
  if (Is_long(v)) return Val_true;
  if (mp_cmp(Z_MP(v), &z_min_int64) >= 0 &&
      mp_cmp(Z_MP(v), &z_max_int64) <= 0)
    return Val_true;
  return Val_false;
}

CAMLprim value ml_z_size(value v)
{
  if (Is_long(v)) return Val_long(1);
  else return Val_long(Z_MP(v)->used);
}


CAMLprim value ml_z_fits_nativeint_unsigned(value v)
{
  if (Is_long(v))
    return Long_val(v) >= 0 ? Val_true : Val_false;
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_intnat_unsigned) <= 0)
    return Val_true;
  return Val_false;
}

CAMLprim value ml_z_fits_int32_unsigned(value v)
{
  if (Is_long(v)) {
    intnat x = Long_val(v);
#ifdef ARCH_SIXTYFOUR
    if (x >= (intnat)Z_HI_UINT32 || x < 0)
      return Val_false;
#else
    if (x < 0)
      return Val_false;
#endif
    return Val_true;
  }
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_int32_unsigned) <= 0)
    return Val_true;
  return Val_false;
}

CAMLprim value ml_z_fits_int64_unsigned(value v)
{
  if (Is_long(v))
    return Long_val(v) >= 0 ? Val_true : Val_false;
  if (!mp_isneg(Z_MP(v)) &&
      mp_cmp(Z_MP(v), &z_max_int64_unsigned) <= 0)
    return Val_true;
  return Val_false;
}



/*---------------------------------------------------
  ARITHMETIC OPERATORS
  ---------------------------------------------------*/

CAMLprim value ml_z_neg(value arg)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
    if (arg > Val_long(Z_MIN_INT)) return 2 - arg;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    value r;
    Z_DECL(arg);
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_neg(mp_arg, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.neg: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_abs(value arg)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
    if (arg >= Val_long(0)) return arg;
    if (arg > Val_long(Z_MIN_INT)) return 2 - arg;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    value r;
    if (Z_ISNEG(arg)) {
      Z_DECL(arg);
      r = ml_z_alloc();
      Z_ARG(arg);
      if (mp_neg(mp_arg, Z_MP(r)) != MP_OKAY) {
        Z_END_ARG(arg);
        mp_clear(Z_MP(r));
        caml_failwith("Z.neg: internal error");
      }
      r = ml_z_reduce(r);
      Z_END_ARG(arg);
    }
    else r = arg;
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_add(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat v = a1 + a2;
    if (Z_FITS_INT(v)) return Val_long(v);
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_add(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.add: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_sub(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat v = a1 - a2;
    if (Z_FITS_INT(v)) return Val_long(v);
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_sub(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.sub: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_mul_overflows(value vx, value vy)
{
#if HAS_BUILTIN(__builtin_mul_overflow) || __GNUC__ >= 5
  intnat z;
  return Val_bool(__builtin_mul_overflow(vx - 1, vy >> 1, &z));
#elif defined(__GNUC__) && defined(__x86_64__)
  intnat z;
  unsigned char o;
  asm("imulq %1, %3; seto %0"
      : "=q" (o), "=r" (z)
      : "1" (vx - 1), "r" (vy >> 1)
      : "cc");
  return Val_int(o);
#elif defined(_MSC_VER) && defined(_M_X64)
  intnat hi, lo;
  lo = _mul128(vx - 1, vy >> 1, &hi);
  return Val_bool(hi != lo >> 63);
#else
  /* Portable C code */
  intnat x = Long_val(vx);
  intnat y = Long_val(vy);
  /* Quick approximate check for small values of x and y.
     Also catches the cases x = 0, x = 1, y = 0, y = 1. */
  if (Z_FITS_HINT(x)) {
    if (Z_FITS_HINT(y)) return Val_false;
    if ((uintnat) x <= 1) return Val_false;
  }
  if ((uintnat) y <= 1) return Val_false;
#if 1
  /* Give up at this point; we'll go through the general case in ml_z_mul */
  return Val_true;
#else
  /* The product x*y is representable as an unboxed integer if
     it is in [Z_MIN_INT, Z_MAX_INT].
     x >= 0 y >= 0:  x*y >= 0 and x*y <= Z_MAX_INT <-> y <= Z_MAX_INT / x
     x < 0  y >= 0:  x*y <= 0 and x*y >= Z_MIN_INT <-> x >= Z_MIN_INT / y
     x >= 0 y < 0 :  x*y <= 0 and x*y >= Z_MIN_INT <-> y >= Z_MIN_INT / x
     x < 0  y < 0 :  x*y >= 0 and x*y <= Z_MAX_INT <-> x >= Z_MAX_INT / y */
  if (x >= 0)
    if (y >= 0)
      return Val_bool(y > Z_MAX_INT / x);
    else
      return Val_bool(y < Z_MIN_INT / x);
  else
    if (y >= 0)
      return Val_bool(x < Z_MIN_INT / y);
    else
      return Val_bool(x < Z_MAX_INT / y);
#endif
#endif
}

CAMLprim value ml_z_mul(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2) &&
      ml_z_mul_overflows(arg1, arg2) == Val_false)) {
    /* fast path */
    return Val_long(Long_val(arg1) * Long_var(arg2));
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_mul(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.mul: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_div_rem(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q,r;
    q = a1 / a2;
    r = a1 % a2;
    if (Z_FITS_INT(q) && Z_FITS_INT(r)) {
      value p = caml_alloc_small(2, 0);
      Field(p,0) = Val_long(q);
      Field(p,1) = Val_long(r);
      return p;
    }
  }
  /* slow path */
  {
    CAMLparam2(arg1, arg2);
    CAMLlocal3(q, r, p);
    Z_DECL(arg1);
    Z_DECL(arg2);
    q = ml_z_alloc();
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_div(mp_arg1, mp_arg2, Z_MP(q), Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(q));
      mp_clear(Z_MP(r));
      caml_failwith("Z.div_rem: internal error");
    }
    q = ml_z_reduce(q);
    r = ml_z_reduce(r);
    p = caml_alloc_small(2, 0);
    Field(p,0) = q;
    Field(p,1) = r;
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(p);
  }
}

CAMLprim value ml_z_div(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
  /* slow path */
  {
    CAMLparam2(arg1, arg2);
    CAMLlocal1(q);
    Z_DECL(arg1);
    Z_DECL(arg2);
    q = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_div(mp_arg1, mp_arg2, Z_MP(q), NULL) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(q));
      caml_failwith("Z.div: internal error");
    }
    q = ml_z_reduce(q);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(q);
  }
}


CAMLprim value ml_z_rem(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat r = a1 % a2;
    if (Z_FITS_INT(r)) return Val_long(r);
  }
  /* slow path */
  {
    CAMLparam2(arg1, arg2);
    CAMLlocal1(r);
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_div(mp_arg1, mp_arg2, NULL, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.rem: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}


/* helper function for division with rounding towards +oo / -oo */
static value ml_z_rdiv(value arg1, value arg2, intnat dir)
{
  CAMLparam2(arg1, arg2);
  CAMLlocal2(q, r);
  Z_DECL(arg1);
  Z_DECL(arg2);
  q = ml_z_alloc();
  r = ml_z_alloc();
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (mp_div(mp_arg1, mp_arg2, Z_MP(q), Z_MP(r)) != MP_OKAY) {
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    mp_clear(Z_MP(q));
    mp_clear(Z_MP(r));
    caml_failwith("Z.[cf]div: internal error");
  }

  if (!mp_iszero(Z_MP(r))) {
    if (dir > 0 && mp_isneg(mp_arg1) == mp_isneg(mp_arg2)) {
      if (mp_incr(Z_MP(q)) != MP_OKAY) {
        Z_END_ARG(arg1);
        Z_END_ARG(arg2);
        mp_clear(Z_MP(q));
        mp_clear(Z_MP(r));
        caml_failwith("Z.[cf]div: internal error");
      }
    }
    if (dir < 0 && mp_isneg(mp_arg1) != mp_isneg(mp_arg2))
      if (mp_decr(Z_MP(q)) != MP_OKAY) {
        Z_END_ARG(arg1);
        Z_END_ARG(arg2);
        mp_clear(Z_MP(q));
        mp_clear(Z_MP(r));
        caml_failwith("Z.[cf]div: internal error");
      }
  }
  q = ml_z_reduce(q);
  Z_END_ARG(arg1);
  Z_END_ARG(arg2);
  mp_clear(Z_MP(r));
  CAMLreturn(q);
}

CAMLprim value ml_z_cdiv(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    /* adjust to round towards +oo */
    if (a1 > 0 && a2 > 0) a1 += a2-1;
    else if (a1 < 0 && a2 < 0) a1 += a2+1;
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
  /* slow path */
  return ml_z_rdiv(arg1, arg2, 1);
}

CAMLprim value ml_z_fdiv(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    intnat q;
    /* adjust to round towards -oo */
    if (a1 < 0 && a2 > 0) a1 -= a2-1;
    else if (a1 > 0 && a2 < 0) a1 -= a2+1;
    q = a1 / a2;
    if (Z_FITS_INT(q)) return Val_long(q);
  }
  /* slow path */
  return ml_z_rdiv(arg1, arg2, -1);
}


CAMLprim value ml_z_succ(value arg)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
    if (arg < Val_long(Z_MAX_INT)) return arg + 2;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    value r;
    Z_DECL(arg);
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_add_d(mp_arg, 1, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.succ: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}


CAMLprim value ml_z_pred(value arg)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
     if (arg > Val_long(Z_MIN_INT)) return arg - 2;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    value r;
    Z_DECL(arg);
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_sub_d(mp_arg, 1, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.pred: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_sqrt(value arg)
{
  CAMLparam1(arg);
  Z_DECL(arg);
  value r;
  Z_ARG(arg);
  if (mp_isneg(mp_arg)) {
    Z_END_ARG(arg);
    caml_invalid_argument("Z.sqrt: square root of a negative number");
  }
  r = ml_z_alloc();
  Z_REFRESH(arg);
  if (mp_sqrt(mp_arg, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.sqrt: internal error");
  }
  r = ml_z_reduce(r);
  Z_END_ARG(arg);
  CAMLreturn(r);
}

CAMLprim value ml_z_sqrt_rem(value arg)
{
  CAMLparam1(arg);
  CAMLlocal3(r1,r2,r);
  Z_DECL(arg);
  Z_ARG(arg);
  if (mp_isneg(mp_arg)) {
    Z_END_ARG(arg);
    caml_invalid_argument("Z.sqrt_rem: square root of a negative number");
  }
  r1 = ml_z_alloc();
  r2 = ml_z_alloc();
  Z_REFRESH(arg);
  if (mp_sqrt(mp_arg, Z_MP(r1)) != MP_OKAY ||
      mp_mul(Z_MP(r1),Z_MP(r1),Z_MP(r2)) != MP_OKAY ||
      mp_sub(mp_arg,Z_MP(r2),Z_MP(r2)) != MP_OKAY) {
    caml_failwith("Z.sqrt_rem: internal error");
    Z_END_ARG(arg);
  }
  r1 = ml_z_reduce(r1);
  r2 = ml_z_reduce(r2);
  r = caml_alloc_small(2, 0);
  Field(r,0) = r1;
  Field(r,1) = r2;
  Z_END_ARG(arg);
  CAMLreturn(r);
}

CAMLprim value ml_z_gcd(value arg1, value arg2)
{
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    if (a1 < 0) a1 = -a1;
    if (a2 < 0) a2 = -a2;
    if (a1 < a2) { intnat t = a1; a1 = a2; a2 = t; }
    while (a2)  {
      intnat r = a1 % a2;
      a1 = a2; a2 = r;
    }
    /* If arg1 = arg2 = min_int, the result a1 is -min_int, not representable
       ￼       as a tagged integer; fall through the slow case, then. */
    if (a1 <= Z_MAX_INT) return Val_long(a1);
  }
  {
    /* slow path */
    CAMLparam2(arg1, arg2);
    CAMLlocal1(r);
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_gcd(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.gcd: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

/* only computes one cofactor */
CAMLprim value ml_z_gcdext_intern(value arg1, value arg2)
{
  CAMLparam2(arg1, arg2);
  CAMLlocal3(r, s, p);
  Z_DECL(arg1);
  Z_DECL(arg2);
  if (Z_ISZERO(arg1)) ml_z_raise_divide_by_zero();
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  r = ml_z_alloc();
  s = ml_z_alloc();
  Z_ARG(arg1);
  Z_ARG(arg2);
  if (mp_exteuclid(mp_arg1, mp_arg2, Z_MP(s), NULL, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      mp_clear(Z_MP(s));
      caml_failwith("Z.gcdext: internal error");
  }
  r = ml_z_reduce(r);
  s = ml_z_reduce(s);
  p = caml_alloc_small(3, 0);
  Field(p,0) = r;
  Field(p,1) = s;
  Field(p,2) = Val_true;
  Z_END_ARG(arg1);
  Z_END_ARG(arg2);
  CAMLreturn(p);
}


/*---------------------------------------------------
  BITWISE OPERATORS
  ---------------------------------------------------*/

CAMLprim value ml_z_logand(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return arg1 & arg2;
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_and(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.logand: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_logor(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return arg1 | arg2;
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_or(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.logor: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_logxor(value arg1, value arg2)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    return (arg1 ^ arg2) | 1;
  }
#endif
  {
    /* slow path */
    CAMLparam2(arg1,arg2);
    value r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    r = ml_z_alloc();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_xor(mp_arg1, mp_arg2, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(Z_MP(r));
      caml_failwith("Z.logxor: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_lognot(value arg)
{
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
    return (~arg) | 1;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    value r;
    Z_DECL(arg);
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_complement(mp_arg, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.lognot: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_left(value arg, value count)
{
  intnat c = Long_val(count);
  if (c < 0)
    caml_invalid_argument("Z.shift_left: count argument must be positive");
  if (c > INT_MAX)
    caml_invalid_argument("Z.shift_left: count argument too large");
  if (!c) return arg;
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg) && c < Z_INTNAT_BITS) {
    /* fast path */
    value a = arg - 1;
    value r = arg << c;
    if (a == (r >> c)) return r | 1;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    Z_DECL(arg);
    value r;
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_mul_2d(mp_arg, c, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.shift_left: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_right(value arg, value count)
{
  intnat c = Long_val(count);
  if (c < 0)
    caml_invalid_argument("Z.shift_right: count argument must be positive");
  if (c > INT_MAX)
    caml_invalid_argument("Z.shift_left: count argument too large");
  if (!c) return arg;
#if !Z_FAST_PATH_IN_OCAML
  if (Is_long(arg)) {
    /* fast path */
    if (c >= Z_INTNAT_BITS) {
      if (arg < 0) return Val_long(-1);
      else return Val_long(0);
    }
    return (arg >> c) | 1;
  }
#endif
  {
    /* slow path */
    CAMLparam1(arg);
    Z_DECL(arg);
    value r;
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_signed_rsh(mp_arg, c, Z_MP(r)) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.shift_right: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}

CAMLprim value ml_z_shift_right_trunc(value arg, value count)
{
  intnat c = Long_val(count);
  if (c < 0)
    caml_invalid_argument("Z.shift_right_trunc: count argument must be positive");
  if (c > INT_MAX)
    caml_invalid_argument("Z.shift_left: count argument too large");
  if (!c) return arg;
  if (Is_long(arg)) {
    /* fast path */
    if (c >= Z_INTNAT_BITS) return Val_long(0);
    if (arg >= 1) return (arg >> c) | 1;
    else return Val_long(- ((- Long_val(arg)) >> c));
  }
  {
    /* slow path */
    CAMLparam1(arg);
    Z_DECL(arg);
    value r;
    r = ml_z_alloc();
    Z_ARG(arg);
    if (mp_div_2d(mp_arg, c, Z_MP(r), NULL) != MP_OKAY) {
      Z_END_ARG(arg);
      mp_clear(Z_MP(r));
      caml_failwith("Z.shift_right_trunc: internal error");
    }
    r = ml_z_reduce(r);
    Z_END_ARG(arg);
    CAMLreturn(r);
  }
}


/* Helper function for numbits: number of leading 0 bits in x */

#ifdef ARCH_SIXTYFOUR
#define BUILTIN_CLZ __builtin_clzll
#else
#define BUILTIN_CLZ __builtin_clzl
#endif

/* Use GCC or Clang built-in if available.  The argument must be != 0. */
#if defined(__clang__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define ml_z_clz BUILTIN_CLZ
#else
/* Portable C implementation - Hacker's Delight fig 5.12 */
int ml_z_clz(uintnat x)
{
  int n;
  uintnat y;
#ifdef ARCH_SIXTYFOUR
  n = 64;
  y = x >> 32;  if (y != 0) { n = n - 32; x = y; }
#else
  n = 32;
#endif
  y = x >> 16;  if (y != 0) { n = n - 16; x = y; }
  y = x >>  8;  if (y != 0) { n = n -  8; x = y; }
  y = x >>  4;  if (y != 0) { n = n -  4; x = y; }
  y = x >>  2;  if (y != 0) { n = n -  2; x = y; }
  y = x >>  1;  if (y != 0) return n - 2;
  return n - x;
}
#endif

CAMLprim value ml_z_numbits(value arg)
{
  if (Is_long(arg)) {
    intnat r = Long_val(arg);
    if (r == 0) {
      return Val_int(0);
    } else {
      int n = ml_z_clz(r > 0 ? r : -r);
      return Val_long(sizeof(intnat) * 8 - n);
    }
  }
  else {
    if (mp_iszero(Z_MP(arg))) return Val_long(0);
    return Val_long(mp_count_bits(Z_MP(arg)));
  }
}


/* Helper function for trailing_zeros: number of trailing 0 bits in x */

#ifdef ARCH_SIXTYFOUR
#define BUILTIN_CTZ __builtin_ctzll
#else
#define BUILTIN_CTZ __builtin_ctzl
#endif

/* Use GCC or Clang built-in if available.  The argument must be != 0. */
#if defined(__clang__) || __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#define ml_z_ctz BUILTIN_CTZ
#else
/* Portable C implementation - Hacker's Delight fig 5.21 */
int ml_z_ctz(uintnat x)
{
  int n;
  uintnat y;
  CAMLassert (x != 0);
#ifdef ARCH_SIXTYFOUR
  n = 63;
  y = x << 32;  if (y != 0) { n = n - 32; x = y; }
#else
  n = 31;
#endif
  y = x << 16;  if (y != 0) { n = n - 16; x = y; }
  y = x <<  8;  if (y != 0) { n = n -  8; x = y; }
  y = x <<  4;  if (y != 0) { n = n -  4; x = y; }
  y = x <<  2;  if (y != 0) { n = n -  2; x = y; }
  y = x <<  1;  if (y != 0) { n = n - 1; }
  return n;
}
#endif

CAMLprim value ml_z_trailing_zeros(value arg)
{
  if (Is_long(arg)) {
    /* fast path */
    intnat r = Long_val(arg);
    if (r == 0) {
      return Val_long (Max_long);
    } else {
      /* No need to take absolute value of r, as ctz(-x) = ctz(x) */
      return Val_long (ml_z_ctz(r));
    }
  }
  else {
    if (mp_iszero(Z_MP(arg))) return Val_long(Max_long);
    return Val_long(mp_cnt_lsb(Z_MP(arg)));
  }
}
  

/* helper function for popcount & hamdist: number of bits at 1 in x */
/* maybe we should use the mpn_ function even for small arguments, in case
   the CPU has a fast popcount opcode?
 */
uintnat ml_z_count(uintnat x)
{
#ifdef ARCH_SIXTYFOUR
  x = (x & 0x5555555555555555UL) + ((x >>  1) & 0x5555555555555555UL);
  x = (x & 0x3333333333333333UL) + ((x >>  2) & 0x3333333333333333UL);
  x = (x & 0x0f0f0f0f0f0f0f0fUL) + ((x >>  4) & 0x0f0f0f0f0f0f0f0fUL);
  x = (x & 0x00ff00ff00ff00ffUL) + ((x >>  8) & 0x00ff00ff00ff00ffUL);
  x = (x & 0x0000ffff0000ffffUL) + ((x >> 16) & 0x0000ffff0000ffffUL);
  x = (x & 0x00000000ffffffffUL) + ((x >> 32) & 0x00000000ffffffffUL);
#else
  x = (x & 0x55555555UL) + ((x >>  1) & 0x55555555UL);
  x = (x & 0x33333333UL) + ((x >>  2) & 0x33333333UL);
  x = (x & 0x0f0f0f0fUL) + ((x >>  4) & 0x0f0f0f0fUL);
  x = (x & 0x00ff00ffUL) + ((x >>  8) & 0x00ff00ffUL);
  x = (x & 0x0000ffffUL) + ((x >> 16) & 0x0000ffffUL);
#endif
  return x;
}

CAMLprim value ml_z_popcount(value arg)
{
  if (Is_long(arg)) {
    /* fast path */
    intnat r = Long_val(arg);
    if (r < 0) ml_z_raise_overflow();
    return Val_long(ml_z_count(r));
  }
  else {
    intnat r;
    int i;
    mp_digit* p;
    if (mp_isneg(Z_MP(arg))) ml_z_raise_overflow();
    for (i=0, r=0, p=Z_MP(arg)->dp; i < Z_MP(arg)->used; i++, p++)
      r += ml_z_count(*p);
    if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
    return Val_long(r);
  }
}

CAMLprim value ml_z_hamdist(value arg1, value arg2)
{
  if (Z_ISNEG(arg1) != Z_ISNEG(arg2))
    ml_z_raise_overflow();
  /* XXX TODO: case where arg1 & arg2 are both negative */
  if (Z_ISNEG(arg1) || Z_ISNEG(arg2))
    caml_invalid_argument("Z.hamdist: negative arguments");
  if (Is_long(arg1) && Is_long(arg2)) {
    return Val_long(ml_z_count(Long_val(arg1) ^ Long_val(arg2)));
  }
  else {
    Z_DECL(arg1);
    Z_DECL(arg2);
    intnat r = 0;
    mp_digit *a1, *a2;
    size_t len1, len2;
    Z_ARG(arg1);
    Z_ARG(arg2);
    /* a1 is the shortest one */
    if (mp_arg1->used <= mp_arg2->used) {
      a1 = mp_arg1->dp; len1 = mp_arg1->used;
      a2 = mp_arg2->dp; len2 = mp_arg2->used;
    }
    else  {
      a1 = mp_arg2->dp; len1 = mp_arg2->used;
      a2 = mp_arg1->dp; len2 = mp_arg1->used;
    }
    for (size_t i = 0; i < len1; i++)
      r += ml_z_count(a1[i] ^ a2[i]);
    for (size_t i = len1; i < len2; i++)
      r += ml_z_count(a2[i]);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    if (r < 0 || !Z_FITS_INT(r)) ml_z_raise_overflow();
    return Val_long(r);
  }
}

CAMLprim value ml_z_testbit(value arg, value index)
{
  intnat b_idx = Long_val(index); /* Caml code checked index >= 0 */
  intnat l_idx = b_idx / MP_DIGIT_BIT;
  mp_digit d;
  if (Is_long(arg)) {
    if (b_idx >= Z_INTNAT_BITS) b_idx = Z_INTNAT_BITS - 1;
    return Val_int((Long_val(arg) >> b_idx) & 1);
  }
  if (l_idx >= Z_MP(arg)->used) return Val_bool(mp_isneg(Z_MP(arg)));
  d = Z_LIMB(arg)[l_idx];
  if (mp_isneg(Z_MP(arg))) {
    for (intnat i = 0; i < l_idx; i++) {
      if (Z_LIMB(arg)[i] != 0) { d = ~d; goto extract; }
    }
    d = -d;
  }
 extract:
  return Val_int((d >> (b_idx % MP_DIGIT_BIT)) & 1);
}

CAMLprim value ml_z_divexact(value arg1, value arg2)
{
  return ml_z_div(arg1,arg2);
}

CAMLprim value ml_z_powm(value base, value exp, value mod)
{
  CAMLparam3(base,exp,mod);
  CAMLlocal1(r);
  Z_DECL(base);
  Z_DECL(exp);
  Z_DECL(mod);

  Z_ARG(base);
  Z_ARG(exp);
  Z_ARG(mod);
  r = ml_z_alloc();
  if (mp_exptmod(mp_base, mp_exp, mp_mod, Z_MP(r)) != MP_OKAY) {
    Z_END_ARG(base);
    Z_END_ARG(exp);
    Z_END_ARG(mod);
    mp_clear(Z_MP(r));
    caml_failwith("Z.powm: internal error");
  }
  r = ml_z_reduce(r);
  Z_END_ARG(base);
  Z_END_ARG(exp);
  Z_END_ARG(mod);
  CAMLreturn(r);
}

CAMLprim value ml_z_powm_sec(value base, value exp, value mod)
{
  return ml_z_powm(base, exp, mod);
}

CAMLprim value ml_z_pow(value base, value exp)
{
  CAMLparam2(base,exp);
  CAMLlocal1(r);
  Z_DECL(base);
  intnat e = Long_val(exp);
  if (e < 0)
    caml_invalid_argument("Z.pow: exponent must be nonnegative");
#ifdef ARCH_SIXTYFOUR
  if (e > 0x7fffffff)
    caml_invalid_argument("Z.pow: exponent too large");
#endif
  Z_ARG(base);
  r = ml_z_alloc();
  if (mp_expt_u32(mp_base, e, Z_MP(r)) != MP_OKAY) {
    Z_END_ARG(base);
    mp_clear(Z_MP(r));
    caml_failwith("Z.pow: internal error");
  }
  r = ml_z_reduce(r);
  Z_END_ARG(base);
  CAMLreturn(r);
}

CAMLprim value ml_z_root(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.root: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_rootrem(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.rootrem: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_perfect_power(UNUSED_PARAM value a)
{
  caml_failwith("Z.perfect_power: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_perfect_square(UNUSED_PARAM value a)
{
  caml_failwith("Z.perfect_square: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_probab_prime(UNUSED_PARAM value a, UNUSED_PARAM int b)
{
  caml_failwith("Z.probab_prime: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_nextprime(UNUSED_PARAM value a)
{
  caml_failwith("Z.nextprime: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_invert(UNUSED_PARAM value base, UNUSED_PARAM value mod)
{
  caml_failwith("Z.invert: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_divisible(value arg1, value arg2)
{
  if (Z_ISZERO(arg2)) ml_z_raise_divide_by_zero();
  if (Is_long(arg1) && Is_long(arg2)) {
    /* fast path */
    intnat a1 = Long_val(arg1);
    intnat a2 = Long_val(arg2);
    return Val_bool(a1 % a2 == 0);
  }
  /* slow path */
  {
    Z_DECL(arg1);
    Z_DECL(arg2);
    mp_int r;
    int res;
    if (mp_init(&r) != MP_OKAY)
      ml_z_raise_out_of_memory();
    Z_ARG(arg1);
    Z_ARG(arg2);
    if (mp_div(mp_arg1, mp_arg2, NULL, &r) != MP_OKAY) {
      Z_END_ARG(arg1);
      Z_END_ARG(arg2);
      mp_clear(&r);
      caml_failwith("Z.divisible: internal error");
    }
    res = mp_iszero(&r);
    mp_clear(&r);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    return Val_bool(res);
  }
}

CAMLprim value ml_z_congruent(UNUSED_PARAM value a, UNUSED_PARAM value b, UNUSED_PARAM value c)
{
  caml_failwith("Z.congruent: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_jacobi(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.jacobi: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_legendre(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.legendre: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_kronecker(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.kronecker: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_remove(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.remove: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_fac(UNUSED_PARAM value a)
{
  caml_failwith("Z.fac: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_fac2(UNUSED_PARAM value a)
{
  caml_failwith("Z.fac2: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_facM(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.facM: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_primorial(UNUSED_PARAM value a)
{
  caml_failwith("Z.primorial: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_bin(UNUSED_PARAM value a, UNUSED_PARAM value b)
{
  caml_failwith("Z.bin: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_fib(UNUSED_PARAM value a)
{
  caml_failwith("Z.fib: not implemented in LibTomMath backend");
}

CAMLprim value ml_z_lucnum(UNUSED_PARAM value a)
{
  caml_failwith("Z.lucnum: not implemented in LibTomMath backend");
}


/*---------------------------------------------------
  CUSTOMS BLOCKS
  ---------------------------------------------------*/

static void ml_z_custom_finalize(value v) {
  mp_clear(Z_MP(v));
}

/* With OCaml < 3.12.1, comparing a block an int with OCaml's
   polymorphic compare will give erroneous results (int always
   strictly smaller than block).  OCaml 3.12.1 and above
   give the correct result.
*/
int ml_z_custom_compare(value arg1, value arg2)
{
  /* Value-equal small integers are equal.
￼    Pointer-equal big integers are equal as well. */
  if (arg1 == arg2) return 0;
  if (Is_long(arg2)) {
    if (Is_long(arg1)) {
      return arg1 > arg2 ? 1 : -1;
    } else {
      /* Either arg1 is positive and arg1 > Z_MAX_INT >= arg2 -> result +1
         or arg1 is negative and arg1 < Z_MIN_INT <= arg2 -> result -1 */
      return Z_SIGN(arg1) ? -1 : 1;
    }
  }
  else if (Is_long(arg1)) {
    /* Either arg2 is positive and arg2 > Z_MAX_INT >= arg1 -> result -1
       or arg2 is negative and arg2 < Z_MIN_INT <= arg1 -> result +1 */
    return Z_SIGN(arg2) ? 1 : -1;
  }
  else {
    /* slow path */
    int r;
    Z_DECL(arg1);
    Z_DECL(arg2);
    Z_ARG(arg1);
    Z_ARG(arg2);
    r = mp_cmp(mp_arg1, mp_arg2);
    Z_END_ARG(arg1);
    Z_END_ARG(arg2);
    /* we assume MP_LT==-1, MP_EQ==0, MP_GT==1 */
    return r;
  }
}


#if 1 /* Select the ml_z_custom_hash implementation */

/*
  This version of hash does not give the same result as the GMP one.
  Moreover, the hash value depends on MP_DIGIT_BIT.
  However, it is simple and fast, and so, enabled by default.
*/

static intnat ml_z_custom_hash(value v)
{
  if (Is_long(v)) {
#ifdef ARCH_SIXTYFOUR
    return caml_hash_mix_uint32((uint32_t)v, (uint32_t)(v >> 32));
#else
    return v;
#endif
  }
  else {
    uint32_t r = 0;
    int i;
    mp_digit* p;
    if (mp_isneg(Z_MP(v))) r = 1;
    for (i=0, p=Z_MP(v)->dp; i < Z_MP(v)->used; i++, p++) {
#ifdef MP_64BIT
      r = caml_hash_mix_uint32(r, (uint32_t)*p);
      r = caml_hash_mix_uint32(r, (uint32_t)((*p) >> 32));
#else
      r = caml_hash_mix_uint32(r, (uint32_t)*p);
#endif
    }
    return r;
  }
}

#else

/*
  This version of hash gives the same result as the GMP one and
  does not depend on the value of MP_DIGIT_BITS.
  However, it is complex, slower and not much tested.
  It is currently disabled by #if
*/

static intnat ml_z_custom_hash(value v)
{
  if (Is_long(v)) {
    intnat n = Long_val(v);
    intnat a = n >= 0 ? n : -n;
    uint32_t h;
#ifdef ARCH_SIXTYFOUR
    h = caml_hash_mix_uint32((uint32_t)a, (uint32_t)(a >> 32));
#else
    h = a;
#endif
    if (n < 0) h++;
    return h;
  }
  else {
    uint32_t acc = 0;
    mp_digit* p;
    uint32_t word = 0; // 32-bit word in construction
    int bits = 0;      // actual bits in word
    size_t nb = 0;        // nb words
    size_t i;

    for (p = Z_MP(v)->dp, i = Z_MP(v)->used; i > 0; p++, i--) {
      // eat a new digit
      mp_digit d = *p;
      word |= (uint32_t)d << bits;
      bits += MP_DIGIT_BIT;
      if (bits >= 32) {
        // word complete
        nb++;
        acc = caml_hash_mix_uint32(acc, word);
        // remaining bits in d
        bits -= 32;
        d >>= MP_DIGIT_BIT - bits;
        while (bits >= 32 && (d || i > 1)) {
          // additional words
          nb++;
          acc = caml_hash_mix_uint32(acc, d);
          bits -= 32;
          d >>= 32;
        }
        word = d;
      }
    }
    if (bits > 0 && word) {
      // last piece of digit
      nb++;
      acc = caml_hash_mix_uint32(acc, word);
    }
    /* ensure an even number of words (compatibility with 64-bit GMP) */
    if (nb % 2 != 0) {
      acc = caml_hash_mix_uint32(acc, 0);
    }
    if (mp_isneg(Z_MP(v))) acc++;
    return acc;
  }
}

#endif

CAMLprim value ml_z_hash(value v)
{
  return Val_long(ml_z_custom_hash(v));
}

static void ml_z_custom_serialize(value v,
                                  uintnat * wsize_32,
                                  uintnat * wsize_64)
{
  size_t i, nb;
  uint8_t* buf;
  Z_DECL(v);
  Z_ARG(v);
  nb = mp_sbin_size(mp_v);
  if (nb != (uint32_t)nb) {
    caml_failwith("Z.serialize: number is too large");
  }
  buf = (uint8_t*)malloc(nb);
  if (!buf) caml_raise_out_of_memory();
  if (mp_to_sbin(mp_v, buf, nb, NULL) != MP_OKAY) {
    free(buf);
    Z_END_ARG(v);
    ml_z_raise_out_of_memory();
  }
  caml_serialize_int_4(nb);
  for (i = 0; i < nb; i++) 
    caml_serialize_int_1(buf[i]);
  /* struct { int; int; enum; ptr; } */
  *wsize_32 = 16;
  *wsize_64 = 24;
  free(buf);
  Z_END_ARG(v);
}

static uintnat ml_z_custom_deserialize(void * dst)
{
  uint32_t i, nb;
  uint8_t* buf;
  if (mp_init(dst) != MP_OKAY)
    ml_z_raise_out_of_memory();
  nb = caml_deserialize_uint_4();
  buf = (uint8_t*)malloc(nb);
  if (!buf) caml_raise_out_of_memory();
  for (i = 0; i < nb; i++) 
    buf[i] = caml_deserialize_uint_1();
  if (mp_from_sbin((mp_int*)dst, buf, nb) != MP_OKAY) {
    free(buf);
    ml_z_raise_out_of_memory();
  }
  free(buf);
  return sizeof(mp_int);
}

struct custom_operations ml_z_custom_ops = {
  /* Identifiers starting with _ are normally reserved for the OCaml runtime
     system, but we got authorization form Gallium to use "_z".
     It is very compact and stays in the spirit of identifiers used for
     int32 & co ("_i" & co.).
  */
  "_z",
  ml_z_custom_finalize,
  ml_z_custom_compare,
  ml_z_custom_hash,
  ml_z_custom_serialize,
  ml_z_custom_deserialize,
  ml_z_custom_compare,
#ifndef Z_OCAML_LEGACY_CUSTOM_OPERATIONS
  custom_fixed_length_default
#endif
};


/*---------------------------------------------------
  INIT / EXIT
  ---------------------------------------------------*/
CAMLprim value ml_z_init()
{
  mp_err err = MP_OKAY;
 /* checks */
  if (MP_LT!=-1 || MP_EQ!=0 || MP_GT!=1)
    caml_failwith("Z.init: invalid values for MP_LT, MP_EQ, MP_GT");
  /* useful constants */
  err |= mp_init_i32(&z_max_int32,   0x7fffffff);
  err |= mp_init_i32(&z_min_int32,  -0x80000000);
  err |= mp_init_i64(&z_max_int64,   0x7fffffffffffffffLL);
  err |= mp_init_i64(&z_min_int64,  -0x7fffffffffffffffLL - 1);
  err |= mp_init_u32(&z_max_int32_unsigned,   0xffffffffU);
  err |= mp_init_u64(&z_max_int64_unsigned,   0xffffffffffffffffLLU);
#ifdef ARCH_SIXTYFOUR
  err |= mp_init_i64(&z_max_int,     0x3fffffffffffffffLL);
  err |= mp_init_i64(&z_min_int,    -0x4000000000000000LL);
  err |= mp_init_i64(&z_max_intnat,  0x7fffffffffffffffLL);
  err |= mp_init_i64(&z_min_intnat, -0x7fffffffffffffffLL - 1);
  err |= mp_init_u64(&z_max_intnat_unsigned,   0xffffffffffffffffLLU);
#else
  err |= mp_init_i32(&z_max_int,     0x3fffffff);
  err |= mp_init_i32(&z_min_int,    -0x40000000);
  err |= mp_init_i32(&z_max_intnat,  0x7fffffff);
  err |= mp_init_i32(&z_min_intnat, -0x80000000);
  err |= mp_init_u32(&z_max_intnat_unsigned,   0xffffffffU);
#endif
  if (err != MP_OKAY) {
    caml_failwith("Z.init: failed to create constants");
  }
  /* install functions */
  caml_register_custom_operations(&ml_z_custom_ops);
  return Val_unit;
}

CAMLprim value ml_z_digit_bits()
{
  return Val_long(MP_DIGIT_BIT);
}
