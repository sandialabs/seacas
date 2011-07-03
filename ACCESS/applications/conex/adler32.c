/*
 * adler32.c -- compute the Adler-32 checksum of a data stream
 * Copyright (C) 1995-2002 Mark Adler For conditions of distribution
 * and use, see copyright notice in zlib.h
 */

/* @(#) $Id: adler32.c,v 1.1 2006/09/08 13:10:56 gdsjaar Exp $ */
#if defined ADDC_
#define ADLER32 adler32_
#else
#define ADLER32 adler32
#endif

#define BASE 65521L /* largest prime smaller than 65536 */
#define NMAX 5552
/* NMAX is the largest n such that 255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#define DO1(buf,i)  {s1 += buf[i]; s2 += s1;}
#define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#define DO16(buf)   DO8(buf,0); DO8(buf,8);

/* ========================================================================= */
void ADLER32(int *crc, int *ids, int *length)
{
  const char *buf = (const char*)ids;
  int len = *length * sizeof(int);
  
  unsigned long adler = 0;
  unsigned long s1 = adler & 0xffff;
  unsigned long s2 = (adler >> 16) & 0xffff;
  int k;

  if (buf == 0 || len == 0) {
    *crc = 1;
    return;
  }

  while (len > 0) {
    k = len < NMAX ? len : NMAX;
    len -= k;
    while (k >= 16) {
      DO16(buf);
      buf += 16;
      k -= 16;
    }
    if (k != 0) do {
      s1 += *buf++;
      s2 += s1;
    } while (--k);
    s1 %= BASE;
    s2 %= BASE;
  }
  *crc = (s2 << 16) | s1;
  return;
}
