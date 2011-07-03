/* 
 * Copyright 2006 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement
 * retains certain rights in this software.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 *     * Neither the name of Sandia Corporation nor the names of its
 *       contributors may be used to endorse or promote products derived
 *       from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */
/* $Id: init.c,v 1.2 2009/06/10 04:30:12 gdsjaar Exp $ */
/***
   NAME
     init
   PURPOSE
     Initialize variables and functions Aprepro
***/
#include <stdlib.h> /* for malloc */
#include "aprepro.h"
#include "y.tab.h"
#include <sys/types.h>
#include "init_structs.h"

void init_table(char comment);
char comm_string[2];

extern double
  do_fabs(double x), do_acos(double x), do_acosd(double x), do_asin(double x), do_asind(double x),
  do_atan(double x), do_atan2(double x, double y), do_atan2d(double x, double y),
  do_atand(double x), do_ceil(double x), do_cos(double x), do_cosd(double x), do_cosh(double x),
  do_d2r(double x), do_dim(double x, double y), do_dist(double x1, double y1, double x2, double y2),
  do_exp(double x), do_floor(double x), do_fmod(double x, double y), do_int(double x),
  do_log(double x), do_log10(double x), do_max(double x, double y), do_min(double x, double y),
  do_r2d(double x), do_rand(double xl, double xh), do_sign(double x, double y), do_sin(double x),
  do_rand_normal(double mean, double stddev), do_rand_weibull(double alpha, double beta),
  do_rand_lognormal(double mean, double stddev), 
  do_sind(double x), do_sinh(double x), do_sqrt(double x), do_tan(double x), do_tand(double x),
  do_tanh(double x), do_hypot(double x, double y), do_polarX(double rad, double ang),
  do_polarY(double rad, double ang), do_angle(double x1, double y1, double x2, double y2),
  do_angled(double x1, double y1, double x2, double y2), do_lgamma(double val),
  do_julday(double mon, double day, double year),
  do_juldayhms(double mon, double day, double year, double h, double mi, double se),
  do_log1p(double x), do_acosh(double x), do_asinh(double x), do_atanh(double x),
  do_word_count(char *string, char *delm), do_strtod(char *string),
  do_nint(double x), do_option(char *option, double value);

struct init arith_fncts[] =
{
  {"Vangle", do_angle},
  {"Vangled", do_angled},
  {"abs", do_fabs},
  {"acos", do_acos},
  {"acosd", do_acosd},
  {"acosh", do_acosh},
  {"asin", do_asin},
  {"asind", do_asind},
  {"asinh", do_asinh},
  {"atan", do_atan},
  {"atan2", do_atan2},
  {"atan2d", do_atan2d},
  {"atand", do_atand},
  {"atanh", do_atanh},
  {"ceil", do_ceil},
  {"cos", do_cos},
  {"cosd", do_cosd},
  {"cosh", do_cosh},
  {"d2r", do_d2r},
  {"dim", do_dim},
  {"dist", do_dist},
  {"exp", do_exp},
  {"floor", do_floor},
  {"fmod", do_fmod},
  {"hypot", do_hypot},
  {"int", do_int},
  {"julday", do_julday},
  {"juldayhms", do_juldayhms},
  {"lgamma", do_lgamma},
  {"ln", do_log},
  {"log", do_log},
  {"log10", do_log10},
  {"log1p", do_log1p},
  {"max", do_max},
  {"min", do_min},
  {"nint", do_nint},
  {"polarX", do_polarX},
  {"polarY", do_polarY},
  {"r2d", do_r2d},
  {"rand", do_rand},
  {"rand_normal", do_rand_normal},
  {"rand_lognormal", do_rand_lognormal},
  {"rand_weibull", do_rand_weibull},
  {"sign", do_sign},
  {"sin", do_sin},
  {"sind", do_sind},
  {"sinh", do_sinh},
  {"sqrt", do_sqrt},
  {"tan", do_tan},
  {"tand", do_tand},
  {"tanh", do_tanh},
  {"word_count", do_word_count},
  {"strtod", do_strtod},
  {"option", do_option},
  {0, 0}				/* Last line must be 0, 0 */
};

extern char *do_tolower(char *string), *do_toupper(char *string), *do_tostring(double x),
  *do_output(char *filename), *do_get_word(double n, char *string, char *delm),
  *do_execute(char *string), *do_getenv(char *env), *do_error(char *error_string),
  *do_rescan(char *string),  *do_Units(char *type), *do_dumpsym(void),
#if !defined(NO_EXODUSII)
  *do_exodus_info(char *filename), *do_exodus_meta(char *filename),
#endif
  *do_include_path(char *new_path), *do_intout(double intval), *do_get_date(void), *do_get_time(void);
  

struct str_init string_fncts[] =
{
  {"tolower", do_tolower},
  {"toupper", do_toupper},
  {"tostring", do_tostring},
  {"getenv", do_getenv},
  {"error", do_error},
  {"output", do_output},
  {"get_word",do_get_word},
  {"execute", do_execute},
  {"rescan", do_rescan},
  {"Units", do_Units},
  {"DUMP", do_dumpsym},
  {"include_path", do_include_path},
  {"IO", do_intout},
  {"get_date", do_get_date},
  {"get_time", do_get_time},
#if !defined(NO_EXODUSII)
  {"exodus_info", do_exodus_info},
  {"exodus_meta", do_exodus_meta},
#endif
  {0, 0}				/* Last line must be 0, 0 */
};

struct var_init variables[] =
{
  {"DEG", 57.29577951308232087680},	/* 180/pi, degrees per radian */
  {"RAD", 0.01745329251994329576},	/* pi/180, radians per degree */
  {"E", 2.71828182845904523536},	/* e, base of natural log     */
  {"GAMMA", 0.57721566490153286060},	/* euler-mascheroni constant  */
  {"PHI", 1.61803398874989484820},	/* golden ratio               */
  {"PI", 3.14159265358979323846},	/* pi                         */
  {"PI_2", 1.57079632679489661923},	/* pi / 2			 */
  {"SQRT2", 1.41421356237309504880},	/* square root of 2		 */
  {"TRUE", 1},
  {"FALSE", 0},
  {0, 0}				/* Last line must be 0, 0 */
};

struct svar_init svariables[] =
{
  {"_FORMAT", "%.10g"},	/* Default output format */
  {0, 0}		/* Last line must be 0, 0 */
};
/* NOTE: The current comment is stored in "_C_"
 *	 Since it can be changed by user on command line, we
 *	 initialize is differently than the other string variables.
 */

void init_table(char comment)
{
  int i;
  symrec *ptr;
  for (i = 0; arith_fncts[i].fname != 0; i++)
    {
      ptr = putsym(arith_fncts[i].fname, FNCT);
      ptr->value.fnctptr = arith_fncts[i].fnct;
    }
  for (i = 0; string_fncts[i].fname != 0; i++)
    {
      ptr = putsym(string_fncts[i].fname, SFNCT);
      ptr->value.strfnct = string_fncts[i].fnct;
    }
  for (i = 0; variables[i].vname != 0; i++)
    {
      ptr = putsym(variables[i].vname, VAR);
      ptr->value.var = variables[i].value;
    }
  for (i = 0; svariables[i].vname != 0; i++)
    {
      ptr = putsym(svariables[i].vname, SVAR);
      ptr->value.svar = svariables[i].value;
    }
  sprintf(comm_string, "%c", comment);
  ptr = putsym("_C_", SVAR);
  ptr->value.svar = comm_string;
  {
  	char *version_string = (char *)malloc(8);
  	version(version_string);
  	ptr = putsym("VERSION", SVAR);
  	ptr->value.svar = version_string;
  }
}



