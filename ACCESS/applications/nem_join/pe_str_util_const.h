#ifndef _PE_STR_UTIL_CONST_H_
#define _PE_STR_UTIL_CONST_H_
/* Function prototypes */
extern
int token_compare(
  char *token,          /* The input character string */
  const char *key       /* The key to compare with token */
  );

/* Case insensitive version of token_compare */
extern
int token_case_compare (char *token, const char *key);

extern
void strip_string(
  char inp_str[],       /* The string to strip */
  const char *tokens    /* The tokens to strip from the beginning and
                         * end of the input string */
  );

extern
void clean_string(
  char inp_str[],       /* The string to clean */
  const char *tokens    /* The tokens to strip multiple copies of */
  );

extern
void string_to_lower(
  char inp_str[],       /* The string to convert to lower case */
  const char cstop      /* Character where to stop */
  );

/* string_length - string length (w/o trailing blanks) */
extern
int string_length (char *string);

#endif /* _PE_STR_UTIL_CONST_H_ */
