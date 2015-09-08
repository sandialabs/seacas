#include <stdio.h>
#include <string.h>
#include <ctype.h>

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 * Author(s):   Gary L. Hennigan (SNL 9221)
 *----------------------------------------------------------------------------
 * Functions contained in this file:
 *      token_compare()
 *      token_case_compare()
 *      strip_string()
 *      clean_string()
 *      string_to_lower()
 *      string_length()
 *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
int token_compare(char *token, const char *key)
{

  int i1, key_len, kcnt=0;

  key_len = strlen(key);

  for(i1=0; i1 < strlen(token); i1++)
  {
    if(isupper(token[i1]))
      token[i1] = tolower(token[i1]);

    if(token[i1] != ' ')
    {
      if(token[i1] == key[kcnt])
      {
        kcnt++;
        if(kcnt > key_len)
          return 0;
      }
      else
        return 0;
    }
    if(key[kcnt] == ' ')
      kcnt++;
  }

  if(kcnt == strlen(key))
    return 1;
  else
    return 0;

} /*--------------End token_compare()-----------*/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
/* 
 * Case insensitive version of token_compare
*/
int token_case_compare(char *token, const char *key)
{
  int i1, key_len, kcnt=0;

  key_len = strlen(key);

  for(i1=0; i1 < strlen(token); i1++)
  {
    if(isupper(token[i1]) && islower(key[kcnt]))
      token[i1] = tolower(token[i1]);

    if(islower(token[i1]) && isupper(key[kcnt]))
      token[i1] = toupper(token[i1]);

    if(token[i1] != ' ')
    {
      if(token[i1] == key[kcnt])
      {
        kcnt++;
        if(kcnt > key_len)
          return 0;
      }
      else
        return 0;
    }
    if(key[kcnt] == ' ')
      kcnt++;
  }

  if(kcnt == strlen(key))
    return 1;
  else
    return 0;

} /*--------------End token_case_compare()-----------*/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void strip_string(char inp_str[], const char *tokens)
{
  int  i, j, itok, ntokes, bval;

  i = 0;
  ntokes = strlen(tokens);

  while(inp_str[i] != '\0')
  {
    bval = 0;
    for(itok=0; itok < ntokes; itok++)
    {
      if(inp_str[i] == tokens[itok])
      {
        i++;
        bval = 1;
        break; /* out of for loop */
      }
    }
    if(bval == 0)
      break; /* out of while loop */
  }

  /* Move real part of string to the front */
  j = 0;
  while(inp_str[j+i] != '\0')
  {
    inp_str[j] = inp_str[j+i];
    j++;
  }
  inp_str[j] = inp_str[j+i];
  j--;

  /* Remove trailing tokens */
  while(j != -1)
  {
    bval = 0;
    for(itok=0; itok < ntokes; itok++)
    {
      if(inp_str[j] == tokens[itok])
      {
        bval = 1;
        j--;
        break; /* out of for loop */
      }
    }
    if(bval == 0)
      break; /* out of while loop */
  }

  inp_str[j+1] = '\0';

  return;
}

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void clean_string(char inp_str[], const char *tokens)
{
  int  i, j, itok, ntokes, bval, inplen;

  ntokes = strlen(tokens);
  inplen = strlen(inp_str);

  i = 0;
  bval = 0;
  while(inp_str[i] != '\0')
  {
    for(itok=0; itok < ntokes; itok++)
    {
      if(inp_str[i] == tokens[itok])
      {
        /* Find out if the next character is also a token */
        for(j=0; j < ntokes; j++)
        {
          if(inp_str[i+1] == tokens[j])
          {
            bval = 1;
            break;
          }
        }

        if(bval == 1)
        {
          for(j=i+1; j < inplen; j++)
            inp_str[j] = inp_str[j+1];

          inplen--;
          bval = 0;
          i--;
	  if (i < 0) i = 0;
        }
      }
    }

    i++;

  } /* End "while(inp_str[i] != '\0')" */

  return;

} /*---------------- End clean_string() -----------------*/

/*****************************************************************************/
/*****************************************************************************/
/*****************************************************************************/
void string_to_lower(char in_string[], const char cval)
{
  int len, cnt;

  len = strlen(in_string);
  for(cnt=0; cnt < len; cnt++)
  {
    if(in_string[cnt] == cval)
      return;

    if(isupper(in_string[cnt]))
      in_string[cnt] = tolower(in_string[cnt]);
  }

  return;
}

/* string_length - string length (w/o trailing blanks) */
int string_length (char *string)
{
  char *ptr = string+strlen(string);    /* start at end of string including blanks */
  while (*(--ptr) == ' ');      /* skip blanks */
  return(ptr-string+1);         /* return trimmed length of string */
}

