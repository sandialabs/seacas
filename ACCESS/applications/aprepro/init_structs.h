#ifndef INIT_STRUCTS_H
#define INIT_STRUCTS_H
struct init
  {
    char *fname;
    double (*fnct)();
  };

struct str_init
  {
    char *fname;
    char *(*fnct)();
  };

struct var_init
  {
    char *vname;
    double value;
  };

struct svar_init
  {
    char *vname;
    char *value;
  };

#endif
