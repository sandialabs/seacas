// $Id: Info_Records.h,v 1.3 1998/03/20 17:04:13 khbrown Exp $

#ifndef INFO_RECORDS_H_
#define INFO_RECORDS_H_


class Info_Records {
public :
  Info_Records(int);
  ~Info_Records();
  void print() const;
  void Save(int) const;
private:
  int NumberOfRecords;
  char** Records;
};

#endif 


