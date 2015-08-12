// $Id: QA_Records.h,v 1.3 1998/03/20 17:04:19 khbrown Exp $

#ifndef QA_RECORDS_H_
#define QA_RECORDS_H_

#define MAX_QA_RECORDS 2000

class QA_Records {
public :
  QA_Records(int);
  ~QA_Records();
  void print() const;
  void Save(int);
  char * qa[MAX_QA_RECORDS][4];
private:
  int NumberOfRecords;
};

#endif 
