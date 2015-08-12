// $Id: QA_Records.C,v 1.5 2004/03/19 15:39:45 gdsjaar Exp $

#include "QA_Records.h"
#include <iostream>
#include <string.h>
#include "exodusII.h"
#include "netcdf.h"
#include <stdlib.h>
#include <time.h>
#include <assert.h>
#include "Version.h"


QA_Records::QA_Records(int FileId) {
  // Read number of information records
  char *cdum=NULL;
  float fdum;
  int i,j;
  int error(ex_inquire(FileId, EX_INQ_QA, &NumberOfRecords, &fdum, cdum));
  if( error < 0 ){
    std::cerr << "Error: " << std::endl;
    std::cerr << "\tQA_RECORDS::QA_RECORDS() " << std::endl;
    std::cerr << "\tReading Number of QA records" << std::endl;
    exit(1);
  }
  if( error > 0){
    std::cerr << "WARNING:  " << std::endl;
    std::cerr << "QA_RECORDS::QA_RECORDS() " << std::endl;
    std::cerr << "\tReading Number of QA records" << std::endl;
    std::cerr << "\tContinuing... " << std::endl;
  }
  
  if( (NumberOfRecords-1)>=MAX_QA_RECORDS ){
    std::cerr << "Error: Too many QA Records " << std::endl;
    exit(1);
  }

  // Read records
  for( i=0;i<NumberOfRecords;i++)
    for( j=0;j<4;j++){
      qa[i][j] = new char[MAX_STR_LENGTH+1];
      assert( qa[i][j] != NULL );
    }
  error = ex_get_qa(FileId, qa);
  
  if(error <0){
    std::cerr << "Error: " << std::endl;
    std::cerr << "\tQA_RECORDS::QA_RECORDS() " << std::endl;
    std::cerr << "\tReading QA records" << std::endl;
    exit(1);
  }
  if( error > 0 ){
    std::cerr << "WARNING:  " << std::endl;
    std::cerr << "QA_RECORDS::QA_RECORDS() " << std::endl;
    std::cerr << "\tReading QA records" << std::endl;
    std::cerr << "\tContinuing... " << std::endl;
  }
  for( j=0;j<4;j++ ){
    qa[NumberOfRecords][j] = new char[MAX_STR_LENGTH+1];
    assert( qa[NumberOfRecords][j] != NULL );
  }
  strcpy( qa[NumberOfRecords][0],"GENHXSHL" );
  char version[81];
  strcpy( version , &(VERSION[11]) );
  int len = strlen( version );
  strcpy( &(version[len - 1]) , "\0" );
  strcpy( qa[NumberOfRecords][1],version  );
  char buffer[MAX_STR_LENGTH+1];
  time_t date_time = time(NULL);
  int str_len = 32;
  strftime( buffer, str_len, "%m/%d/%y", localtime(&date_time) );
  str_len = strlen( buffer );
  strncpy(qa[NumberOfRecords][2], buffer, str_len);
  strftime( buffer, str_len, "%H:%M:%S", localtime(&date_time) );
  str_len = strlen( buffer );
  strncpy(qa[NumberOfRecords][3], buffer, str_len);
  NumberOfRecords++;
}

QA_Records::~QA_Records() {
  int i,j;
  for(i=0;i<NumberOfRecords;i++)
    for(j=0;j<4;j++)
      delete [] qa[i][j];
}

void QA_Records::print() const {
  std::cout << "QA_RECORDS::print() " << std::endl;
  std::cout << "\t Number of QA records: " << NumberOfRecords << std::endl;
  for(int i=0;i<NumberOfRecords;i++)
    for(int j=0;j<4;j++)
      std::cout << "\t\tRecord[" << i << "][" << j << "]: " << qa[i][j] << std::endl;
}

void QA_Records::Save(int FileId) {
  int i,j;

  // Save records to file
  int error(ex_put_qa(FileId, NumberOfRecords, qa));
  
  // Delete locally allocated space
  for(i=0;i<NumberOfRecords;i++)
    for(j=0;j<4;j++)
      delete [] qa[i][j];

  if( error < 0 ){
    std::cerr << "Error: " << std::endl;
    std::cerr << "\tQA_RECORDS::Save() " << std::endl;
    std::cerr << "\tSaving QA records to file" << std::endl;
    exit(1);
  }
  if( error > 0 ){
    std::cerr << "WARNING:  " << std::endl;
    std::cerr << "\tQA_RECORDS::Save() " << std::endl;
    std::cerr << "\tSaving QA records to file" << std::endl;
    std::cerr << "\tContinuing... " << std::endl;
  }


}
