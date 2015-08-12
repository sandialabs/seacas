// $Id: Info_Records.C,v 1.6 2004/03/19 15:39:45 gdsjaar Exp $

#include <iostream>
#include <assert.h>
#include <stdlib.h>
#include "Info_Records.h"
#include "exodusII.h"
#include "netcdf.h"


Info_Records::Info_Records(int FileId) {
  // Read number of information records
  char *cdum = NULL;
  float fdum;
  int error(ex_inquire(FileId, EX_INQ_INFO, &NumberOfRecords, &fdum, cdum));
  if(error !=0){
    std::cerr << "Error: " << std::endl;
    std::cerr << "\tINFO_RECORDS::INFO_RECORDS() " << std::endl;
    std::cerr << "\tReading Number of INFO records" << std::endl;
    exit(1);
  }
  if(error){
    std::cerr << "WARNING:  " << std::endl;
    std::cerr << "INFO_RECORDS::INFO_RECORDS() " << std::endl;
    std::cerr << "\tReading Number of INFO records" << std::endl;
    std::cerr << "\tContinuing... " << std::endl;
  }
  
  // Read records
  if( NumberOfRecords != 0){
    Records = new char*[NumberOfRecords];
    assert( Records != NULL );
    for( int i=0;i<NumberOfRecords;++i){
      Records[i] = new char[MAX_LINE_LENGTH+1];
      assert( Records[i] != NULL );
    }
    error = ex_get_info (FileId, Records);
    if(error != 0){
      std::cerr << "Error: " << std::endl;
      std::cerr << "\tINFO_RECORDS::INFO_RECORDS() " << std::endl;
      std::cerr << "\tReading INFO records" << std::endl;
      exit(1);
    }
    if(error != 0){
      std::cerr << "WARNING:  " << std::endl;
      std::cerr << "INFO_RECORDS::INFO_RECORDS() " << std::endl;
      std::cerr << "\tReading INFO records" << std::endl;
     std::cerr << "\tContinuing... " << std::endl;
    }
  }
}

Info_Records::~Info_Records() {
  for(int i=0;i<NumberOfRecords;++i)
    delete [] Records[i];
  if( NumberOfRecords != 0 )
    delete [] Records;
}

void Info_Records::print() const {
  std::cout << "INFO_RECORDS::print() " << std::endl;
  std::cout << "\t Number of INFO records: " << NumberOfRecords << std::endl;
  for(int i=0;i<NumberOfRecords;++i)
    std::cout << "\t\tRecord[" << i << "]: " << Records[i] << std::endl;
}

void Info_Records::Save(int FileId) const {
  if( NumberOfRecords != 0){
    int error(ex_put_info (FileId, NumberOfRecords, Records));
    if(error != 0){
      std::cerr << "Error: " << std::endl;
      std::cerr << "\tINFO_RECORDS::Save() " << std::endl;
      std::cerr << "\tSaving INFO records to file" << std::endl;
      exit(1);
    }
    if( error != 0){
      std::cerr << "WARNING:  " << std::endl;
      std::cerr << "\tINFO_RECORDS::Save() " << std::endl;
      std::cerr << "\tSaving INFO records to file" << std::endl;
      std::cerr << "\tContinuing... " << std::endl;
    }
  }
}
