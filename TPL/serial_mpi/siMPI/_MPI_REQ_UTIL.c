/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: _MPI_REQ_UTIL.c,v $
 *    $Author: gdsjaar $
 *    $Date: 2006/08/10 22:26:03 $
 *    $Revision: 1.1 $
 ****************************************************************************/
/******************************************************************************/
/* FILE  ********************     _MPI_REQ_UTIL.c      ************************/
/******************************************************************************/
/* Author : Lisa Alano July 12 2002                                           */
/* Copyright (c) 2002 University of California Regents                        */
/******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "mpi.h"


/*============================================================================*/
#ifndef PREALLOCATION_SIZE
#define PREALLOCATION_SIZE 10
#endif
/*============================================================================*/
int _MPI_Req_Insert (MPI_Request request)
{
  _MPI_COVERAGE();
  return _MPI_Req_Post (request->buffer, request->size, request->type, request->tag, request->comm, request->send);
}
/*============================================================================*/
int _MPI_Req_Invalid (int index)
{
  _MPI_COVERAGE();
  _MPI_REQ_LIST[index].buffer = (void *)0;
  _MPI_REQ_LIST[index].size = _MPI_NOT_VALID;
  _MPI_REQ_LIST[index].tag = _MPI_NOT_VALID;
  /* TODO: Figure this out */
  if(_MPI_REQ_LIST[index].valid == _MPI_VALID)
    _MPI_REQ_LIST[index].comm = MPI_COMM_NULL;
  _MPI_REQ_LIST[index].send = _MPI_NOT_VALID;
  _MPI_REQ_LIST[index].valid = _MPI_NOT_VALID;

  return MPI_SUCCESS;
}
/*============================================================================*/
int _MPI_Req_Equal (MPI_Request request1, MPI_Request request2)
{
  _MPI_COVERAGE();
  if ( (request1->tag == request2->tag) && (request1->comm == request2->comm) ) {
    _MPI_COVERAGE();
    return _MPI_TRUE;
  }
  _MPI_COVERAGE();
  return _MPI_FALSE;
}
/*============================================================================*/
int _MPI_Req_Find(int tag, MPI_Comm comm)
{
  int index;
  _MPI_COVERAGE();
  for (index=0; index<_MPI_REQ_COUNT; index++) {
     _MPI_COVERAGE();
     if(_MPI_REQ_LIST[index].valid == _MPI_VALID) {
       _MPI_COVERAGE();
       if (_MPI_REQ_LIST[index].comm == comm) {
         _MPI_COVERAGE();
         if ( (_MPI_REQ_LIST[index].tag == tag)||(tag == MPI_ANY_TAG) ) {
           _MPI_COVERAGE();
           return index;
         }
       }
     }
  }
  _MPI_COVERAGE();
  return _MPI_NOT_OK;
}
/*============================================================================*/
int _MPI_Set_Request(MPI_Request* request, void* message, int count, MPI_Datatype datatype, 
    int send, int tag, MPI_Comm comm)
{
  _MPI_COVERAGE();
/* KDDKDD I think this routine should be called only for sends.  Recvs search
   KDDKDD global request array. */
  *request = (_MPI_REQUEST_OBJECT *) _MPI_safeMalloc(sizeof(_MPI_REQUEST_OBJECT), "MPI_Recv malloc of MPI_Request");
  (*request)->buffer = message;
  (*request)->size = count;
  (*request)->tag = tag;
  (*request)->type = datatype;
  (*request)->comm = comm;
  (*request)->send = send;
  (*request)->valid = _MPI_VALID;

  return MPI_SUCCESS;
}
/*============================================================================*/
int _MPI_Unset_Request(MPI_Request request)
{
  _MPI_COVERAGE();
  
  request->buffer = 0;
  request->size = _MPI_NULL;
  request->tag = _MPI_NULL;
  request->type = MPI_DATATYPE_NULL;
  request->comm = MPI_COMM_NULL;
  request->send = _MPI_FALSE;
  request->valid = _MPI_NOT_VALID;
  _MPI_safeFree(request,"request");
  /*
    printf("%s:%d: Free(%x) [request]\n",__FILE__,__LINE__,(int)request); 
  */
  return MPI_SUCCESS;
}
/*============================================================================*/
int _MPI_Req_Exists(MPI_Request request)
{
  int i;
  _MPI_COVERAGE();
  for (i=0; i<_MPI_REQ_COUNT; i++)
  {
  _MPI_COVERAGE();
    if (_MPI_Req_Equal((MPI_Request) &_MPI_REQ_LIST[i], request)==_MPI_TRUE)
    {
  _MPI_COVERAGE();
       return i; 
    } 
  }
  _MPI_COVERAGE();
  return _MPI_FALSE;
}
/*============================================================================*/
int _MPI_Req_Post (void* buffer, int size, MPI_Datatype type,
        int tag, MPI_Comm comm, int send)
{
  int index;
  _MPI_COVERAGE();
  for (index=0; index < _MPI_REQ_ARRAY_SIZE; index++)
  {
  _MPI_COVERAGE();
    if (_MPI_REQ_LIST[index].valid == _MPI_NOT_VALID)
    {
  _MPI_COVERAGE();
      _MPI_REQ_LIST[index].buffer = buffer;
      _MPI_REQ_LIST[index].size = size;
      _MPI_REQ_LIST[index].tag = tag;
      _MPI_REQ_LIST[index].type = type;
      _MPI_REQ_LIST[index].comm = comm;
      _MPI_REQ_LIST[index].send = send;
      _MPI_REQ_LIST[index].valid = _MPI_VALID;
      _MPI_REQ_COUNT ++;
      return index;
    }
  }
  #ifndef PREALLOCATION_SIZE
  #define PREALLOCATION_SIZE 10
  #endif

  _MPI_REQ_LIST = (_MPI_REQUEST_OBJECT *) _MPI_safeRealloc(_MPI_REQ_LIST, (_MPI_REQ_ARRAY_SIZE+PREALLOCATION_SIZE)*sizeof(_MPI_REQUEST_OBJECT), "Error _MPI_Req_Post - reallocation");
  _MPI_REQ_LIST[index].buffer = buffer;
  _MPI_REQ_LIST[index].size = size;
  _MPI_REQ_LIST[index].tag = tag;
  _MPI_REQ_LIST[index].type = type;
  _MPI_REQ_LIST[index].comm = comm;
  _MPI_REQ_LIST[index].send = send;
  _MPI_REQ_LIST[index].valid = _MPI_VALID;
  _MPI_REQ_COUNT ++;

  _MPI_COVERAGE();
  return index;
}
/*============================================================================*/
int _MPI_Check_Request_Array(int count, MPI_Request array[])
{
  int index;
  _MPI_COVERAGE();
  for(index = 0; index<count; index++)
  {
  _MPI_COVERAGE();
    if(_MPI_Req_Null((MPI_Request) &array[index])==_MPI_FALSE) {
  _MPI_COVERAGE();
       return MPI_SUCCESS; 
    }
  }
  _MPI_COVERAGE();
  return _MPI_NOT_OK;  
}
/*============================================================================*/
int _MPI_Req_Null (MPI_Request request)
{
  _MPI_COVERAGE();
  if(request == MPI_REQUEST_NULL) {
  _MPI_COVERAGE();
     return _MPI_TRUE;
  }
  _MPI_COVERAGE();
  return _MPI_FALSE;
}
/*============================================================================*/
