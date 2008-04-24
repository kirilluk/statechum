/*
 * Copyright (c) 2006, 2007, 2008 Neil Walkinshaw and Kirill Bogdanov
 * 
 * This file is part of StateChum
 * 
 * StateChum is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * 
 * StateChum is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with
 * StateChum. If not, see <http://www.gnu.org/licenses/>.
 */ 

/* This file is based on the umfpack_simple.c file from UMFPACK, */

/* -------------------------------------------------------------------------- */
/* UMFPACK Copyright (c) Timothy A. Davis, CISE,                              */
/* Univ. of Florida.  All Rights Reserved.  See ../Doc/License for License.   */
/* web: http://www.cise.ufl.edu/research/sparse/umfpack                       */
/* -------------------------------------------------------------------------- */

#include <stdio.h>
#include <assert.h>
#include <malloc.h>
	

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <memory.h>
#include "umfpack.h"
#include "solver.h"

JNIEXPORT jint JNICALL
STDCALLFUDGE(JNI_OnLoad)
(JavaVM *vm, void *reserved)
{
  umfpack_Wi=NULL;umfpack_W=NULL;Control_IRSTEP=0;
  useWorkingMemory = JNI_FALSE;
  return JNI_VERSION_1_4;
}

JNIEXPORT void JNICALL
STDCALLFUDGE(JNI_OnUnload)(JavaVM *vm, void *reserved)
{
  freeMemory();
}


/*
UF_long    n = 5 ;
UF_long    Ap [ ] = {0, 2, 5, 9, 10, 12} ;
UF_long    Ai [ ] = { 0,  1,  0,   2,  4,  1,  2,  3,   4,  2,  1,  4} ;
double Ax [ ] = {2., 3., 3., -1., 4., 4., -3., 1., 2., 2., 6., 1.} ;
double b [ ] = {8., 45., -3., 3., 19.} ;
double x [5] ;
*/


const double *null = (double *) NULL ;

void processUMFError(struct solverData *data,SOLVER_INT status, const char *whatFailed, double *Control, double *Info)
{
  char * statusDesc = NULL;
  int switchArg = (int)Info[UMFPACK_STATUS];

  assert(data != NULL && whatFailed != NULL && data->java.j_env != NULL);

  umfpack_di_report_status (Info, status);
  umfpack_di_report_status (Control, status);
  switch(switchArg)
  {
  case UMFPACK_OK:
    statusDesc="OK";break;
  case UMFPACK_WARNING_singular_matrix:
    statusDesc="matrix is singular";break;
  case UMFPACK_WARNING_determinant_underflow:
    statusDesc="determinant underflow";break;
  case UMFPACK_WARNING_determinant_overflow:
    statusDesc="determinant overflow";break;
  case UMFPACK_ERROR_n_nonpositive:
    statusDesc="n is not positive";break;
  case UMFPACK_ERROR_invalid_matrix:
    statusDesc="matrix is invalid";break;
  case UMFPACK_ERROR_out_of_memory:
    statusDesc="out of memory";break;
  case UMFPACK_ERROR_argument_missing:
    statusDesc="argument is missing";break;
  case UMFPACK_ERROR_internal_error:
    statusDesc="UMFPACK reported an internal error";break;
  default: statusDesc="UNKNOWN";break;
  }
  assert(statusDesc != NULL);
  snprintf(data->message,sizeof(data->message),
	   "%s%s: %s (%d)\n",status != 0?"ERROR in ":"SUCCESS",
	   status != 0?whatFailed:"",
	   statusDesc,status);

  if (status != 0) // treat warnings as errors.
    throwException(data->java.j_env,data->message);
}

void dumpIntBuffer(SOLVER_INT *buffer, int size)
{
  int i=0;
  printf("buf: ");for(i=0;i<size;++i) printf("%d ",buffer[i]);printf("\n");
}

void dumpDoubleBuffer(double *buffer, int size)
{
  int i=0;
  printf("buf: ");for(i=0;i<size;++i) printf("%g ",buffer[i]);printf("\n");
}

// Solves the system of equations supplied in the array data and
// returns true on success, false on failure. 
// Upon a failure, the error
// string is filled in (data->message).
//
jboolean umfsolve(struct solverData *data)
{
    void *Symbolic, *Numeric ;
    SOLVER_INT status = 0;
    double Info [UMFPACK_INFO], Control [UMFPACK_CONTROL];
    Control[UMFPACK_IRSTEP]=Control_IRSTEP;

    assert(data != NULL);data->message[0]=0;

    if (useWorkingMemory == JNI_TRUE && (umfpack_W == NULL || umfpack_Wi == NULL))
    {
      throwException(data->java.j_env,"working memory was not allocated, call extmalloc to allocate");return JNI_FALSE;
    }
    

    // get the default control parameters
    umfpack_di_defaults (Control);

    // change the default print level for this demo
    // (otherwise, nothing will print)
    Control [UMFPACK_PRL] = 0 ;

    status = umfpack_di_symbolic (data->n, data->n, (SOLVER_INT*)data->Ap, (SOLVER_INT*)data->Ai, data->Ax, &Symbolic, Control, Info) ;
    if (status != 0)
    {
      processUMFError(data,status,"umfpack_di_symbolic",Control,Info);return JNI_FALSE;
    }

    status = umfpack_di_numeric ((SOLVER_INT*)data->Ap,(SOLVER_INT*) data->Ai, data->Ax, Symbolic, &Numeric, Control, Info) ;
    if (status != 0)
    {
      processUMFError(data,status,"umfpack_di_numeric",Control,Info);return JNI_FALSE;
    }

    umfpack_di_free_symbolic (&Symbolic);
    if (useWorkingMemory == JNI_TRUE)
      status = umfpack_di_wsolve (UMFPACK_A,(SOLVER_INT*) data->Ap,(SOLVER_INT*) data->Ai,  data->Ax, data->x, data->b, Numeric, Control, Info, umfpack_Wi, umfpack_W);
    else
      status = umfpack_di_solve (UMFPACK_A,(SOLVER_INT*) data->Ap,(SOLVER_INT*) data->Ai,  data->Ax, data->x, data->b, Numeric, Control, Info);
    if (status != 0)
    {
      processUMFError(data,status,"umfpack_di_solve",Control,Info);return JNI_FALSE;
    }

    umfpack_di_free_numeric (&Numeric) ;
    //printf("Memory needed: %g\n",Info[UMFPACK_PEAK_MEMORY_ESTIMATE]);
    return JNI_TRUE;
}

void throwException(JNIEnv *j_env,const char *errorMessage)
{
  assert(j_env != NULL && errorMessage != NULL);

  jclass illegalArgument = (*j_env)->FindClass(j_env,"java/lang/IllegalArgumentException");
  if (illegalArgument != NULL && (*j_env)->ExceptionOccurred(j_env) == NULL)
  {// successfully loaded a class
    (*j_env)->ThrowNew(j_env,illegalArgument,errorMessage);
  }
}

// Releases all the mappings from Java to C arrays.
void unmapJavaArrays(struct solverData * data);

jboolean mapJavaArrays(struct solverData * data)
{
  assert(data != NULL && data->java.j_env != NULL);

  data->n = (*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_Ap)-1;

  if (data->n <= 0) // zero-sized problem
  {
    throwException(data->java.j_env,"zero-sized problem");return JNI_FALSE;
  }

#define MAP_ARRAY(VARNAME,KIND)						\
  data->VARNAME = (*data->java.j_env)->Get##KIND##ArrayElements(data->java.j_env, data->java.j_##VARNAME, NULL); \
  if (data->VARNAME == NULL)						\
  {									\
    unmapJavaArrays(data);						\
    throwException(data->java.j_env,"cannot map " #VARNAME);return JNI_FALSE; \
  }

  MAP_ARRAY(Ap,Int);MAP_ARRAY(Ai,Int);
  MAP_ARRAY(Ax,Double);MAP_ARRAY(b,Double);MAP_ARRAY(x,Double);

  data->nz = data->Ap[data->n];

  if ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_Ai) != 
      ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_Ax)))
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"inconsistent dimensions of  Ai and Ax");return JNI_FALSE;
  }

  if ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_Ax) < data->nz)
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"too few elements in Ax");return JNI_FALSE;
  }

  if ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_Ai) < data->nz)
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"too few elements in Ai");return JNI_FALSE;
  }

  if ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_b) != data->n)
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"inconsistent dimension of  b");return JNI_FALSE;
  }

  if ((*data->java.j_env)->GetArrayLength(data->java.j_env,data->java.j_x) != data->n)
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"inconsistent dimension of  x");return JNI_FALSE;
  }

  if (data->Ap[0] != 0)
  {
    unmapJavaArrays(data);
    throwException(data->java.j_env,"Ap[0] should be 0");return JNI_FALSE;
  }
  return JNI_TRUE;
}

void unmapJavaArrays(struct solverData * data)
{
  assert(data != NULL && data->java.j_env != NULL);

#define RELEASE_ARRAY(VARNAME,KIND)			\
  if (data->VARNAME != NULL) {			\
    (*data->java.j_env)->Release##KIND##ArrayElements(data->java.j_env, data->java.j_##VARNAME, data->VARNAME, 0);data->VARNAME = NULL; \
  }

  RELEASE_ARRAY(Ap,Int);RELEASE_ARRAY(Ai,Int);
  RELEASE_ARRAY(Ax,Double);RELEASE_ARRAY(b,Double);RELEASE_ARRAY(x,Double);
}

void freeMemory()
{
  if (umfpack_Wi != NULL) { free(umfpack_Wi);umfpack_Wi = NULL; }
  if (umfpack_W != NULL) { free(umfpack_W);umfpack_W = NULL; }
}

JNIEXPORT jboolean JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_rpnicore_LSolver_setIRStep)(
   JNIEnv *j_env,        //interface pointer
   jclass j_cls,        // "this" pointer
   jint j_n)
{
  Control_IRSTEP = j_n;
  return JNI_TRUE;
}

JNIEXPORT jboolean JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_rpnicore_LSolver_extmalloc)(
   JNIEnv *j_env,        //interface pointer
   jclass j_cls,        // "this" pointer
   jint j_n,
   jint j_c)
{
  freeMemory();

  umfpack_Wi = (int *)malloc(sizeof(int)*j_n);
  if (umfpack_Wi == NULL)
  {
    freeMemory();
    throwException(j_env,"failed to allocate memory for Wi");return JNI_FALSE;
  }
  umfpack_W = (double *)malloc(sizeof(double)*j_n*j_c);
  if (umfpack_Wi == NULL)
  {
    freeMemory();
    throwException(j_env,"failed to allocate memory for W");return JNI_FALSE;
  }

  return JNI_TRUE;
}

// Solves a column-ordered system of linear equations using UMFPACK.
//
// We need a return value here because we may fail to throw an exception on error. 
//
JNIEXPORT jboolean JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_rpnicore_LSolver_extsolve)(
   JNIEnv *j_env,        //interface pointer
   jclass j_cls,        // "this" pointer
     jintArray j_Ap,
     jintArray j_Ai,
     jdoubleArray j_Ax,
     jdoubleArray j_b,
     jdoubleArray j_x)
{
  struct solverData data;
  assert(j_env != NULL);
  memset(&data,0,sizeof(data));// yes, I know what I'm doing :)


#define SET(NAME) data.java.j_##NAME=j_##NAME
  SET(Ap);SET(Ai);SET(Ax);SET(b);SET(x);
  data.java.j_env = j_env;

  if (mapJavaArrays(&data) == JNI_FALSE)
    return JNI_FALSE;// the exception will have been thrown already.

  if (umfsolve(&data) == JNI_FALSE)
    return JNI_FALSE;

  unmapJavaArrays(&data);

  return JNI_TRUE;
}
