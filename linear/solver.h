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

#ifndef SOLVER_H
#define SOLVER_H 1

/* The following line is based on http://www.vtk.org/pipermail/vtkusers/2006-May/085047.html */
typedef long long __int64;

#include <jni.h>

#define ERRMSG_MAX 4096

typedef int SOLVER_INT;

struct solverData
{
  jsize  n;
  jsize  nz;
  jint  *  Ap;
  jint  *  Ai;
  jdouble * Ax;
  jdouble * b;

  /** Stores the solution. */
  jdouble * x;

  /** Stores the description of the outcome. */
  char *message[ERRMSG_MAX];

  struct java_arrays
  {
    jintArray j_Ap;
    jintArray j_Ai;
    jdoubleArray j_Ax;
    jdoubleArray j_b;
    jdoubleArray j_x;

    /** Reference to the JVM. */
    JNIEnv *j_env;
  } java;
  
};

/** Throws Java exception with the supplied error message. Does nothing if the exception cannot be thrown. */
void throwException(JNIEnv *j_env,const char *errorMessage);

/** Solves the system of equations supplied in the array data and
 * returns true on success, false on failure. 
 * Upon a failure, the error
 * string is filled in (data->message).
 */
jboolean umfsolve(struct solverData *data);

int * umfpack_Wi;
double *umfpack_W;

/** Frees the above two arrays. */
void freeMemory();

jint Control_IRSTEP;

/** Whether I'm using di_wsolve or di_solve. */
jboolean useWorkingMemory;

/** Cygwin does not add underscores to names on win32, hence we have to add them ourselves. */
#if defined(__CYGWIN__)
#define STDCALLFUDGE(NAME) _##NAME
#else
#define STDCALLFUDGE(NAME) NAME
#endif


#endif /* !SOLVER_H */


