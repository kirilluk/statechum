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

#ifndef SMT_H
#define SMT_H 1

/* The following line is based on http://www.vtk.org/pipermail/vtkusers/2006-May/085047.html */
typedef long long __int64;

#include <jni.h>

#define ERRMSG_MAX 4096
/** Throws Java exception with the supplied error message. Does nothing if the exception cannot be thrown. */
void throwException(JNIEnv *j_env,const char *errorMessage);

/** Cygwin does not add underscores to names on win32, hence we have to add them ourselves. */
#if defined(__CYGWIN__)
#define STDCALLFUDGE(NAME) _##NAME
#else
#define STDCALLFUDGE(NAME) NAME
#endif


#endif /* !SMT_H */

