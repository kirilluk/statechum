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

#include <stdio.h>
#include <assert.h>
#include <malloc.h>
	

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <memory.h>
#include "smt.h"
#include <yices_c.h>
#include <yicesl_c.h>

JNIEXPORT jint JNICALL
STDCALLFUDGE(JNI_OnLoad)
(JavaVM *vm, void *reserved)
{
  return JNI_VERSION_1_4;
}

JNIEXPORT void JNICALL
STDCALLFUDGE(JNI_OnUnload)(JavaVM *vm, void *reserved)
{
}



