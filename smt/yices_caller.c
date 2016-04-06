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
#include <stdlib.h>
#include <unistd.h>

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

void throwException(JNIEnv *j_env,const char *errorMessage)
{
  assert(j_env != NULL && errorMessage != NULL);
  if ((*j_env)->ExceptionCheck(j_env) == JNI_FALSE)
  {// exception can be thrown
	  jclass illegalArgument = (*j_env)->FindClass(j_env,"java/lang/IllegalArgumentException");
	  if (illegalArgument != NULL && (*j_env)->ExceptionOccurred(j_env) == NULL)
	  {// successfully loaded a class
		  (*j_env)->ThrowNew(j_env,illegalArgument,errorMessage);
	  }
  }
}

/** Extracts field ID of the "context" field. */
jfieldID getFieldID(JNIEnv *j_env,jobject obj)
{
	assert (j_env != NULL);

	jclass clazz = NULL;
	if (obj != NULL) clazz = (*j_env)->GetObjectClass(j_env,obj);
	if (clazz == NULL)
	{
		throwException(j_env,"failed to get class of an object");return NULL;
	}

	jfieldID field = (*j_env)->GetFieldID(j_env,clazz,"context","J");
	if (field == NULL)
	{// the exception does not actually get invoked because by this time Java has already thrown an exception.
		throwException(j_env,"failed to find field \"context\"");return NULL;
	}

	return field;
}

/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    testGetField
 * Signature: (Ljava/lang/Object;)Z
 */
JNIEXPORT jboolean JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_testGetField) (JNIEnv *j_env, jclass clazz, jobject obj)
{
	return getFieldID(j_env,obj) != NULL;
}

/** Configures Yices.
 *
 * Class:     statechum_analysis_learning_Smt
 * Method:    configYices
 * Signature: (IZ)V
 */
JNIEXPORT void JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_configureYices) (JNIEnv *j_env, jclass smtClass, jint verbosity, jboolean typecheck)
{
	yicesl_set_verbosity(verbosity);
	yicesl_enable_type_checker(typecheck == JNI_TRUE?1:0);
}

/** Extracts Yices context from a field. */
yicesl_context getContext(JNIEnv *j_env, jobject obj)
{
	jfieldID field = getFieldID(j_env,obj);if (field == NULL) return NULL;
	jlong context = (*j_env)->GetLongField(j_env,obj,field);
	if (context == 0)
	{// not yet assigned.
		context = (jlong)yicesl_mk_context();
		(*j_env)->SetLongField(j_env,obj,field,context);
	}

	return (yicesl_context) context;
}

/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    releaseContext
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_releaseContext) (JNIEnv *j_env, jobject smtObj)
{
	jfieldID field = getFieldID(j_env,smtObj);if (field == NULL) return;
	jlong context = (*j_env)->GetLongField(j_env,smtObj,field);
	if (context != 0)
	{
		yicesl_del_context((yicesl_context) context);
		(*j_env)->SetLongField(j_env,smtObj,field,0);
	}
}

/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    check
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_check) (JNIEnv *j_env, jobject smtObj)
{
	yicesl_context context = getContext(j_env,smtObj);
	if (context == NULL) return JNI_FALSE;// exception should be already pending.
	return yicesl_inconsistent(context)?JNI_FALSE:JNI_TRUE;
}

static const char * errCannotHandleString = "failed to handle text in a string", * errColon = " : ";

/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    loadData
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_loadData) (JNIEnv *j_env, jobject smtObj, jstring str)
{
	yicesl_context context = getContext(j_env,smtObj);
	const char * text = NULL;
	int parseResult = 0;

	if (context == NULL) return;// exception should be already pending.
	text = (*j_env)->GetStringUTFChars(j_env,str,NULL);
	if (text == NULL)
	{
		throwException(j_env,"failed to extract data from a string");return;
	}

	parseResult = yicesl_read(context,text);
	(*j_env)->ReleaseStringUTFChars(j_env,str,text);

	if (!parseResult)
	{
		char *msg=yicesl_get_last_error_message(), *errorMessage = NULL;
		if (msg != NULL)
		{
			int errTextLen = strlen(errCannotHandleString), errColonLen = strlen(errColon);
			int len = strlen(msg);errorMessage = malloc(len+1+errTextLen+errColonLen);
			if (errorMessage != NULL)
			{
				strcpy(errorMessage,errCannotHandleString);strcpy(errorMessage+errTextLen,errColon);strcpy(errorMessage+errTextLen+errColonLen,msg);
			}
		}

		if (errorMessage != NULL)
			throwException(j_env,errorMessage);
		else
			throwException(j_env,errCannotHandleString);

		// No idea if I should release msg, but I think I should not release errorMessage
	}

	//(*j_env)->IsInstanceOf(j_env,jobject,(*j_env)->FindClass(j_env,"java/lang/String"))
}

int savedFD = -1;
const int stdOut = 1;
/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    releaseContext
 * Signature: ()V
 */
JNIEXPORT void JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_closeStdOut) (JNIEnv *j_env, jclass smtClass)
{
	savedFD = dup(stdOut);if (savedFD >= 0) close(stdOut);
}

/*
 * Class:     statechum_analysis_learning_Smt
 * Method:    releaseContext
 * Signature: ()V
 */
JNIEXPORT void JNICALL
STDCALLFUDGE(Java_statechum_analysis_learning_Smt_reopenStdOut) (JNIEnv *j_env, jclass smtClass)
{
	if (savedFD >= 0)
	{
		dup2(savedFD,stdOut);savedFD = -1;
	}
}



