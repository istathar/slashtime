/*
 * NativeTime.c
 * 
 * Copyright (c) 2006-2007 Operational Dynamics Consulting Pty Ltd
 * 
 * The code in this file, and the program it is a part of, are made available
 * to you by the authors under the terms of the "GNU General Public Licence,
 * version 2" See the LICENCE file for the terms governing usage and
 * redistribution.
 */

#include <jni.h>
#include <stdlib.h>
#include <time.h>
#include "com_operationaldynamics_slashtime_NativeTime.h"

#define MAXWIDTH 100


/*
 * Class:     com_operationaldynamics_slashtime_NativeTime
 * Method:    tzset
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT void JNICALL Java_com_operationaldynamics_slashtime_NativeTime_tzset
  (JNIEnv *env, jclass klass, jstring _zoneinfo)
{
	/*
	 * Carry out the magic to switch zones by calling tzset(). It doesn't
	 * have parameters - it pulls TZ from the environment.
	 */

	const char *zoneinfo;
	int ok;

	zoneinfo = (*env)->GetStringUTFChars(env, _zoneinfo, NULL);
	if (zoneinfo == NULL) {
		return; /* OutOfMemoryError already thrown */
	}

	ok = setenv("TZ", zoneinfo, 1);

	(*env)->ReleaseStringUTFChars(env, _zoneinfo, zoneinfo);
	if (ok != 0) {
		// throw exception
		return;
	}

	tzset();
}

/*
 * Class:     com_operationaldynamics_slashtime_NativeTime
 * Method:    strftime
 * Signature: (Ljava/lang/String;J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_operationaldynamics_slashtime_NativeTime_strftime
	(JNIEnv *env, jclass klass, jstring _format, jlong _timestamp)
{

	/*
	 * Call strftime() to generate the string in the desired format. We
	 * pass in size as the max, and the return value indicates how much was
	 * used.
	 */

	const char *format;
	size_t size;
	char buf[MAXWIDTH];
	struct tm *brokendown;
	time_t timestamp;

	size = MAXWIDTH;

	format = (*env)->GetStringUTFChars(env, _format, NULL);
	if (format == NULL) {
		return NULL; /* OutOfMemoryError already thrown */
	}

	// is this 2038 safe?!? We're passing in a long seconds, but what's time_t?
	timestamp = (time_t) _timestamp;

	brokendown = localtime(&timestamp);

	size = strftime(buf, size, format, brokendown);

	(*env)->ReleaseStringUTFChars(env, _format, format);
	if (size == 0) {
		// throw exception instead!
		return (*env)->NewStringUTF(env, "Nothing returned!\0");
	}

	return (*env)->NewStringUTF(env, buf);
}

/*
 * Class:     com_operationaldynamics_slashtime_NativeTime
 * Method:    mktime
 * Signature: (IIIIII)J
 */
JNIEXPORT jlong JNICALL Java_com_operationaldynamics_slashtime_NativeTime_mktime
  (JNIEnv *env, jclass klass, jint _year, jint _month, jint _day, jint _hour, jint _minute, jint _second)
{
	struct tm *brokendown;
	time_t timestamp;
	brokendown = calloc(1, sizeof(struct tm));

	brokendown->tm_year = _year - 1900;
	brokendown->tm_mon = _month - 1;
	brokendown->tm_mday = _day;
	brokendown->tm_hour = _hour;
	brokendown->tm_min = _minute;
	brokendown->tm_sec = _second;

	timestamp = mktime(brokendown);

#ifdef DEBUG
	fprintf(stderr, "JNI: %s\n", getenv("TZ"));
	size_t size;
	char buf[MAXWIDTH];
	strftime(buf, size, "%a, %d %b %Y %H:%M:%S %z %Z", localtime(&timestamp));
	fprintf(stderr, "JNI: %d; %s and %d\n", (int) timestamp, buf, brokendown->tm_isdst);
	fflush(stderr);
#endif

	/*
	 * Bizarre bug that mktime adds an hour of DST to the displayed time if in DST.
	 */
	if (brokendown->tm_isdst == 1) {
		timestamp -= 3600;
	}

	free(brokendown);
		
	return (jlong) timestamp;
}
