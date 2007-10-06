#!/bin/sh
#
# Copyright (c) 2006 Operational Dynamics Consulting Pty Ltd
# See LICENCE file for usage and redistribution terms
#

#
# Run slashtime in a the Java VM as set by ./configure
#
# Note that no assumption or test is made about the state of the class files -
# presumably they exist and are up to date but this is not enforced (in order
# to avoid the cost of calling `make`).
#
# Obviously this needs to be made location independent and installable... for
# the mean time, you can run this script and background the program by doing:
#
#   $ ./slashtime-java &
#
# or
#
#   $ src/joe/slashtime-0.3.6/slashtime-java &
#
# You get the idea.
#

cd `dirname $_`

#
# Do at least a cursory investigation that things are in order.
#
if [ ! -f .config ] ; then
	echo "You need to run ./configure and make in $PWD first!"
	exit 1;
fi

if [ ! -f tmp/classes/com/operationaldynamics/slashtime/Master.class ] ; then
	echo "This is a wrapper to execute slashtime in a Java VM;"
	echo "You need to compile the code first! Run make."
	exit 1;
fi

#
# source all the Make variables as Shell variables
#
eval `cat .config | sed -e '/^#/d' -e '/^$/d' -e 's/=/="/' -e 's/$/"/'`

#
# and run Java appropriately.
#
exec $JAVA \
	-classpath $JAVAGNOME_JARS:tmp/classes \
	-Djava.library.path=$JNI_PATH:tmp \
	com.operationaldynamics.slashtime.Master
