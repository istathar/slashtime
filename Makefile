
# If you need to debug some classpath, includes, or command line arguments
# option, then comment out MAKEFLAGS line below, or set V=1 on the command
# line before make.
#
ifdef V
else
MAKEFLAGS=-s
endif

ifdef D
DEBUG=--debug=all
endif

.PHONY: all run clean distclean

# --------------------------------------------------------------------
# Variable setup. You may want to set your editor to wrap to see the
# full CLASSPATH
# --------------------------------------------------------------------

-include .config

#
# if (and only if) GCJ native works then we build it; if you reach in directly
# and build a deep native target without GCJ being present, that's just silly
# and you deserve the breakage :).
#
ifdef GCJ
all: build/classes build/jni-dist build/native
else
all: build/classes build/jni-dist
endif

# [this  will be called by the above include if .config is missing.
# we don't call ./configure automatically to allow scope for
# manual configuration and overrides]
.config: src/java/com/operationaldynamics/slashtime/Version.java
	echo
	echo "You need to run ./configure to check prerequisites"
	echo "and setup preferences before you can build accounts."
	( if [ ! -x configure ] ; then chmod +x configure ; echo "I just made it executable for you." ; fi )
	echo
	exit 1

# Variables we expect to be set in .config are:
#	JAVAGNOME_JARS
#	JUNIT_JARS
#	JNI_PATH
#	JAVAC[_CMD]	[expected to be 9 chars wide]
#	JAVA[_CMD]	[expected to be 9 chars wide]

# [This is just a quick sanity check]
build/config: build/dirs .config
	@echo "CHECK     build system configuration"
	( if [ ! "$(JAVAGNOME_JARS)" ] ; then echo "Sanity check failed. Run ./configure" ; exit 1 ; fi )
	touch $@

CLASSPATH=$(JAVAGNOME_JARS)

SOURCES_DIST=$(shell find src/java -name '*.java')
SOURCES_JNI=$(shell find src/java -name '*.c')

# [we now go to the bother of listing the .class targets individually in order
# to allow us to use gcj, which doesn't compile all the things it needs to 
# (as javac does) even though it has to find things by scanning. This
# can considerably slow a javac build depending on the order which classes
# are encountered; oh well]
CLASSES_DIST=$(shell echo $(SOURCES_DIST) | sed -e's/\.java/\.class/g' -e's/src\/java/tmp\/classes/g')
OBJECTS_JNI=$(shell echo $(SOURCES_JNI) | sed -e's/\.c/\.o/g' -e's/src\/java/tmp\/jni/g')

# [same thing, but this time the individual .o targets]
NATIVE_DIST=$(shell echo $(SOURCES_DIST) | sed -e's/\.java/\.o/g' -e's/src\/java/tmp\/native/g')

#
# convenience target: setup pre-reqs
#
build/dirs:
	@echo "MKDIR     preping temporary files and build directories"
	-test -d build || mkdir build
	-test -d tmp/classes || mkdir -p tmp/classes
	-test -d tmp/include || mkdir -p tmp/include
	-test -d tmp/jni || mkdir -p tmp/jni
	-test -d tmp/native || mkdir -p tmp/native
	-test -d tmp/launcher || mkdir -p tmp/launcher
	touch $@

# [these are only necessary as a defence against the system having evolved
# since it was ./configured. Java is so bad at identifying the root cause 
# being missing files that were expected that such a safety check helps
# innocent builders maintain their sanity.]
build/check-jars:
	@echo "CHECK     prerequite core jar files"
	( if [ ! "$(CLASSPATH)" ] ; then echo "\"CLASSPATH\" variable is an empty. How did you get here?" ; exit 1 ; fi )
	( for i in `echo $(CLASSPATH) | sed -e's/:/ /g'` ; do if [ ! -f $$i ] ; then echo $$i not found. ; exit 1 ; fi ; done )
	touch $@

# --------------------------------------------------------------------
# Source compilation
# --------------------------------------------------------------------

# [anything Java JVM runtime should depend on this target]
build/classes: build/classes-dist slashtime-java

build/native: build/native-dist slashtime-native

#
# build the sources (that are part of the distributed app)
#
build/classes-dist: build/config build/check-jars $(CLASSES_DIST)
	touch $@

tmp/classes/%.class: src/java/%.java
	@echo "$(JAVAC_CMD) $@"
	$(JAVAC) -d tmp/classes -classpath tmp/classes:$(CLASSPATH):src/java $<


tmp/launcher/launcher.o: src/C/launcher/launcher.c .config
	@echo "$(CC_CMD) $@"
	gcc -g -Wall -o $@ -c $<

slashtime-java: tmp/launcher/launcher.o
	@echo "$(LINK_CMD) $@"
	gcc -g -o $@ $<
	@echo "STRIP     $@"
	strip $@


build/native-dist: build/config build/check-jars $(NATIVE_DIST)
	touch $@

tmp/native/%.o: src/java/%.java
	@echo "$(GCJ_CMD) $@"
	if [ ! -d `dirname $@` ] ; then mkdir -p `dirname $@` ; fi
	$(GCJ) -fjni -classpath $(CLASSPATH):src/java -o $@ -c $<

#
# build JNI library
#

ifdef V
JAVAH:=$(JAVAH) -verbose
endif

build/headers: build/headers-generate
	touch $@

# We don't use an implict rule for this for the simple reason that we
# only want to do one invocation, which means using $? (newer than target).
# It gets more complicated because of the need to give classnames to javah.
build/headers-generate: $(SOURCES_JNI)
	@echo "$(JAVAH_CMD) tmp/headers/*.h"
	$(JAVAH) -d tmp/include -classpath tmp/classes \
		$(shell echo $? | sed -e 's/src\/java\///g' -e 's/\.c//g' -e 's/\//./g')
	touch $@

tmp/jni/%.o: src/java/%.c
	@echo "$(CC_CMD) $@"
	if [ ! -d $(@D) ] ; then mkdir -p $(@D) ; fi
	$(CC) -Itmp/include -o $@ -c $<


build/jni-dist: build/config build/headers tmp/libslashtimejni-0.so
	touch $@

tmp/libslashtimejni-0.so: $(OBJECTS_JNI)
	@echo "$(LINK_CMD) $@"
	$(LINK) -shared -o $@ $(OBJECTS_JNI)

#
# Link executable
#
slashtime-native: build/native-dist build/jni-dist
	@echo "$(GCJ_LINK_CMD) $@"
	$(GCJ) \
		-fjni -O \
		-Wl,-rpath=$(JAVAGNOME_LIB_PATH) \
		-L$(JAVAGNOME_LIB_PATH) \
		-lglibjava -lgtkjava \
		-Wl,-rpath=$(PWD)/tmp \
		-Ltmp \
		-lslashtimejni-0 \
		-Djava.library.path=$(JAVAGNOME_LIB_PATH):$(PWD)/tmp \
		--main=com.operationaldynamics.slashtime.Master -o $@ \
		$(NATIVE_DIST)

# --------------------------------------------------------------------
# Generate documentation
# --------------------------------------------------------------------

ifdef V
JAVADOC=javadoc
WGET=wget
else
JAVADOC=javadoc -quiet
WGET=wget --quiet
REDIRECT=>/dev/null
endif

tmp/javadoc/classpath/package-list: build/dirs-javadoc
	-test -d $(@D) || mkdir -p $(@D)
	@echo "WGET      classpath package-list"
	$(WGET) http://developer.classpath.org/doc/package-list -O $@
	touch $@

tmp/javadoc/java-gnome/package-list: build/dirs-javadoc
	-test -d $(@D) || mkdir -p $(@D)
	@echo "WGET      java-gnome package-list"
	$(WGET) http://java-gnome.sourceforge.net/docs/javadoc/package-list -O $@
	touch $@

build/dirs-javadoc:
	@echo "MKDIR     preping javadoc output directories"
	-test -d tmp/javadoc || mkdir -p tmp/javadoc
	-test -d doc/api || mkdir -p doc/api
	touch $@

doc: build/javadoc
build/javadoc: build/classes-dist build/dirs-javadoc \
		tmp/javadoc/classpath/package-list \
		tmp/javadoc/java-gnome/package-list
	@echo "JAVADOC   src/java/*"
	$(JAVADOC) \
		-d doc/api \
		-classpath tmp/classes:$(JAVAGNOME_JARS) \
		-public \
		-nodeprecated \
		-source 1.4 \
		-notree \
		-noindex \
		-nohelp \
		-version \
		-author \
		-linkoffline http://developer.classpath.org/doc tmp/javadoc/classpath \
		-linkoffline http://java-gnome.sourceforge.net/docs/javadoc tmp/javadoc/java-gnome \
		-sourcepath lib:src/java \
		-subpackages "generic:accounts:country" \
		-doctitle "<h1>SlashTime</h1><p>A timezone program</p>" \
		-windowtitle "slashtime version $(VERSION)" \
		-header "<span style=\"font-family: arial; font-size: small; font-style: normal; colour: gray;\">API documentation for <a class=\"black\" href="http://research.operationaldynamics.com/projects/slashtime/">SlashTime</a>, a timezone program for GNOME</span>" \
		-footer "<img src=\"http://www.operationaldynamics.com/images/logo/logo-60x76.jpg\" style=\"float:left; padding-left:5px; padding-right:10px;\"><img src=\"http://www.operationaldynamics.com/images/logo/type-342x32.jpg\" align=\"right\"><br><p style=\"font-family: arial; font-size: small; font-style: normal; colour: gray; clear: right;\">Copyright &copy; 2006 <a class=\"black\" href=\"http://www.operationaldynamics.com/\">Operational Dynamics</a> Consulting Pty Ltd and others. This code is made available under the terms of the GPL, and patches are accepted. On the other hand, if you wish to see a specific feature developed, we would be happy to discuss terms for a project quote that will meet your needs.</p>" \
		-group "Main GUI program" "com.operationaldynamics.slashtime" \
		-breakiterator $(REDIRECT)
	touch $@


# --------------------------------------------------------------------
# Runtime convenience targets
# --------------------------------------------------------------------

# the point is to *run* these, so we don't touch a stamp file.

run: build/classes build/jni-dist
	@echo "$(JAVA_CMD) SlashTime $(DEBUG)"
	$(JAVA) \
		-Djava.library.path=$(JNI_PATH):tmp \
		-classpath $(CLASSPATH):tmp/classes \
		com.operationaldynamics.slashtime.Master $(DEBUG)

# --------------------------------------------------------------------
# House keeping
# --------------------------------------------------------------------

# [note that we don't remove .config here, as a) darcs doesn't pick it up
# so if it's hanging around it won't cause problems, and b) if it is removed 
# here, then `make clean all` fails]
clean:
	@echo "RM        temporary build directories"
	-rm -rf build
	-rm -rf tmp
	-rm -rf hs_err_*
	@echo "RM        executables and wrappers"
	-rm -f slashtime-native slashtime-java

distclean: clean
	@echo "RM        build configuration information"
	-rm -f .config .config.tmp
	@echo "RM        generated documentation"
	-rm -f doc/api/*

# --------------------------------------------------------------------
# Distribution target
# --------------------------------------------------------------------

# We did have:
#
#	tar cf slashtime-$(VERSION)-dist.tar \
#		--exclude '*.tar*' \
#		[A-Za-z]*
#	mkdir /tmp/slashtime-$(VERSION)
#	tar x -C /tmp/slashtime-$(VERSION) -f slashtime-$(VERSION)-dist.tar 
#	tar cj -C /tmp -f slashtime-$(VERSION).tar.bz2 slashtime-$(VERSION)
#	rm -rf /tmp/slashtime-$(VERSION) slashtime-$(VERSION)-dist.tar
# 
# but switched to using an export from git to allow us to worry less
# about stuff being vaccumed up that shouldn't be. Also means we don't need
# to depend on target distclean.
#
# NOTE THAT IT IS FILES IN GIT'S INDEX FILE THAT WILL BE EXPORTED! This means
# if you have to do a temporary tweak to create a special version for someone
# you can do so by using git-update-index to put the file into the index and
# then running make dist.
#
# Remember that if you bump the version number you need to re-./configure
dist:
	@echo "TAR       distribution tarball"
	# note trailing slash!
	git-checkout-index --prefix=slashtime-$(VERSION)/ -a
	tar cjf slashtime-$(VERSION).tar.bz2 slashtime-$(VERSION)
	rm -rf slashtime-$(VERSION)

# Create a snapshot tarball. Includes source repository in .git
tarball: clean
	@echo "TAR       backup tarball"
	tar cjf slashtime-$(VERSION)-snapshot-`date +%y%m%d`.tar.bz2 \
		--exclude '*.tar.*' --exclude '.config' \
		.

