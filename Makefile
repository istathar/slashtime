
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

all: setup build

setup: 
	test -d tmp/stamp || mkdir -p tmp/stamp

build: .config tmp/stamp/dirs tmp/stamp/compile slashtime

.config: src/java/com/operationaldynamics/slashtime/Version.java
	echo
	echo "You need to run ./configure to check prerequisites"
	echo "and setup preferences before you can build slashtime."
	( if [ ! -x configure ] ; then chmod +x configure ; echo "I just made it executable for you." ; fi )
	echo
	exit 1


CLASSPATH=$(JAVAGNOME_JARS)

SOURCES_DIST=$(shell find src/java -name '*.java')

tmp/stamp/dirs:
	@echo -e "MKDIR\tpreping temporary files and build directories"
	-test -d tmp/classes || mkdir -p tmp/classes
	-test -d tmp/stamp || mkdir -p tmp/stamp
	-test -d tmp/launcher || mkdir -p tmp/launcher
	touch $@

# --------------------------------------------------------------------
# Source compilation
# --------------------------------------------------------------------

#
# build the sources (that are part of the distributed app)
#

tmp/stamp/compile: $(SOURCES_DIST)
	@echo -e "$(JAVAC_CMD)\tsrc/java/*.java"
	$(JAVAC) -d tmp/classes -classpath tmp/classes:$(CLASSPATH) -sourcepath src/java $^
	touch $@

slashtime: tmp/launcher/slashtime-local
	@echo -e "INSTALL\t$@"
	cp -f $< $@
	chmod +x $@

# --------------------------------------------------------------------
# Installation
# --------------------------------------------------------------------

install: all \
		$(DESTDIR)$(PREFIX) \
		$(DESTDIR)$(PREFIX)/bin \
		$(DESTDIR)$(PREFIX)/share/java \
		$(DESTDIR)$(PREFIX)/share/java/slashtime.jar \
		$(DESTDIR)$(PREFIX)/share/pixmaps \
		$(DESTDIR)$(PREFIX)/bin/slashtime

$(DESTDIR)$(PREFIX):
	@echo -e "MKDIR\t$(DESTDIR)$(PREFIX)/"
	-mkdir -p $(DESTDIR)$(PREFIX)

$(DESTDIR)$(PREFIX)/bin:
	@echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/share/java:
	@echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/bin/slashtime: tmp/launcher/slashtime-install
	@echo -e "INSTALL\t$@"
	cp -f $< $@
	chmod +x $@


tmp/slashtime.jar: tmp/stamp/compile
	@echo -e "$(JAR_CMD)\t$@"
	$(JAR) -cf tmp/slashtime.jar -C tmp/classes .

$(DESTDIR)$(PREFIX)/share/pixmaps: share/pixmaps/*.png
	@echo -e "MKDIR\t$@/"
	-mkdir $@
	@echo -e "INSTALL\t$@"
	cp -f $^ $@

$(DESTDIR)$(PREFIX)/share/java/slashtime.jar: tmp/slashtime.jar
	@echo -e "INSTALL\t$@"
	cp -f $< $@


# --------------------------------------------------------------------
# House keeping
# --------------------------------------------------------------------

# [note that we don't remove .config here, as a) darcs doesn't pick it up
# so if it's hanging around it won't cause problems, and b) if it is removed 
# here, then `make clean all` fails]
clean:
	@echo -e "RM\ttemporary build directories"
	-rm -rf tmp/classes
	-rm -rf tmp/stamp
	-rm -rf hs_err_*
	@echo -e "RM\texecutables and wrappers"
	-rm -f tmp/slashtime.jar
	-rm -f slashtime

distclean: clean
	@echo -e "RM\tbuild configuration information"
	-rm -f .config .config.tmp
	-rm -rf tmp/
#	@echo "RM        generated documentation"
#	-rm -f doc/api/*

# --------------------------------------------------------------------
# Distribution target
# --------------------------------------------------------------------

#
# Remember that if you bump the version number you need to commit the change
# and re-./configure before being able to run this! On the other hand, we
# don't have to distclean before calling this.
#
dist: all
	@echo -e "CHECK\tfully committed state"
	bzr diff > /dev/null || ( echo -e "\nYou need to commit all changes before running make dist\n" ; exit 4 )
	@echo -e "EXPORT\ttmp/slashtime-$(VERSION)"
	-rm -rf tmp/slashtime-$(VERSION)
	bzr export --format=dir tmp/slashtime-$(VERSION)
	@echo -e "RM\tnon essential files"
	rm -r tmp/slashtime-$(VERSION)/web
	@echo -e "TAR\tslashtime-$(VERSION).tar.bz2"
	tar cjf slashtime-$(VERSION).tar.bz2 -C tmp slashtime-$(VERSION)
	rm -r tmp/slashtime-$(VERSION)

