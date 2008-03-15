
# If you need to debug some classpath, includes, or command line arguments
# option, then comment out MAKEFLAGS line below, or set V=1 on the command
# line before make.
#
ifdef V
else
MAKEFLAGS=-s
endif

.PHONY: all dirs clean distclean

-include .config

all: .config dirs tmp/stamp/compile slashtime

.config: src/java/com/operationaldynamics/slashtime/Version.java
	echo
	echo "You need to run ./configure to check prerequisites"
	echo "and setup preferences before you can build slashtime."
	( if [ ! -x configure ] ; then chmod +x configure ; echo "I just made it executable for you." ; fi )
	echo
	exit 1


CLASSPATH=$(JAVAGNOME_JARS)

SOURCES_DIST=$(shell find src/java -name '*.java')

dirs: tmp/classes tmp/stamp

tmp/classes:
	@echo -e "MKDIR\t$@"
	mkdir $@

tmp/stamp:
	@echo -e "MKDIR\t$@"
	mkdir $@

# --------------------------------------------------------------------
# Source compilation
# --------------------------------------------------------------------

#
# build the sources (that are part of the distributed app)
#

tmp/stamp/compile: $(SOURCES_DIST)
	@echo -e "$(JAVAC_CMD)\ttmp/classes/*.class"
	$(JAVAC) -d tmp/classes -classpath tmp/classes:$(CLASSPATH) -sourcepath src/java $^
	touch $@

slashtime: tmp/launcher/slashtime-local
	@echo -e "CP\t$@"
	cp -f $< $@
	chmod +x $@

# --------------------------------------------------------------------
# Installation
# --------------------------------------------------------------------

install: all \
		$(DESTDIR)$(PREFIX) \
		$(DESTDIR)$(PREFIX)/share/java/slashtime-$(APIVERSION).jar \
		tmp/stamp/install-pixmaps \
		$(DESTDIR)$(PREFIX)/share/applications/slashtime.desktop \
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

$(DESTDIR)$(PREFIX)/share/applications:
	@echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/bin/slashtime: \
		$(DESTDIR)$(PREFIX)/bin \
		tmp/launcher/slashtime-install
	@echo -e "INSTALL\t$@"
	cp -f tmp/launcher/slashtime-install $@
	chmod +x $@

$(DESTDIR)$(PREFIX)/share/applications/slashtime.desktop: \
		$(DESTDIR)$(PREFIX)/share/applications \
		tmp/launcher/slashtime.desktop
	@echo -e "INSTALL\t$@"
	cp -f tmp/launcher/slashtime.desktop $@

tmp/slashtime.jar: tmp/stamp/compile
	@echo -e "$(JAR_CMD)\t$@"
	$(JAR) -cf tmp/slashtime.jar -C tmp/classes .


$(DESTDIR)$(PREFIX)/share/pixmaps: 
	@echo -e "MKDIR\t$@/"
	-mkdir $@

tmp/stamp/install-pixmaps: \
		$(DESTDIR)$(PREFIX)/share/pixmaps \
		share/pixmaps/*.png
	@echo -e "INSTALL\t$(DESTDIR)$(PREFIX)/share/pixmaps/*.png"
	cp -f share/pixmaps/*.png $(DESTDIR)$(PREFIX)/share/pixmaps
	touch $@

$(DESTDIR)$(PREFIX)/share/java/slashtime-$(APIVERSION).jar: \
		$(DESTDIR)$(PREFIX)/share/java \
		tmp/slashtime.jar
	@echo -e "INSTALL\t$@"
	cp -f tmp/slashtime.jar $@


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
	@echo -e "TAR\tslashtime-$(VERSION).tar.bz2"
	tar cjf slashtime-$(VERSION).tar.bz2 -C tmp slashtime-$(VERSION)
	rm -r tmp/slashtime-$(VERSION)

