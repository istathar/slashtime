
# If you need to debug some classpath, includes, or command line arguments
# option, then comment out MAKEFLAGS line below, or set V=1 on the command
# line before make.
#
ifdef V
else
MAKEFLAGS=-s
endif

.PHONY: all dirs compile translation install clean distclean

-include .config

all: .config dirs compile translation slashtime

.config: src/java/slashtime/client/Version.java
	/bin/echo
	/bin/echo "You need to run ./configure to check prerequisites"
	/bin/echo "and setup preferences before you can build slashtime."
	( if [ ! -x configure ] ; then chmod +x configure ; /bin/echo "I just made it executable for you." ; fi )
	/bin/echo
	exit 1


CLASSPATH=$(JAVAGNOME_JARS)

SOURCES_DIST=$(shell find src/java -name '*.java' | sort)
TRANSLATIONS=$(shell find po/ -name '*.po' | sed -e 's/po\/\(.*\)\.po/share\/locale\/\1\/LC_MESSAGES\/slashtime\.mo/g')


dirs: tmp/classes tmp/stamp tmp/i18n

tmp/classes:
	@/bin/echo -e "MKDIR\t$@"
	mkdir $@

tmp/stamp:
	@/bin/echo -e "MKDIR\t$@"
	mkdir $@

tmp/i18n:
	@/bin/echo -e "MKDIR\t$@"
	mkdir $@

# --------------------------------------------------------------------
# Source compilation
# --------------------------------------------------------------------

#
# build the sources (that are part of the distributed app)
#

compile: tmp/stamp/compile
tmp/stamp/compile: $(SOURCES_DIST)
	@/bin/echo -e "$(JAVAC_CMD)\ttmp/classes/*.class"
	$(JAVAC) -d tmp/classes -encoding UTF-8 -classpath tmp/classes:$(CLASSPATH) -sourcepath src/java $^
	touch $@


translation: tmp/i18n/slashtime.pot $(TRANSLATIONS)

# strictly speaking, not necessary to generate the .pot file, but this has to
# go somewhere and might as well get it done

tmp/i18n/slashtime.pot: $(SOURCES_DIST)
	@/bin/echo -e "EXTRACT\t$@"
	xgettext -o $@ --omit-header --from-code=UTF-8 --keyword=_ --keyword=N_ $^

share/locale/%/LC_MESSAGES/slashtime.mo: po/%.po
	mkdir -p $(dir $@)
	@/bin/echo -e "MSGFMT\t$@"
	msgfmt -o $@ $<


slashtime: tmp/launcher/slashtime-local
	@/bin/echo -e "CP\t$@"
	cp -f $< $@
	chmod +x $@


# --------------------------------------------------------------------
# Installation
# --------------------------------------------------------------------

install: all \
		$(DESTDIR)$(JARDIR)/slashtime-$(APIVERSION).jar \
	 	tmp/stamp/install-pixmaps \
	 	tmp/stamp/install-translations \
		$(DESTDIR)$(PREFIX)/share/applications/slashtime.desktop \
		$(DESTDIR)$(PREFIX)/bin/slashtime

$(DESTDIR)$(PREFIX):
	@/bin/echo -e "MKDIR\t$(DESTDIR)$(PREFIX)/"
	-mkdir -p $(DESTDIR)$(PREFIX)

$(DESTDIR)$(PREFIX)/bin:
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(JARDIR):
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/share/applications:
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/bin/slashtime: \
		$(DESTDIR)$(PREFIX)/bin \
		tmp/launcher/slashtime-install
	@/bin/echo -e "INSTALL\t$@"
	cp -f tmp/launcher/slashtime-install $@
	chmod +x $@

$(DESTDIR)$(PREFIX)/share/applications/slashtime.desktop: \
		$(DESTDIR)$(PREFIX)/share/applications \
		tmp/launcher/slashtime.desktop
	@/bin/echo -e "INSTALL\t$@"
	cp -f tmp/launcher/slashtime.desktop $@

tmp/slashtime.jar: tmp/stamp/compile
	@/bin/echo -e "$(JAR_CMD)\t$@"
	$(JAR) -cf tmp/slashtime.jar -C tmp/classes .

$(DESTDIR)$(PREFIX)/share/slashtime/images: 
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/share/icons/hicolor/48x48/apps: 
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

$(DESTDIR)$(PREFIX)/share/locale: 
	@/bin/echo -e "MKDIR\t$@/"
	-mkdir -p $@

tmp/stamp/install-pixmaps: \
		$(DESTDIR)$(PREFIX)/share/icons/hicolor/48x48/apps \
		$(DESTDIR)$(PREFIX)/share/slashtime/images \
		share/icons/hicolor/48x48/apps/*.png \
		share/slashtime/images/*.png
	@/bin/echo -e "INSTALL\t$(DESTDIR)$(PREFIX)/share/icons/hicolor/48x48/apps/*.png"
	cp -f share/icons/hicolor/48x48/apps/*.png $(DESTDIR)$(PREFIX)/share/icons/hicolor/48x48/apps
	@/bin/echo -e "INSTALL\t$(DESTDIR)$(PREFIX)/share/slashtime/images/*.png"
	cp -f share/slashtime/images/*.png $(DESTDIR)$(PREFIX)/share/slashtime/images
	touch $@

tmp/stamp/install-translations: \
		$(DESTDIR)$(PREFIX)/share/locale \
		share/locale/*/LC_MESSAGES/slashtime.mo
	@/bin/echo -e "INSTALL\t$(DESTDIR)$(PREFIX)/share/locale/*/LC_MESSAGES/slashtime.mo"
	cp -af share/locale/* $(DESTDIR)$(PREFIX)/share/locale
	touch $@

$(DESTDIR)$(JARDIR)/slashtime-$(APIVERSION).jar: \
		$(DESTDIR)$(JARDIR) \
		tmp/slashtime.jar
	@/bin/echo -e "INSTALL\t$@"
	cp -f tmp/slashtime.jar $@
	@/bin/echo -e "SYMLINK\t$(@D)/slashtime.jar -> slashtime-$(APIVERSION).jar"
	cd $(@D) && rm -f slashtime.jar && ln -s slashtime-$(APIVERSION).jar slashtime.jar


# --------------------------------------------------------------------
# House keeping
# --------------------------------------------------------------------

# [note that we don't remove .config here, as a) darcs doesn't pick it up
# so if it's hanging around it won't cause problems, and b) if it is removed 
# here, then `make clean all` fails]
clean:
	@/bin/echo -e "RM\ttemporary build directories"
	-rm -rf tmp/classes
	-rm -rf tmp/stamp
	-rm -rf hs_err_*
	@/bin/echo -e "RM\texecutables and wrappers"
	-rm -f tmp/slashtime.jar
	-rm -f slashtime
	@/bin/echo -e "RM\tgenerated message files"
	-rm -rf share/locale

distclean: clean
	@/bin/echo -e "RM\tbuild configuration information"
	-rm -f .config .config.tmp
	-rm -rf tmp/
#	@/bin/echo "RM        generated documentation"
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
	@/bin/echo -e "CHECK\tfully committed state"
	bzr diff > /dev/null || ( /bin/echo -e "\nYou need to commit all changes before running make dist\n" ; exit 4 )
	@/bin/echo -e "EXPORT\ttmp/slashtime-$(VERSION)"
	-rm -rf tmp/slashtime-$(VERSION)
	bzr export --format=dir tmp/slashtime-$(VERSION)
	@/bin/echo -e "TAR\tslashtime-$(VERSION).tar.bz2"
	tar cjf slashtime-$(VERSION).tar.bz2 -C tmp slashtime-$(VERSION)
	rm -r tmp/slashtime-$(VERSION)

