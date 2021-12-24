Slashtime

This is slashtime, a small program which displays the time in various places.

<style>
pre {
    color: white; background: black; padding: 10px; margin: 10px;
}
</style>


COMPILING AND RUNNING
=====================

For the impatient:

	$ ./configure
	$ make

but don't forget to create a `tzlist` file; see [`PLACES`](PLACES).

There is also the old Perl script which is a standalone command line version,
it doesn't need building, just copying somewhere where binaries live:

	$ cp slashtime.pl ~/.local/bin/

PREREQUISITES
-------------

slashtime is written in Java and uses uses:

* **java-gnome**  
  Bindings around the GTK and GNOME user interface libraries,  
  <http://java-gnome.sourceforge.net>  
  You'll need a version `>= 4.1.1`


CONFIGURATION
-------------

The top level directory contains a custom `./configure` which detects your
operating system variant, sets defaults accordingly, verifies the location of
prerequisites, and finally chooses a Java bytecode compiler and Java virtual
machine runtime environment.

The configuration output is a makefile fragment which is written to .config
and subsequently included by the top level Makefile.

Your configuration is persistent across builds in that checkout - ie, `make
clean` won't force you to reconfigure (though `make distclean` will). The
configure script runs very quickly, so it's no big deal if you have to re run
it.

You can override the choices configure makes by listing parameters on the
command line, like this:

	$ ./configure compiler=javac runtime=jamvm

This facilitates easily switching between runtimes and compilers for testing.
At the moment, the available selections are:

* compiler ->	javac, ecj

* runtime  ->	java, cacao, jamvm, cacao

At last check, Gentoo Linux is fully configured, with it being a good bet to
run on Debian Linux, Fedora Core Linux, and Solaris Unix.  If you are running a
different operating system or distribution, please contact us and we'll add it
-- it's just a matter of identifying the location of a few things. Better yet,
look in the configure Perl script -- the places where OS is switched are
obvious, and just add what you need to add, and send us a patch.

The whole point of configure is to figure things out for you, but if it can't
quite figure out where Java is, you can override it by specifying an alternate
location to find a JDK using either of the following:

* jdk			(where to find a traditional Java Development Kit,
			 ie JAVA_HOME)

* java-gnome		(prefix of an alternate java-gnome install)

* jamvm			(path to the jamvm executable)

* prefix		(system installation prefix)

Examples:

	$ ./configure
	$ ./configure jdk=/opt/sun-jdk-bin-1.6.0.10 java-gnome=/usr/local
	$ ./configure jamvm=/home/joe/custom/bin/jamvm runtime=jamvm

If you're having trouble with something as Make runs and need to debug it, you
can try:

	$ V=1 make

which will show you the actual commands being executed by Make (ie, Make's
normal behaviour, which we override for appearances sake).

INSTALLING
----------

Really, that's your Linux distribution's job, but if you insist:

	# make install

will bludgeon its way into the proper places on the system. If you need to set
the prefix to something else, use the configure script. You can likewise
specify a temporary build directory to compose the installation image, perhaps:


	$ ./configure prefix=/usr
	# make DESTDIR=/var/tmp/portage/app-misc/slashtime-0.5.9/image install


RUNNING
=======

If you just built it locally, then you can run it in-place:

	$ ./slashtime

Or, if you installed it somewhere:

	$ /opt/local/bin/slashtime

If your distro packaged it for you, then it's most likely on your PATH as:

	$ slashtime

and it will be available from the Accessories part of the GNOME Applications
menu.


Places list
-----------

The cities shown by slashtime is drawn from the list found in a file located
at $HOME/.config/slashtime/tzlist. See the [PLACES](PLACES) file for a fully
documented example.

Don't whine to me about the lack of UI to change the hard coded Places list. If
you want your own list of cities, then whip up a `~/.config/slashtime/tzlist`
file.


HISTORY
=======

Circa 2002, a Debian package called gworldclock came with a short shell script
called tzwatch which somewhat mimicked its output. Both programs were
reasonable enough, but like most timezone things, they displayed the offset
from GMT. We've always found that a little useless.  Telling me that I am in
GMT+11 and someone I want to talk to is shown in GMT-4 still doesn't help me
get an intuitive grasp of what the differential is.

So Andrew Cowie wrote a small perl script called `slashtime`. Named after the
short cut on his employer's website that gets you to an HTML version,
[`operationaldynamics.com/time`](http://operationaldynamics.com/time), it
started out life as a perl script, and gained a bit of a cult following from
people who would put it into their `.bash_profiles` to run when launching a
terminal.

Sometime in 2006, Andrew got the idea to write a GUI version, taking advantage
of the new java-gnome bindings to create a compact and rich presentation of the
time zone information. The current version of Slashtime, using java-gnome 4.0,
was first packaged by Gentoo Linux as `app-misc/slashtime` in June 2008.

FAQ
===

What's with the white, gray, and black?
---------------------------------------

White: business hours
Gray: civil hours (it is still "civilized" to call someone at that hour)
Black: night time

What's with the day rolling over at 01:30?
------------------------------------------

Simple: that's when hackers go to bed! So black at the bottom means that
someone hard core may still be reachable online if they're up working, whereas
black on top really means they're asleep.

AfC
