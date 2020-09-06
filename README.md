Slashtime
=========

This is slashtime, a small program which displays the time in various places.

<style>
pre {
    color: white; background: black; padding: 10px; margin: 10px;
}
</style>


COMPILING AND RUNNING
=====================

For the impatient:

	$ stack install

but don't forget to create a `.tzlist` file; see [`PLACES`](PLACES.html).


PREREQUISITES
-------------

slashtime is written in Haskell and uses uses:

* **gi-gtk**  
  Bindings around the GTK and GNOME user interface libraries,  
  You'll need a version `>= 3.0 && < 4`

RUNNING
=======

Places list
-----------

The cities shown by slashtime is drawn from the list found in a file called
.tzlist in your home directory. See the [PLACES](PLACES.html) file for a fully
documented example.

Don't whine to me about the lack of UI to change the hard coded Places list. If
you want your own list of cities, then whip up a `~/.tzlist` file.


HISTORY
=======

Circa 2002, a Debian package called gworldclock came with a short shell script
called tzwatch which somewhat mimicked its output. Both programs were
reasonable enough, but like most timezone things, they displayed the offset
from GMT. We've always found that a little useless.  Telling me that I am in
GMT+11 and someone I want to talk to is shown in GMT-4 still doesn't help me
get an intuitive grasp of what the differential is.

So Andrew Cowie wrote a small perl script called `slashtime`. Named after the
short cut on his then employer's website that gets you to an HTML version,
`operationaldynamics.com/time`, slashtime started out life as a perl script,
and gained a bit of a cult following from people who would put it into their
`.bash_profiles` to run when launching a terminal.

Sometime in 2006, Andrew got the idea to write a GUI version, taking advantage
of the new **java-gnome** bindings to create a compact and rich presentation
of the time zone information. That version of Slashtime, written in Java was
first packaged by Gentoo Linux as `app-misc/slashtime` in June 2008.

In 2020, Andrew again found himself with an international team, and so the
package was revitalized and ported to Haskell using the **gi-gtk** bindings.

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
