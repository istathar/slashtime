Slashtime

This is slashtime, a small program which displays the time in various places.


COMPILING AND RUNNING
=====================

For the impatient:

	$ cargo run

but don't forget to create a _tzlist_ file; see [`PLACES`](PLACES).


RUNNING
=======

If you just built it locally, then you can run it in-place:

	$ target/debug/slashtime


Places list
-----------

The cities shown by slashtime is drawn from the list found in a file called
_tzlist_ in the appropriate place in your home directory. See the
[PLACES](PLACES) file for a fully documented example.


HISTORY
=======

Circa 2002, a Debian package called gworldclock came with a short shell script
called tzwatch which somewhat mimicked its output. Both programs were
reasonable enough, but like most timezone things, they displayed the offset
from GMT. We've always found that a little useless. Telling me that I am in
GMT+11 and someone I want to talk to is shown in GMT-4 still doesn't help me
get an intuitive grasp of what the differential is.

So Andrew Cowie wrote a small perl script called `slashtime`. Named after the
short cut on his employer's website that got you to an HTML version, it started
out life as a Perl script, and gained a bit of a cult following from people who
would put it into their `.bash_profiles` to run when launching a terminal.

Sometime in 2006, Andrew got the idea to write a GUI version, taking advantage
of the new java-gnome bindings to create a compact and rich presentation of the
time zone information. The previous version of Slashtime was written using the
java-gnome 4.0 bindings and was first packaged by Gentoo Linux as
`app-misc/slashtime` in 2008.

The program was rewritten in Rust in 2023.

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

