# Slashtime

This is **slashtime**, a small program which displays the time in various
places. What differentiates it from other world clock programs is that
Slashtime shows the offset from your current location, not from UTC.

# CONFIGURATION


## Places list

The cities shown by Slashtime are drawn from the list found in a file called
_tzlist_ in the appropriate place in your home directory, most likely
_~/.config/slashtime/tzlist_.

For each city you wish to see, its location is given a line listing the IANA
timezone name, the name you actually want to use, and the country it is in.

```
"Asia/Calcutta"	"Bangalore"	"India"
```

This answers another common critique of programs displaying timezones: the
IANA timezone database is filed by continent then capital cities, which
ignores country and tends not to actually be the actual place you want to see
in your list. The _tzlist_ configuration file gives you control over both.

See the [PLACES](PLACES) file for a fully documented example.

# COMPILING AND RUNNING

## Building

For the impatient:

    $ cargo build
    $ cargo run --bin slashtime-gui

but don't forget to create a _tzlist_ file first.

## Installing

If you just built it locally, then you can run it in-place:

    $ target/debug/slashtime-gui

but ideally you would install to your user's directory

    $ cargo install --path .

## HISTORY

Circa 2002, a Debian package called **gworldclock** came with a short shell
script called _tzwatch_ which somewhat mimicked its output. Both programs were
reasonable enough for their day, but like most timezone things, they displayed
the offset from GMT. We've always found that a little useless. Telling you
that you are in GMT+11 and someone you want to talk to is in GMT-4 doesn't
really help you get an intuitive grasp of what the differential is.

So Andrew Cowie wrote a small perl script called _slashtime_, named after the
short cut on his then website that got you to an HTML version of it. Slashtime
started out life as a Perl script, and gained a bit of a cult following from
people who would put it into their _.bash_profiles_ to run when launching a
terminal.

Sometime in 2006, Andrew got the idea to write a GUI version, taking advantage
of the new **java-gnome** bindings to create a compact and rich presentation
of the time zone information. That version of Slashtime was written using the
java-gnome 4.0 bindings and was first packaged by Gentoo Linux as
`app-misc/slashtime` in 2008.

After a long hiatus, the program was rewritten in Rust in 2023, and ported
from **gtk4-rs** to **egui** in 2026. It 's now available as a single binary
you can download from the GitHub Releases page.

FAQ
===

What's with the white, gray, and black?
---------------------------------------

White: business hours  
Gray: civil hours (it is still "civilized" to call someone at that hour)  
Black: night time

What's with the day rolling over at 01:30?
------------------------------------------

Simple: that's when techies go to bed! So black at the bottom means that
someone hard core may still be reachable online if they're up working, whereas
black on top really means they're asleep.
