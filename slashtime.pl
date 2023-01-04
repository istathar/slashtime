#!/usr/bin/perl -w

#
# slashtime
# http://research.operationaldynamics.com/projects/scripts/#slashtime
#
# Copyright (C) 2003-2005 Andrew Cowie <andrew@operationaldynamics.com>
# Code tidy ups contributed by Michael Beattie <mike@ethernal.org>
# Props to Stewart Smith <stewart@flamingspork.com> who tried to fix the half
# hour thing. Unfortunately, his patch didn't work. Good try, though :)
#
# This source released under the GNU GPL, version 2.
#

#
# Usage tip: run this from your .bash_profile! Then whenver you need to
# see what time it is somewhere, just pop a terminal window!.
#
# Also see online at http://operationaldynamics.com/time
#
# originally inspired by tzlist, part of gworldclock, a package available in
# Debian Linux. The thing that tzlist didn't do is give you sense of what the
# time offset from your *present* location is. That's what slashtime does.
# And, it colours your current timezone blue, and UTC green, for eye candy.
#

#
# See online example of .tzlist for that file's format:
# http://research.operationaldynamics.com/projects/scripts/slashtime/.tzlist
# 

#use strict;
use POSIX qw(strftime);
use POSIX qw(tzset);
use Date::Parse;

my $TZLIST="$ENV{'HOME'}/.config/slashtime/tzlist";
my $SYSTZ="/etc/timezone";
my $FORMAT="%H:%M %a, %e %b %y";

my $now;

#
# Get the local timezone's 'real' name.
#
# Incidentally, 'localtime' is a symlink in the system timezones directory
# /usr/share/zoneinfo pointing at /etc/localtime which points at the
# appropriate current timezone file.  We use "localtime" as a handyname to
# ensure its something we can find later]
#
my $local_timezone;
if (-f $SYSTZ)
{
  open(LOCALZONE,"$SYSTZ"); $local_timezone = <LOCALZONE>; chomp
  $local_timezone; close(LOCALZONE);
}
elsif (-f '/etc/localtime')
{
  $local_timezone = readlink '/etc/localtime';
  $local_timezone =~ s/.*zoneinfo\///;
}
else
{
  die "Can't find current local time zone :("
}

#
# parse the command line. If one of the args can
# be interpreted as a timezone, then reset to act as if
# that zone is the local timezone. The rest of the command line
# is interpreted by Date::Parse, so you can do something like
# 
#	$ slashtime America/Toronto 3pm
#
# and get a listing of what time it will be various places when it is 1500
# in Toronto. Yes, this is a touch crude, but it's handy for quick meeting 
# planning. WATCH OUT FOR WHICH DATE IT ASSUMES THE TARGET TO BE.
#

if (scalar(@ARGV) < 1) {
	$now = time();
} else {
	my $time;
	my $arg_zone;

	foreach my $arg (@ARGV) {
		if ( -f "/usr/share/zoneinfo/" . $arg) {
			$arg_zone = $arg;
		} else {
			$time .= $arg . " ";
		}
	}
 
	if ($arg_zone) {
		$ENV{'TZ'} = $arg_zone;
		tzset();
	}

	$now = str2time($time);
	unless (defined $now) { die "Can't parse '$time' as a time or date\n" }

	$ENV{'TZ'} = $local_timezone;
	tzset();
}


my %label;
my %date;
my %tzname;
my %reloffset;
my %halfhour;
my $localoffset;
my %utcoffset;

$localoffset = strftime("%z",localtime($now));


sub halves {
	my $arg = $_[0];
	use integer;
	my $hours = $arg / 100;
	my $halves = $hours * 2;
	if (($arg % 100) != 0) {
		$halves++;
	}
	return $halves;
}

#
# Load timezone information from user's timezone list file
#

open(ZONES,"$TZLIST");
while (<ZONES>) {
	chomp;
	if (/^#/) {
 		next;
	}
	if (/^$/) {
 		next;
	}
	
	# convert the arbitrary whitespace seperator
	# into a single space
	s/(\S+)\s+"/$1 "/;	
	# loose the quotes
	s/"//g;	

	my ($timezone, $place) = split(/\s/,$_,2);

	$ENV{'TZ'} = $timezone;
	tzset();

	$label{$timezone} = $place;
	$date{$timezone} = strftime("$FORMAT",localtime($now));
	$tzname{$timezone} = "(".strftime("%Z",localtime($now)).")";
        # Calculate offset relative to current timezone, rather than GMT
#	$reloffset{$timezone} = (strftime("%z",localtime($now)) - $localoffset) / 100;
	my $zoneoffset = strftime("%z",localtime($now));
#	print halves($zoneoffset)." ".halves($localoffset)." $place\n";
	$utcoffset{$timezone} = $zoneoffset;
	$reloffset{$timezone} = halves($zoneoffset) - halves($localoffset);
}
close(ZONES);

#
# Get the info for the local timezone.
#

# ... but, we don't need this if the system's current 
# local time matches [otherwise we get Local and Sydney showing
# up blue. Really, the colour is enough of a queue. No need to list
# Local if the local_timezone matches one of the cities in the list]
#

#foreach my $timezone ( keys %reloffset ) { 
#	print "DEBUG $timezone $reloffset{$timezone}\n";
#}
	
if (!$tzname{$local_timezone}) {

	$ENV{'TZ'} = $local_timezone;
	tzset();

	$label{"localtime"} = "Local";
	$date{"localtime"} = strftime("$FORMAT",localtime($now));
	$tzname{"localtime"} = "(".strftime("%Z",localtime($now)).")";
	$reloffset{"localtime"} = 0;
}

#
# Output
#

print "\n";

foreach my $timezone ( sort { $reloffset{$a} <=> $reloffset{$b} } keys %reloffset ) { 

	my $coloured;
	
	if (($timezone =~ /^localtime$/i) || ($timezone eq $local_timezone)) {
		print "\033[0;36m";
		$coloured = 1;
	} elsif (($timezone eq "GMT") || ($timezone =~ /^Zulu$/i) || ($timezone eq "UTC")) {
		print "\033[0;32m";
		$coloured = 1;
	} else {
		$coloured = 0;
	}

	format STDOUT_TOP=
Timezone                Current Date          Name    UTC     Local offset
--------                ------------          ----    ---     ------------
.

	format STDOUT = 
@<<<<<<<<<<<<<<<<<<<... @<<<<<<<<<<<<<<<<<<<< @<<<<<< @<<<<<< @<<<<
$tz_label,              $tz_date,             $tz_name,$tz_utc, $tz_off
.
	#printf("%-12s %s %6s %+3.0f%lc",


	$tz_label = $label{$timezone};
	$tz_date = $date{$timezone};
	$tz_name = $tzname{$timezone};
	$tz_name = $tzname{$timezone};
        $tz_utc  = sprintf("%+3.0f%s", $utcoffset{$timezone}/100, ($utcoffset{$timezone}%100) ? ".5": "");
        $tz_off  = sprintf("%+3.0f%s", 		$reloffset{$timezone}/2 +
			($reloffset{$timezone} > 0 ? -0.1 : +0.1),
		(($reloffset{$timezone}%2) == 0 ? "" : ".5")
);

write();

#			the +/- 0.1 bit is an ugly hack to make the rounding
#			work out. Otherwise either plus or minus 1.5 ends up 
#			off by an hour

	print "\033[0m" if ($coloured);
}

print "\n";
