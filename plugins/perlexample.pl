#!/usr/bin/perl

# Example of how to write an external PageTransform plugin in Perl.

use strict;

print "Hello from Perl!";
print "<br/>";

print "Your script recieved these args:";
print "<br/>";
print "@ARGV";
print "<br/>";

print "And this text from stdin:";
print "<br/>";
print <STDIN>;
print "<br/>";
print "<br/>";
