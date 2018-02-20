#!/usr/bin/env perl

use strict;
use warnings;
use 5.17.0;

open(my $fh, '<', $ARGV[0]) or die "could not open file $ARGV[0]";

my %alias;

while (<$fh>) {
    $alias{$2} = $1 if /assign _(\d+)_ = _(\d+)_/;
}

seek $fh, 0, 0;

while (<$fh>) {
    while (my ($from, $to) = each %alias) {
        s/$from/$to/;
    }
    print unless /assign _(\d+)_ = _\g1_/;
}


