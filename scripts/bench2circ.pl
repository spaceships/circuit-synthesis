#!/usr/bin/perl

# convert abc bench format to amaloz/obfuscation boolean circuit format

use 5.20.0;
use List::Util qw( max );

my %mapping;
my %gates;
my @inputorder;
my @gateorder;
my $outgate;
my $nextref = 0;

open(my $fh, '<', $ARGV[0]) or die "couldn't open $ARGV[0]: $!";
my $type = $ARGV[1] || die "no type given";

#eventually take this and input to y from command line
my ($nxs, $nys) = @ARGV[2..3];
my $xstart = 0;
my $xend   = $xstart + $nxs - 1;
my $ystart = $xend;
my $yend   = $xend + $nys; # account for const 1

while (<$fh>) {
  my $var;
  my $x;
  my $y;

  if (/INPUT\((.*)\)/) {
    $gates{$nextref} = [ "input", [] ];
    push @inputorder, $nextref;
    $mapping{$1} = $nextref++;
  }

  elsif (/OUTPUT\((.*)\)/) {
    $outgate = $1;
  }

  elsif (/([\w\d]+)\s+=\s+(\w+)\(([\w\d]+)\)/) {
    $x = $mapping{$3};
    $gates{$nextref} = [ $2, [$x] ];
    push @gateorder, $nextref;
    $mapping{$1} = $nextref++;
  }

  elsif (/([\w\d]+)\s+=\s+(\w+)\(([\w\d]+), ([\w\d]+)\)/) {
    $x = $mapping{$3};
    $y = $mapping{$4};
    $gates{$nextref} = [ $2, [$x, $y] ];
    push @gateorder, $nextref;
    $mapping{$1} = $nextref++;
  }
}

close $fh;

my %get_depth_seen;
sub get_depth {
    my ($ref) = @_;
    if (exists $get_depth_seen{$ref}) {
        return $get_depth_seen{$ref};
    }
    my @gate = @{$gates{$ref}};
    if ($gate[0] eq "input") {
        $get_depth_seen{$ref} = 0;
        return 0;
    } else {
        my $res = 1 + max(map {get_depth($_)} @{$gate[1]});
        $get_depth_seen{$ref} = $res;
        return $res;
    }
}

my $outgateref = $mapping{$outgate};

my $depth = get_depth($outgateref);

sub print_arith {
  # first input is const 1
  say ": nins $nxs";
  say ": depth $depth";
  say "0 input y0 1";
  my $xctr = 0;
  my $yctr = 1;
  for my $var (@inputorder) {
    print "${\( $var+1 )} input ";
    if ($var >= $xstart and $var <= $xend) {
      say "x", $xctr++;
    } elsif ($var >= $ystart and $var <= $yend) {
      say "y", $yctr++, " 0";
    }
  }
  for my $var (@gateorder) {
    my $type = $gates{$var}->[0];
    my @args = map {$_+1} @{$gates{$var}->[1]};
    my $str = $var+1;
    if ($var == $outgateref) {
      $str .= " output ";
    } else {
      $str .= " gate ";
    }
    if ($type eq "NOT") {
      $str .= "SUB 0 @args";
    } elsif ($type eq "AND") {
      $str .= "MUL @args";
    } else {
      die "Unknown gate type: $type";
    }
    say $str;
  }
}

sub print_bool {
  say ": nins $nxs";
  say ": depth $depth";
  push @inputorder, @gateorder;
  for my $var (@inputorder) {
    my $type = $gates{$var}->[0];
    my $args = $gates{$var}->[1];
    if ($type eq "input") {
      say "$var input";
    } else {
      my $str = $var;
      if ($var == $outgateref) {
        $str .= " output ";
      } else {
        $str .= " gate ";
      }
      $str .= "$type @$args";
      say $str;
    }
  }
}

sub print_formula {
  # do a post-order traversal to rename variables
  $nextref = 0;
  my @inputs;
  my @gates;
  my $output;
  my $visit;
  $visit = sub { 
    my ($var) = @_;
    my $type = $gates{$var}->[0];
    my $args = $gates{$var}->[1];
    if ($type eq "input") {
      push @inputs, "$nextref input";
      return $nextref++;
    } 
    else {
      my @argvars = map { &{$visit}($_) } @$args;
      my $gatestr = ($var == $outgateref) ?  "output" : "gate";
      push @gates, "$nextref $gatestr $type @argvars";
      return $nextref++;
    }
  };
  &$visit($outgateref);
  say for @inputs;
  say for @gates;
}

if    ($type eq "boolean-circuit")    { print_bool()    }
elsif ($type eq "arithmetic-circuit") { print_arith()   }
elsif ($type eq "boolean-formula")    { print_formula() }
