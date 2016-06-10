#!/usr/bin/python

import sys

[_, verilog_file, xsize, ysize] = sys.argv
print(verilog_file, xsize, ysize)

xstart = 0
xend   = xstart + xsize - 1
ystart = xend
yend   = xend + ysize # account for const 1
