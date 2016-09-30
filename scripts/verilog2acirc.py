#!/usr/bin/python

import sys
import re

fname = sys.argv[1];

wires = {};
inputs = {};
outputs = {};

f = open(fname)
f_iter = iter(f)

for line in f_iter:
    line = line.strip()

    # skip comments
    if re.match("/\*.*\*/", line) or re.match("\(\*.*\*\)", line):
        continue

    # skip useless wire declarations
    if re.match("^(wire|input|output)", line):
        continue

    if re.match("^NOT", line):
        a = int(re.match(".A\(_(\d+)_\),", next(f_iter).strip()).group(1))
        y = int(re.match(".Y\(_(\d+)_\)", next(f_iter).strip()).group(1))
        wires[y] = ["NOT", a]
        next(f_iter) # throw away the closing paren
        continue

    m = re.match("^(AND|XOR)", line)
    if m:
        a = int(re.match(".A\(_(\d+)_\),", next(f_iter).strip()).group(1))
        b = int(re.match(".B\(_(\d+)_\),", next(f_iter).strip()).group(1))
        y = int(re.match(".Y\(_(\d+)_\)", next(f_iter).strip()).group(1))
        wires[y] = [m.group(1), a, b]
        next(f_iter) # throw away the closing paren
        continue

    m = re.match("^assign _(\d+)_ = pi(\d+);", line)
    if m:
        x = int(m.group(2))
        w = int(m.group(1))
        inputs[x] = w
        continue

    m = re.match("^assign po(\d+) = _(\d+)_;", line)
    if m:
        z = int(m.group(1))
        w = int(m.group(2))
        outputs[z] = w
        continue

f.close()

print(inputs)
print(outputs)
print(wires)
