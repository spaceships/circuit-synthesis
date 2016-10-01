#!/usr/bin/python

import sys
import re

fname = sys.argv[1]
key_len = int(sys.argv[2])

gates = {}
inputs = {} # input number -> verilog wire number
outputs = []

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
        gates[y] = ["NOT", a]
        next(f_iter) # throw away the closing paren
        continue

    m = re.match("^(AND|XOR)", line)
    if m:
        a = int(re.match(".A\(_(\d+)_\),", next(f_iter).strip()).group(1))
        b = int(re.match(".B\(_(\d+)_\),", next(f_iter).strip()).group(1))
        y = int(re.match(".Y\(_(\d+)_\)", next(f_iter).strip()).group(1))
        gates[y] = [m.group(1), a, b]
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
        #  z = int(m.group(1))
        w = int(m.group(2))
        outputs.append(w)
        continue
f.close()

input_len = len(inputs) - key_len 

print(": nins {}".format(input_len))

# assign const 1
print("0 input y0 1")

next_x = 0
next_y = 1
next_ref = 1

refs = {} # verilog wires to refs

for i in inputs:
    if i < input_len: 
        print("{} input x{}".format(next_ref, next_x))
        next_x += 1
    else:
        # just make the key 0 for convenience: change it if you care
        print("{} input y{} 0".format(next_ref, next_y))
        next_y += 1
    refs[inputs[i]] = next_ref
    next_ref += 1

def print_circ(wire):
    global next_ref

    # nullary gate
    if gates[wire][0] == "NOT":
        if not gates[wire][1] in refs:
            print_circ(gates[wire][1])

        print("{} {} SUB {} {}".format(
            next_ref, 
            "output" if wire in outputs else "gate", 
            0,
            refs[gates[wire][1]]
        ))

    # binary gate
    else: 
        if not gates[wire][1] in refs:
            print_circ(gates[wire][1])

        if not gates[wire][2] in refs:
            print_circ(gates[wire][2])


        if gates[wire][0] == "AND":
            print("{} {} MUL {} {}".format(
                next_ref, 
                "output" if wire in outputs else "gate", 
                refs[gates[wire][1]],
                refs[gates[wire][2]]
            ))

        #  else if gates[wire][0] == "XOR":
        #      print("{} {} AND {} {}".format(
        #          next_ref, 
        #          "output" if wire in outputs else "gate", 
        #          refs[gates[wire][1]],
        #          refs[gates[wire][2]]
        #      ))

        else:
            sys.exit("unknown gate type: {}".format(gates[wire][0]))

    refs[wire] = next_ref
    next_ref += 1

for w in outputs:
    print_circ(w)
