#!/usr/bin/env python3

from sympy import (expand, srepr, Pow, Add, Mul)
import sys

def _simplify(expr):
    if type(expr) == Pow:
        return Pow(*(expr.args[0], 1))
    elif type(expr) == Add:
        return Add(*(_simplify(e) for e in expr.args))
    elif type(expr) == Mul:
        return Mul(*(_simplify(e) for e in expr.args))
    else:
        return expr

def simplify(expr):
    # print("Expanding expression...", file=sys.stderr)
    expr = expand(expr)
    # print("Simplifying expression...", file=sys.stderr)
    expr = _simplify(expr)
    return expr

def main(argv):
    if len(argv) == 1:
        # read from stdin
        expr = simplify(sys.stdin.read())
    else:
        # read from file
        with open(argv[1], 'r') as f:
            expr = simplify(f.read())

    print(srepr(expr))
            

if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        pass
