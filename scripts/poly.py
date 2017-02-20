#!/usr/bin/env python3

from sympy import *
from sympy.printing.dot import dotprint
import argparse, sys

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
    parser = argparse.ArgumentParser(
        description='Polynomial optimizer.')

    parser.add_argument('file', metavar='FILE', nargs='?', help='optional file to read from')
    parser.add_argument('--dot', action='store_true', help='output in dot file format')
    parser.add_argument('--no-opt', action='store_true', help='don\'t optimize')

    args = parser.parse_args()
    if args.file:
        with open(argv[1], 'r') as f:
            expr = f.read()
    else:
        expr = sys.stdin.read().strip()

    if args.no_opt:
        expr = eval(expr)       # XXX: can we do this w/o eval?
    else:
        expr = simplify(expr)

    if args.dot:
        print(dotprint(expr))
    else:
        print(srepr(expr))

if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        pass
