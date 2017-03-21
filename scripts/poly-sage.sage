#!/usr/bin/env sage

from sage.calculus.functional import *
from collections import deque
import argparse, sys, time

def _simplify(expr):
    newexpr = 0
    for i in range(len(expr)):
        subexpr = repr(expr.op[i])
        simple = prod(expr.op[i].args())
        try:
            # extract the 1st value, which is the coefficient
            leading, _ = subexpr.split('*', 1)
            try:
                simple *= int(leading)
            except ValueError:
                # -1's are treated as -variable, so extract '-' if possible.
                # Otherwise, it has no coefficient
                if leading.startswith('-'):
                    simple *= -1
        except ValueError:
            # If the subexpression only has a single term, it could have a
            # negation in front of it, so check for that
            if subexpr.startswith('-'):
                simple *= -1
        newexpr += simple
    return newexpr

# Unrolls all constants
def unroll(expr, negate=False):
    assert not expr.startswith('-')
    if ' + ' in expr:
        a, b = expr.split(' + ', 1)
        return "%s + %s" % (unroll(a, negate), unroll(b))
    if ' - ' in expr:
        a, b = expr.split(' - ', 1)
        return "%s - %s" % (unroll(a, negate), unroll(b, True))
    try:
        a, b = expr.split('*', 1)
    except ValueError:
        return expr
    op = '-' if negate else '+'
    try:
        v = abs(int(a))
        return ('%s' % b) + (' %s %s' * (v-1)) % ((op, b) * (v-1))
    except ValueError:
        return expr

# Makes sure the first position is not a negation
def firstpos(expr):
    if ' + ' in expr:
        a, b = expr.split(' + ', 1)
        if a.startswith('-'):
            return '%s - %s' % (b, a[1:])
        else:
            return expr
    else:
        return expr

# Moves all subtractions to the end
def sub2end(expr):
    str = expr.split()
    d = deque([str[0]])
    minus = False
    for (op, v) in zip(str[1::2], str[2::2]):
        if op == '+':
            if minus:
                d.extendleft([op, v])
            else:
                d.extend([op, v])
        elif op == '-':
            minus = True
            d.extend([op, v])
    return ' '.join(d)

def simplify(expr):
    print >>sys.stderr, expr
    print >>sys.stderr, "Expanding expression... ",
    start = time.time()
    expr = expand(expr)
    end = time.time()
    print >>sys.stderr, "%.2f seconds" % (end - start)
    print >>sys.stderr, expr
    print >>sys.stderr, "Simplifying expression... ",
    start = time.time()
    expr = _simplify(expr)
    end = time.time()
    print >>sys.stderr, "%.2f seconds" % (end - start)
    print >>sys.stderr, expr
    print >>sys.stderr, "Unrolling expression... ",
    start = time.time()
    print >>sys.stderr, ""
    expr = firstpos(repr(expr))
    print >>sys.stderr, expr
    expr = sub2end(expr)
    print >>sys.stderr, expr
    expr = unroll(expr)
    end = time.time()
    print >>sys.stderr, "%.2f seconds" % (end - start)
    print >>sys.stderr, expr
    return expr

def _sexp(expr):
    if '*' in expr:
        a, b = expr.split('*', 1)
        return "Mul(%s, %s)" % (sexp(a), sexp(b))
    else:
        try:
            _ = int(expr)
            return "Integer(%s)" % expr
        except ValueError:
            return "Symbol('%s')" % expr

def __sexp(expr):
    try:
        v = int(expr)
        return "Integer(%s)" % expr
    except ValueError:
        return "Symbol('%s')" % expr
        
def _sexp(expr):
    lst = [__sexp(v) for v in expr.split('*')]
    if len(lst) == 1:
        return lst[0]
    else:
        return 'Mul(%s)' % (', '.join(lst))

def sexp(expr):
    if ' - ' in expr:
        adds, minuses = expr.split(' - ', 1)
        lst = [_sexp(v) for v in adds.split(' + ')]
        if len(lst) == 1:
            adds = lst[0]
        else:
            adds = 'Add(%s)' % (', '.join(lst))
        lst = [_sexp(v) for v in minuses.split(' - ')]
        if len(lst) == 1:
            subs = lst[0]
        else:
            subs = 'Add(%s)' % (', '.join(lst))
        return 'Sub(%s, %s)' % (adds, subs)
    else:
        lst = [_sexp(v) for v in expr.split(' + ')]
        if len(lst) == 1:
            adds = lst[0]
        else:
            adds = 'Add(%s)' % (', '.join(lst))
        return adds
        

def main(argv):
    parser = argparse.ArgumentParser(
        description='Polynomial optimizer.')

    parser.add_argument('file', metavar='FILE', nargs='?', help='optional file to read from')
    parser.add_argument('--dot', action='store_true', help='output in dot file format')
    parser.add_argument('--no-opt', action='store_true', help='don\'t optimize')

    args = parser.parse_args()
    if args.file:
        with open(argv[1], 'r') as f:
            exprs = f.read()
    else:
        exprs = sys.stdin.read().strip()

    exprs = eval(exprs)
    if type(exprs) != list:
        exprs = [exprs]
    for expr in exprs:
        if type(expr) == str:
            expr = eval(expr)
        if not args.no_opt:
            expr = simplify(expr)

        if args.dot:
            print(dotprint(expr))
        else:
            print(sexp(expr))

if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        pass
