#!/usr/bin/env sage

from sage.calculus.functional import *
import argparse, sys, time

def _mysimplify(expr):
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

def mysimplify(expr):
    print >>sys.stderr, "Expanding expression... ",
    start = time.time()
    expr = expand(eval(expr))
    end = time.time()
    print >>sys.stderr, "%.2f seconds" % (end - start)
    print >>sys.stderr, "Simplifying expression... ",
    start = time.time()
    expr = _mysimplify(expr)
    end = time.time()
    print >>sys.stderr, "%.2f seconds" % (end - start)
    return expr

def sexp(expr):
    if ' + ' in expr:
        a, b = expr.split(' + ', 1)
        return "Add(%s, %s)" % (sexp(a), sexp(b))
    elif ' - ' in expr:
        a, b = expr.split(' - ', 1)
        return "Add(Mul(Integer(-1), %s), %s)" % (sexp(a), sexp(b))
    elif '*' in expr:
        a, b = expr.split('*', 1)
        return "Mul(%s, %s)" % (sexp(a), sexp(b))
    else:
        try:
            v = int(expr)
            return "Integer(%s)" % expr
        except ValueError:
            return "Symbol('%s')" % expr

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
        expr = mysimplify(expr)

    if args.dot:
        print(dotprint(expr))
    else:
        print(sexp(repr(expr)))

if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        pass
