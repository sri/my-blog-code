#! /usr/bin/env python2.5
#
# orange4.py -- an interpreter for a  simple lisp-like language
# [under same license as Python]
# (inspired by Peter Norvig's JScheme)
#
# =================================================================
# + Notes +
# =================================================================
#
#  - This is just a silly hack for a simple
#    lisp-like interpreter in Python.
#  - Python tuples are used for the language.
#    To overcome the fact that freevars will cause
#    Python to error out, invoke the Using class
#    as the first argument of the form: and pass
#    in a string of all freevars used in the
#    second form.
#  - The use of Python tuples makes things
#    really, really UGLY!! Other than the
#    commas everywhere, you have to be careful
#    about singleton tuples: for example,
#    for a LET, you have a single binding, say
#    (a, 10); you can't say
#    (let, ((a, 10)) ...)
#    you have to:
#    (let, ((a, 10),) ...)!!!
#
# =================================================================
# + Examples (see the function test for more) +
# =================================================================
#
#  > oeval((using('a b'), (let, ((a, 10), (b, 20)), (pr, (plus, a, b)))))
#  30
#
#  > oeval((using('a b c'), (oif, (let, ((a, 100), (b, 200)),
#                                   (let, ((c, (gt, a, b)),),
#                                     (oif, c, (pr, 'greater'), (pr, 'lower')),
#                                     c)),
#                              'hi',
#                              'bye')))
#  lower
#  'bye'
#
#  [User-defined functions]
#
#  > oeval((using('a x y z'), (let, ((a, (olambda, (x, y, z),
#     (plus, (plus, x, y), z),)),), (a, 100, 200, 300))))
#  600

__version__ = "$Id: orange4.py,v 1.10 2006/12/09 19:47:42 sri Exp $"

trace = False

class oexc(Exception):
    pass

def error(s):
    raise oexc(s)

class var:
    def __init__(self, name):
        self.name = name
    def __str__(self):
        return "<lispvar %s>" % self.name
    __repr__ = __str__
        

# I can't say something like
# oeval((let, ((a, 10), (b, 20)) ...)):
# Python will complain about a and b.
# So we do something like this:
# oeval((using('a b'), (let, ((a, 10), (b, 20)) ...))):
class using:
    def __init__(self, string):
        for name in string.split():
            if name in globals():
                print "Warning: `%s' already defined, ignoring it" % name
            else:
                globals()[name] = var(name)

# names that conflict with Python
# builtins start with an 'o'
primitives = """
    oif pr plus minus
    true nil olambda let gt
    closure eq multiply
"""

for prim in primitives.split():
    globals()[prim] = prim
    globals()[prim.upper()] = prim


# =================================================================
# + Eval & Apply +
# =================================================================

def oeval(x, env=None):
    if trace: print 'oeval', x, env
    
    if isinstance(x, var):
        return env_lookup(x, env)
    elif not isinstance(x, tuple):
        return x

    if isinstance(x[0], using):
        x = x[1]

    fn   = x[0]
    args = x[1:]

    if fn == OIF:
        if oeval(args[0], env) == NIL:
            return oeval(args[2], env)
        else:
            return oeval(args[1], env)

    elif fn == OLAMBDA:
        return (CLOSURE, args[0], args[1:], env)

    elif fn == LET:
        bindings = args[0]
        newenv = dict((var.name, oeval(val, env))
                      for (var, val) in bindings)

        return oevalis(args[1:], [newenv, env])

    else: # function application
        # global env is the python global env,
        # so we don't need to evaluate fn (i think).
        return oapply(oeval(fn, env),
                      tuple(oeval(arg, env) for arg in args))
        

# since we are applying fns, args are evalled
def oapply(fn, args):
    def err(type):
        error("%s %s, %s" % (
            type,
            (fn.name if isinstance(fn, var) else fn),
            args))
    
    if not isinstance(fn, tuple):
        if fn == GT:
            if args[0] > args[1]:
                return TRUE
            return NIL

        elif fn == PR:
            print args[0]
            return NIL

        elif fn == PLUS:
            return args[0] + args[1]

        elif fn == MINUS:
            return args[0] - args[1]

        elif fn == EQ:
            # args should just be numbers
            if args[0] == args[1]:
                return TRUE
            return NIL

        elif fn == MULTIPLY:
            return args[0] * args[1]
        
        else:
            err("unknown function")

    # user-defined functions:
    elif fn[0] == CLOSURE:
        if trace: print 'calling closure', fn, args
        
        formal_params, body, env_when_defined = fn[1:]
        actual_params = args
        
        if len(formal_params) != len(actual_params):
            err("wrong number of args")
        newenv = dict(zip((x.name for x in formal_params),
                          actual_params))
        # lexical scoping
        return oevalis(body, [newenv, env_when_defined])

        # return oevalis(body, [newenv, env])
        # the above specifies dynamic scoping (i think!)
        # env, should be an env passed
        # to this oapply fn.
        
    else:
        err("unknown function")
    

def oevalis(body, env):
    result = nil
    for x in body:
        result = oeval(x, env)
    return result

# envs are stacked like so: [{'a', 1},   [{'b': 2, 'c': 3},   {}]]
#
# env is either a non-True value, or
# a 2-element list:
#  - the 1st element is a dict
#  - the 2nd element is an env
def env_lookup(var, env):
    if not env:
        error("unbound variable %s: %s" % (var.name, env))
    env, parents = env
    if var.name in env:
        return env[var.name]
    return env_lookup(var, parents)


# =================================================================
# + Tests +
# =================================================================

def test():
    def really_assert(expected, form):
        try:
            actual = oeval(form)
        except oexc, ex:
            print "lisperror while evaluating form: %s\n%s" % (
                str(ex),
                str(form))
        else:
            if expected != actual:
                print "expected `%s' but got `%s':\n%s\n%s\n%s" % (
                    expected, actual,
                    "="*50,
                    str(form),
                    "="*50)


    # ================= 
    really_assert(30, (using('a b'), (let, ((a, 10), (b, 20)),
                                        (pr, (plus, a, b)),
                                        (plus, a, b))))


    really_assert("bye", (using('a b c'),
      (oif, (let, ((a, 100), (b, 200)),
             (let, ((c, (gt, a, b)),),
               (oif, c, (pr, 'greater'), (pr, 'lower')),
                c)),
        'hi',
        'bye')))
                            
    really_assert(600, (using('a x y z'),
                         (let, ((a, (olambda, (x, y, z),
                                      (plus, (plus, x, y), z),)),),
                           (a, 100, 200, 300))))
    

    # the y combinator:
    # translated from
    # http://www.ece.uc.edu/~franco/C511/html/Scheme/ycomb.html
    really_assert(3628800,
                  (using('ycomb x proc arg fact fnarg n'),
       (let, ((ycomb, (olambda, (x,),
                        ((olambda, (proc,),
                           (x, (olambda, (arg,), ((proc, proc), arg)))),
                         (olambda, (proc,),
                           (x, (olambda, (arg,), ((proc, proc), arg))))))),),
         (let, ((fact, (olambda, (fnarg,),
                         (olambda, (n,),
                           (oif, (eq, n, 0),
                                 1,
                                 (multiply, n,
                                            (fnarg, (minus, n, 1))))))),),
          (pr, "hi"),
          (pr, ((ycomb, fact), 10)),
          ((ycomb, fact), 10)))))
