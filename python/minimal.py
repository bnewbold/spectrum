"""
    null -> None
    pair -> <use tuples>
    boolean (true/false) -> True/False
    number -> number
    identifier/symbol -> string


    const (atomic values)
    lambda
    quote (s-expr "one level down")
    identifier (a symbol to be looked up in the context)
    cond (conditional expression)
"""

def is_atom(x):
    return x is () or type(x) not in (None, tuple, list, dict)

def is_boolean(x):
    return x in (True, False)

def is_number(x):
    return type(x) in (int, float, long)

def is_zero(x):
    return x == 0

def is_null(x):
    return x is ()

def is_eq(a, b):
    return a == b

def is_builtin(x):
    return x in ('lambda', 'quote', 'cond', 'else', 'cons', 'car', 'cdr',
                 'null?', 'eq?', 'atom?', 'zero?', 'number?',
                 '+', '-', '*', '/')

# because python does not allow circular function definitions, and action
# functions call out to meaning(), which depends on some actions, we'll use an
# "action lookup table" to handle function references ourself.
# A more "pythonic" way to do this might be to use an object.
actions = dict()

def meaning(x, ctx):
    action = None
    if is_atom(x):
        if is_number(x) or is_boolean(x) or is_builtin(x):
            action = actions['identity']
        else:   # an identifier
            action = actions['lookup']
    elif type(x) is tuple:
        if x[0] is 'quote':
            action = actions['quote']
        elif x[0] is 'cond':
            action = actions['cond']
        elif x[0] is 'lambda':
            action = actions['lambda']
        elif type(x[0]) is tuple:
            action = actions['apply']
        else:   # some other identifier, either builtin or not
            action = actions['apply']
    else:
        raise ValueError("Unexpected expression: %s" % x)
    return action(x, ctx)

def meaning_list(l, ctx):
    return tuple([meaning(x, ctx) for x in l])

def identity_action(x, ctx):
    return x
actions['identity'] = identity_action

def lookup_action(x, ctx):
    if not x in ctx:
        ValueError("Unknown identifier: %s" % x)
    else:
        return ctx[x]
actions['lookup'] = lookup_action

def quote_action(x, ctx):
    if len(x) != 2:
        ValueError("Improper quote usage: %s" % x)
    return x[1]
actions['quote'] = quote_action

def cond_action(x, ctx):
    for line in x[1:]:
        if line[0] == 'else':
            return meaning(line[1], ctx)
        elif meaning(line[0], ctx):
            return meanint(line[1], ctx)
        else:
            # "unspecified"
            return None
actions['cond'] = cond_action

def lambda_action(x, ctx):
    return ('procedure', x[1], x[2:], ctx.copy())
actions['lambda'] = lambda_action

def apply_action(x, ctx):
    if is_builtin(x[0]):
        args = meaning_list(x[1:], ctx)
        if x[0] is 'cons':
            return (args[0], ) + args[1]
        elif x[0] is 'car':
            return args[0][0]
        elif x[0] is 'cdr':
            return args[0][1]
        elif x[0] is 'null?':
            return is_null(args[0])
        elif x[0] is 'eq?':
            return is_eq(args[0], args[1])
        elif x[0] is 'atom?':
            return is_atom(args[0])
        elif x[0] is 'zero?':
            return is_zero(args[0])
        elif x[0] is 'number?':
            return is_number(args[0])
        elif x[0] is '+':
            return args[0] + args[1]
        elif x[0] is '-':
            return args[0] - args[1]
        elif x[0] is '*':
            return args[0] * args[1]
        elif x[0] is '/':
            return args[0] / args[1]
        else:
            raise Exception("Unexpected builtin: %s" % x[0])
    elif type(x[0]) is tuple:
        proc = meaning(x[0], ctx)
        if proc[0] is not 'procedure':
            raise Exception("Not applicable: %s" % str(proc))
        variables = proc[1]
        body = proc[2]
        closure = proc[3].copy()
        args = meaning_list(x[1:], ctx)
        for i in range(len(variables)):
            closure[variables[i]] = args[i]
        for expr in body:
            ret = meaning(expr, closure)
        return ret
    else:
        raise Exception("Unexpected... thing...: %s" % str(x[0]))
actions['apply'] = apply_action

def value(x):
    return meaning(x, dict())

def test():
    # ((lambda (x) (+ 1 x)) 5) ; 6
    v = value( (('lambda', ('x',), ('+', 1, 'x')), 5) )
    assert(v is 6)
    return True

if __name__=='__main__':
    print(test())
