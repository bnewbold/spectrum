
def tokenize(s, sep="()", ws=" \t\n"):
    L = []
    food = 0
    for i, c in enumerate(s):
        if c in sep or c in ws:
            if food > 0:
                L.append(s[i-food:i])
            if c in sep:
                L.append(c)
            food = 0
        elif i+1 == len(s):
            L.append(s[i-food:])
        else:
            food += 1
    return L

def _parse_tokens(tokens, depth=0):
    L = []
    i = 0
    while i < len(tokens):
        el = tokens[i]
        if el == '(':
            expr, skip = _parse_tokens(tokens[i+1:], depth+1)
            L.append(expr)
            i += skip + 1
        elif el == ')':
            assert depth > 0, "Missing open bracket..."
            return L, i+1
        else:
            L.append(el)
            i += 1
    assert depth == 0, "Missing close bracket..."
    return L, i

def parse(s):
    tokens = tokenize(s)
    expr, size = _parse_tokens(tokens)
    return expr[0]
    
