
"""
Takes a string 's' and will convert it to an Int64 or Float64 (if possible), or
return the string if not.
"""
function trynum(s::AbstractString)
    # Test for: only number chars, and optionally a single decimal (period),
    # but not just a single decimal on it's own ("." is not a number).
    # Also allow a '-' in the first position.
    # Could have just try/caught around Julia's float(), used below.
    decimal_count = 0
    dash_count = 0
    for (i, c) in enumerate(s)
        if c == '.'
            decimal_count += 1
        elseif c == '-' && i == 1 && length(s) > 1
            dash_count += 1
        elseif !(c in "1234567890")
            return s
        end
    end
    if decimal_count > 1 || decimal_count + dash_count >= length(s)
        return s
    end
    # Haven't written our own string-to-number function; use Julia's
    if decimal_count > 0
        return float(s)
    else
        # Julia 0.4.3 complains with "use parse(Int,s) instead" of int(s)
        return parse(Int, s)
    end
end

"""
Take a string 's' and splits it into elements (based on 'ws' white space
characters) and tuples (based on 'sep' separators).
"""
function tokenize(s::AbstractString; sep="()", ws=" \t\n")
    L = AbstractString[]
    food = 0  # num of yummy non-whitespace characters we have just eaten
    for (i, c) in enumerate(s)
        if c in sep || c in ws
            if food > 0
                push!(L, s[i-food:i-1])
            end
            if c in sep
                push!(L, string(c))
            end
            food = 0
        elseif i == length(s)
            push!(L, s[i-food:end])
            # will break next iteration
        else
            food += 1
        end
    end
    # Convert Array of strings to an (immutable) Tuple
    return tuple(L...)
end

"""
Helper for `parse()`.

Returns two values: the parsed expression, and the number of tokens consumed.

Note that this function always returns a Tuple, even if only a single token is
passed: calling code must unwrap. Also, this will only parse out the first
complete expression, silently discarding trailing elements.

Should probably use value exceptions instead of @assert on failure
"""
function _parse_tokens(tokens, depth=0)
    L = []
    i = 1
    while i <= length(tokens)
        el = tokens[i]
        if el == "("
            (expr, skip) = _parse_tokens(tokens[(i+1):end], depth + 1)
            push!(L, expr)
            i += skip
        elseif el == ")"
            @assert depth > 0 "Missing open bracket..."
            return (tuple(L...), i+1)
        else
            push!(L, el)
            i += 1
        end
    end
    @assert depth == 0 "Missing close bracket..."
    return (tuple(L...), i)
end

"""
Takes a string and returns a tuple-based pseudo AST.

Notes: all numbers are converted to Float64. No weird special characters are
parsed specially (eg, comma, pipe, hash, etc).
"""
function parse(s::AbstractString)
    # First split into a flat list...
    tokens = tokenize(s)

    # ... then convert any numbers ...
    tokens = map(trynum, tokens)

    # ... then parse into nested tuples.
    (expr, sz) = _parse_tokens(tokens)

    # Unwrap the first element and return that.
    return expr[1]
end

# parse("1")
# parse("((()) ())")
# parse("(a 134 (   4 5 6) 2 ( ))")
# parse("(asdf 134 (   4 5 6) 2 ( ))")
# parse("(1 2 3 -4567 123.25624 . -.)")
# parse("(1 2 3 -4567 123.25624 .1 1. -.1 -1. - . -.)")
# parse("(first (list 1 (+ 2 3) 9))")
