
# A partial Scheme implementation in Julia

#   null -> nothing
#   pair -> Pair type
#   boolean (true/false) -> true/false
#   number -> Number
#   identifier/symbol -> symbol

is_atom(x) = x == () || !(x == nothing ||
                          typeof(x) <: Tuple ||
                          typeof(x) <: Array ||
                          typeof(x) <: Dict)
is_null(x) = x == ()
is_zero(x) = x == 0
is_eq(a, b) = a == b
is_number(x) = typeof(x) <: Number
is_boolean(x) = x == true || x == false
is_builtin(x) = x in (:lambda, :quote, :cond, :else, :cons, :car, :cdr,
                      :isnull, :iseq, :isatom, :iszero, :isnumber,
                      :+, :-, :*, :/, +, -, *, /)

identity_action(x, ctx) = x
lookup_action(x, ctx) = ctx[x]
quote_action(x, ctx) = length(x) == 2 ? x[2] : throw(ErrorException())

function cond_action(x, ctx)
    for line in x[2:end]
        if line[1] == :else
            return meaning(line[2], ctx)
        elseif meaning(line[1], ctx)
            return meaning(line[2], ctx)
        else
            # "unspecified"
            return nothing
        end
    end
end

function lambda_action(x, ctx)
    return (:procedure, x[2], x[3:end], copy(ctx))
end

function apply_action(x, ctx)
    if is_builtin(x[1])
        action = x[1]
        args = [meaning(y, ctx) for y in x[2:end]]
        if action == :cons
            return tuple(args[1], args[2])
        elseif action == :car
            return args[1][1]
        elseif action == :cdr
            return args[1][2]
        elseif action == :isnull
            return is_null(args[1])
        elseif action == :iseq
            return is_eq(args[1], args[2])
        elseif action == :isatom
            return is_atom(args[1])
        elseif action == :iszero
            return is_zero(args[1])
        elseif action == :isnumber
            return is_number(args[1])
        elseif action == :+ || action == +
            return args[1] + args[2]
        elseif action == :- || action == -
            return args[1] - args[2]
        elseif action == :* || action == *
            return args[1] * args[2]
        elseif action == :/ || action == /
            return args[1] / args[2]
        else
            throw(ErrorException("Unexpected builtin: $(x[0])"))
        end
    elseif typeof(x[1]) <: Tuple
        proc = meaning(x[1], ctx)
        if proc[1] != :procedure
            throw(ErrorException("Not applicable: $(str(proc))"))
        end
        variables = proc[2]
        body = proc[3]
        closure = copy(proc[4])
        args = [meaning(y, ctx) for y in x[2:end]]
        for i in 1:length(variables)
            closure[variables[i]] = args[i]
        end
        ret = nothing
        for expr in body
            ret = meaning(expr, closure)
        end
        return ret
    else
        throw(ErrorException("Unexpected... thing...: $(x[1])"))
    end
end

function meaning(x, ctx)
    action = nothing
    if is_atom(x)
        if is_number(x) || is_boolean(x) || is_builtin(x)
            return identity_action(x, ctx)
        elseif typeof(x) <: Symbol
            return lookup_action(x, ctx)
        end
    elseif typeof(x) <: Tuple
        if x[1] == :quote
            return quote_action(x, ctx)
        elseif x[1] == :cond
            return cond_action(x, ctx)
        elseif x[1] == :lambda
            return lambda_action(x, ctx)
        elseif typeof(x[1]) <: Tuple
            return apply_action(x, ctx)
        else   # some other identifier, either builtin or not
            return apply_action(x, ctx)
        end
    end
    throw(ErrorException("Unexpected expression: $x"))
end

function value(ast)
    return meaning(ast, Dict())
end

function test_minimal_scheme()
    @assert 6 == value( ((:lambda, (:x, ), (+, 1, :x)), 5) )
    value( (:car, (:quote, (1, 2, 3, 4))) )
end
