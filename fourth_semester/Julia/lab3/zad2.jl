function isexpr(ex, symbol)
  return ex.head == symbol
end

find_symbols(x::Number) = []
find_symbols(x::Symbol) = [x]
find_symbols(ex) = reduce(vcat, [], map(find_symbols, ex.args[2:end]))

macro fill_series(ex)
  l, r = ex.args # left- and right-hand sides of equation
  println(string("L: ", l, " R: ", r) )
  var_t =                   # expecting left to be array[expression]
                            # so second arg is expression in brackets
    if isa(l.args[2], Symbol)
      l.args[2]               # it's just a variable name so bind it
      println(string("args: ", l.args, " is a symbol"))
    elseif isexpr(l.args[2], :call) #if it's a call it's something like t+1
      println(string("args: ", l.args, " is a call"))
      #find first symbol that occurs in this call
      find_symbols(l)[1]
    end
  println(string("variable is: ", var_t))
  # find biggest and smallest offsets from left and right
  offsets = extrema(vcat(get_offsets(l), get_offsets(r)))
  println(string("Got offsets: ", offsets))
  #get range with expression of (1-smallest_offset):(legth_of_given_array-biggest_offset)
  loopindex = :($(1 - offsets[1]):(length($(l.args[1])) - $(offsets[2])))
  println(loopindex)
  println(string("for ", var_t, " in ", loopindex))
  println(string("    ", ex))
  quote
    for $var_t in $loopindex
      $ex
    end
  end
end

function get_offsets(ex_::Expr)
  println(string(ex_, " is of type ", typeof(ex_)))
  if isexpr(ex_,:call)
    println(string("ex ", ex_, " is a call"))
    # it's a call, so first arg is operator and the rest are its arguments so
    # we need to find refs in it
     return [[get_offsets(a) for a in ex_.args[2:end]]...]
   end

  if isexpr(ex_,:ref)
     println(string("ex ", ex_, " is a ref with arg 2: ", ex_.args[2]))
     return get_offset_from_ref(ex_.args[2])
   end

  warn("Not expecting to be here")
  return Int64[]
end

get_offsets(x) = Int64[]

get_offset_from_ref(s::Symbol) = 0
get_offset_from_ref(x::Number) = x

function get_offset_from_ref(ex_::Expr)
  if isexpr(ex_,:call)
    if ex_.args[1] == :+
      println(string("ex ", ex_, " arg 1 is + so gonna sum offsets from ", ex_.args[2:end]))
      return sum([get_offset_from_ref(a) for a in ex_.args[2:end]])
    end

    if ex_.args[1] == :-
      println(string("ex ", ex_, " arg 1 is - so gonna sub offsets from ", ex_.args[2:end]))
      #expression is in form of y[t-2] so get offset of t(=0) and sub offset of 2(=2)
      return (get_offset_from_ref(ex_.args[2]) - sum([get_offset_from_ref(a) for a in ex_.args[3:end]]))
    end
  end
  warn("Didn’t expect to get here")
  return(0)
end
