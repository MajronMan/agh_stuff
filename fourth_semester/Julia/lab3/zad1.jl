@generated function harm(x::Integer...)
  l = length(x)
  if(l == 0)
    return 0
  end
  ex = :(1/x[1])
  for i = 2:l
    ex = :($ex + 1/x[$i])
  end
  ex = :($l/$ex)
  println(ex)
  return ex
end

function harm2(x::Integer...)
  l = length(x)
  if(l == 0)
    return 0
  end
  ex = :(1/x[1])
  for i = 2:l
    ex = :($ex + 1/x[$i])
  end
  ex = :($l/$ex)
  return ex
end
