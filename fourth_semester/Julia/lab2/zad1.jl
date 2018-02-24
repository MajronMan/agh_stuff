function testA()
  a = []
  for i in 1:1000
    push!(a, repeat("a", i))
  end
  return a
end

function testB()
  b = []
  for i in 1:10000
    push!(b, repeat("b", i))
  end
  return b
end

function tester()
  for i in 1:100
    println(length(testA()))
    println(length(testB()))
  end
end
