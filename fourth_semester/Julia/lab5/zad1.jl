int i = 1
ans = []
@async function f1()
  i = 1 + i%3
  ans.push(i)
end
