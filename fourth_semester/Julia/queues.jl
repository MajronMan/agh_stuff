type cust
  id :: Int64
end
id = 1
# function mn()
#   id = 1
#   stat = []
#   queues = [[], [], []]
#   for time in 1:1000
#     if rand(0:4) == 2
#       push!(queues[1+i%3], cust(id))
#       id++
#     end
#   end
#   print(queues)
# end

function mn()
stat = []
queues = [[], [], []]
id=1
  for time in 1:1000
    if rand(0:4) == 2
      push!(queues[1+time%3], cust(id))
      id = id+1
    end
  end
  print(queues)
end
# function main()
#   for time in 1:1000
#     if rand(0:4) == i % 5
#       push!(queues[1+i%3], [cust(id)])
#       id++
#     end
#   end
#   print(queues)
# end
