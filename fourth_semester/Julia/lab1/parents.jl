function parents(the_type)
  _parents(the_type, 0)
end

function _parents(the_type, d)
  if(the_type != Any)
    _parents(supertype(the_type), d+1)
  end
  print(the_type)
  if(d > 0)
    print("--->")
  end
end
