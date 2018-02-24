abstract 動物

type 捕食者 <: 動物
  位置 :: Array{Int64, 1}
  捕食者(x, y) = new([y, x])
end

type 獲物 <: 動物
  位置 :: Array{Int64, 1}
  獲物(く, ゆ) = new([ゆ, く])
end

type 虚 <: 動物
  虚() = new()
end

type 天地
  地図 :: Array{動物, 2}
  天地(ん) = new(fill(虚(), ん, ん))
end

function 動物を加える{つ}(世界, く, ゆ, 物::Type{つ})
  if(typeof(世界.地図[ゆ, く]) != 虚)
    error("場所が取られた")
  end
  世界.地図[ゆ, く] = 物(く, ゆ)
end

function 距離(動物一::動物, 動物二::動物)
  return abs(動物一.位置[1] - 動物二.位置[1]) + abs(動物一.位置[1] - 動物二.位置[1])
end

function 除去(世界::天地, 動物一::動物)
  世界.地図[動物一.位置[1], 動物一.位置[2]] = 虚()
end

function 対話(世界::天地, 動物一::捕食者, 動物二::獲物)
  除去(世界, 動物二)
end

function 移転する(世界::天地, 動物一::獲物)
  for い in 1:size(世界.地図)[1]
    for や in 1:size(世界.地図)[2]
      if(typeof(世界.地図[い, や]) == 虚)
        世界.地図[い, や] = 動物一
        動物一.位置 = [い, や]
        return
      end
    end
  end
end

function 対話(世界::天地, 動物一::獲物, 動物二::捕食者)
  除去(世界, 動物一)
  移転する(世界::天地, 動物一::獲物)
end

function 対話(世界::天地, 動物一::捕食者, 動物二::捕食者)
  return "ぶるっっっ"
end

function 対話(世界::天地, 動物一::獲物, 動物二::獲物)
  return "べえええ"
end

# s =
