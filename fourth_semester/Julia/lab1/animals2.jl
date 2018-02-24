abstract Zwierzę

type Drapieżnik <: Zwierzę
  Pozycja :: Array{Int64, 1}
  Drapieżnik(x, y) = new([y, x])
end

type Ofiara <: Zwierzę
  Pozycja :: Array{Int64, 1}
  Ofiara(x, y) = new([y, x])
end

type Pustak <: Zwierzę
  Pustak() = new()
end

type Świat
  Mapa :: Array{Zwierzę, 2}
  Świat(n) = new(fill(Pustak(), n, n))
end

function DodajZwierzę{T <: Zwierzę}(MójŚwiat, x, y, Zwierz::Type{T})
  if(typeof(MójŚwiat.Mapa[y, x]) != Pustak)
    error("Zajęte")
  end
  MójŚwiat.Mapa[y, x] = Zwierz(x, y)
end

function Odległość{T<: Zwierzę, W <: Zwierzę}(Zwierzę1::T, Zwierzę2::W)
  return abs(Zwierzę1.Pozycja[1] - Zwierzę2.Pozycja[1]) + abs(Zwierzę1.Pozycja[1] - Zwierzę2.Pozycja[1])
end

function Usuń(MójŚwiat::Świat, Zwierzę1::Zwierzę)
  MójŚwiat.Mapa[Zwierzę1.Pozycja[1], Zwierzę1.Pozycja[2]] = Pustak()
end

function Interakcja(MójŚwiat::Świat, Zwierzę1::Drapieżnik, Zwierzę2::Ofiara)
  Usuń(MójŚwiat, Zwierzę2)
end

function Uciekaj(MójŚwiat::Świat, Zwierzę1::Ofiara)
  for i in 1:size(MójŚwiat.Mapa)[1]
    for j in 1:size(MójŚwiat.Mapa)[2]
      if(typeof(MójŚwiat.Mapa[i, j]) == Pustak)
        MójŚwiat.Mapa[i, j] = Zwierzę1
        Zwierzę1.Pozycja = [i, j]
        return
      end
    end
  end
end

function Interakcja(MójŚwiat::Świat, Zwierzę1::Ofiara, Zwierzę2::Drapieżnik)
  Usuń(MójŚwiat, Zwierzę1)
  Uciekaj(MójŚwiat::Świat, Zwierzę1::Ofiara)
end

function Interakcja(MójŚwiat::Świat, Zwierzę1::Drapieżnik, Zwierzę2::Drapieżnik)
  return "WRrrr"
end

function Interakcja(MójŚwiat::Świat, Zwierzę1::Ofiara, Zwierzę2::Ofiara)
  return "Beeee"
end
