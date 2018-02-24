defmodule Pollution do
  defp readFile(name) do
    {:ok, file} = File.open(name, [:read])
    {:ok, read} = File.read(name)
    File.close(file)
    read
  end
  defp parseLine(line) do
    [date, time, e, n, val] = String.split(line, ",")
    %{
      datetime: {
        date |> String.split("-") |> Enum.reverse |> :erlang.list_to_tuple,
        time |> String.split(":")
        },
      location: {n, e},
      pollutionLevel: val
    }
  end
  defp stationFromRecord(record) do
    %{Map.get(record, :location) => Map.drop(record, [:location])}
  end
  def betterPut(map, key, value) do
    case Map.get(map, key) do
      nil -> Map.put(map, key, [value])
      old -> Map.put(map, key, old ++ [value])
    end
  end
  def createStations(data) do
    Enum.reduce(data, %{},
    fn(record, acc) -> betterPut(acc, record.location, Map.drop(record, [:location])) end)
  end
  def parseFile(name) do
    name
      |> readFile
      |> String.split
      |> Enum.map(fn(line) -> parseLine(line) end)
      |> createStations
  end
end
