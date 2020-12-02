defmodule Adventofcode.DayOne do

  def product({x,y}), do: x * y

  def inputs do
    "resources/input.txt"
    |> Path.absname()
    |> File.stream!
    |> Stream.map(fn x -> get_int(x) end)
  end

  def input_pair(sum) do
    lis = for x <- inputs(), y <- inputs(), x + y == sum, do: {x,y}
    lis
    |> Enum.at(0)
  end

  defp get_int(str) do
    {x,_} = Integer.parse(str)
    x
  end

end
