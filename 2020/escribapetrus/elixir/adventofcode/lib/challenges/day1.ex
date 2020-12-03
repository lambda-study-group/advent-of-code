defmodule Adventofcode.DayOne do
  alias Adventofcode.Inputs

  def product({x,y}), do: x * y
  def product({x,y,z}), do: x * y * z

  def inputs do
    Inputs.get(1)
    |> Stream.map(fn x -> get_int(x) end)
  end

  def input_pair(sum) do
    lis = for x <- inputs(), y <- inputs(), x + y == sum, do: {x,y}
    lis
    |> Enum.at(0)
  end

  def input_triplet(sum) do
    lis = for x <- inputs(), y <- inputs(), z <- inputs(), x + y + z == sum, do: {x,y,z}
    lis
    |> Enum.at(0)
  end

  defp get_int(str) do
    {x,_} = Integer.parse(str)
    x
  end

end
