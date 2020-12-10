defmodule Adventofcode.DayNine do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(9)
    |> Stream.map(&(&1 |> String.trim))
    |> Stream.map(fn x ->
        {a,_b} = Integer.parse(x)
        a
      end)
  end

  def valid?(num, preambule) do
    combinations = for x <- preambule, y <- preambule, x != y, do: {x,y}
    combinations = combinations |> Enum.map(fn {x,y} -> x + y end) |> Enum.uniq
    num in combinations
  end

  def iter(stream) do
    [num|preambule] = stream |> Enum.take(26) |> Enum.reverse()
    valid = valid?(num,preambule)

    if valid == true do
      iter(Stream.drop(stream, 1))
    else
      num
    end
  end

  def iter(stream, chunk, target) do
    {x,xs} = stream |> Stream.reject(&(&1 >= target)) |> Enum.split(chunk)
    res = x |> Enum.sum()
    cond do
      (res == target) -> Enum.min(x) + Enum.max(x)
      (xs == []) -> iter(inputs(),chunk+1,target)
      true -> iter(xs, chunk, target)
    end
  end

end
