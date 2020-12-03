defmodule Adventofcode.DayThree do
  alias Adventofcode.Inputs

  def inputs do
      Inputs.get(3)
      |> Stream.map(fn x -> String.replace(x, "\n", "") end)
      |> Stream.map(fn x -> String.graphemes(x) end)
      |> Stream.map(fn x -> Stream.cycle(x) end)
  end

  def count_trees(stream) do
    lines = stream |> Enum.count()
    count_trees(stream, lines, 0, 0)
  end
  def count_trees(_,lines,trees,at) when lines == at + 1, do: trees
  def count_trees(stream,lines,trees,at) do
    case (get_position(stream,at)) do
      "#" -> count_trees(stream,lines, trees + 1, at + 1)
      _ -> count_trees(stream,lines, trees, at + 1)
    end
  end

  def get_position(stream,line) do
    stream
    |> Enum.at(line)
    |> Enum.at(iter_line(line))
  end

  def iter_line(0), do: 0
  def iter_line(nth) when nth > 0 do
    3 + iter_line(nth-1)
  end

end
