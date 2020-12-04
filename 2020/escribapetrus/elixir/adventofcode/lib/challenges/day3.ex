defmodule Adventofcode.DayThree do
  alias Adventofcode.Inputs

  def inputs do
      Inputs.get(3)
      |> Stream.map(fn x -> String.replace(x, "\n", "") end)
      |> Stream.map(fn x -> String.graphemes(x) end)
      |> Stream.map(fn x -> Stream.cycle(x) end)
  end

  def position(stream, coordinates) do
    {x,y} = coordinates
    line = stream |> Enum.at(y)
    case line do
      nil -> nil
      _ -> Enum.at(line, x)
    end
  end

  def graph_coordinates(slope) do
    {x,y} = slope
    {0,0}
    |> Stream.iterate(fn {a,b} -> {a + x, b + y}  end)
  end

  def graph_values(graph,stream) do
    graph
    |> Stream.map(fn x -> position(stream, x) end)
  end

  def count_trees(stream, slope) do
    stream_size = Enum.count(stream)
    values = slope |> graph_coordinates() |> graph_values(stream)
    values
    |> Enum.take(stream_size)
    |> Enum.filter(fn x -> x == "#" end)
    |> Enum.count()
  end

end
