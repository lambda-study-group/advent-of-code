defmodule Adventofcode.DaySix do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(6)
    |> Stream.map(fn x -> String.replace(x, "\n", "") end)
    |> Stream.flat_map(fn x -> String.split(x, " ") end)
    |> Stream.chunk_by(&(&1 != ""))
    |> Stream.reject(&(&1 == [""]))
  end

  def count(answers, :all) do
    answers
    |> Stream.map(fn x ->
        x
        |> Enum.map(fn y -> String.graphemes(y) end)
        |> Enum.reduce(fn w, z -> for a <- w, a in z, do: a end)
        |> Enum.count()
      end)
    |> Enum.sum
  end

  def count(answers, :any) do
    answers
    |> Stream.map(fn x ->
      Enum.reduce(x, fn y, z -> y <> z end)
    end)
    |> Stream.map(fn x ->
      x |> String.graphemes() |> Enum.uniq() |> Enum.count()
    end)
    |> Enum.sum()
  end

end
