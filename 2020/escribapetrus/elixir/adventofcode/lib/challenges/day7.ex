defmodule Adventofcode.DaySeven do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(7)
    |> Stream.map(fn x -> format(x) end)
    |> Stream.map(fn x -> rule(x) end)
  end

  def format(string) do
    string
    |> String.replace("\n", "")
    |> String.replace("bags", "")
    |> String.replace("bag", "")
    |> String.replace(".", "")
  end

  def rule(string) do
    [x,contains] = string |> String.split(" contain ")
    x = x |> String.trim
    contains = contains |> String.trim
    case contains do
      "no other" -> %{color: x, contains: contains}
      _ ->
        contains =
        contains
        |> String.split(", ")
        |> Enum.map(fn x ->
          [[num],[color]] = Regex.scan(~r/[1-9]+|[a-z- ]+/,x)
          {num,_} = num |> Integer.parse()
          color = color |> String.trim()
          {color, num}
        end)
        %{color: x, contains: contains}
    end
  end

  def containers_of(stream, color) do
    stream
    |> Stream.reject(fn x -> x.contains == "no other" end)
    |> Stream.filter(fn x ->
      x
      |> Enum.filter(fn {c, _} -> c == color end)
      |> Enum.any?()
    end)
  end

end
