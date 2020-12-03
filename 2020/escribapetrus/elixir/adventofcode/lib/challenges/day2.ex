defmodule Adventofcode.DayTwo do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(2)
    |> Stream.map(fn x -> String.replace(x, "\n", "") end)
  end

  def correct_count(stream, validation_method) do
    correct_data(stream, validation_method) |> Enum.count()
  end

  def correct_data(stream, validation_method) do
    stream
    |> Stream.map(fn x -> String.split(x,": ") end)
    |> Stream.filter(fn [policy, password] -> valid?(policy, password, validation_method) end)
  end

  def valid?(policy, password, :old) do
    [range, char] = String.split(policy, " ")
    [min, max] = Regex.scan(~r/\d+/, range) |> Enum.map(fn [x] -> get_int(x) end)
    count_pass = Regex.scan(~r/#{char}/, password) |> Enum.count()
    cond do
      (count_pass >= min && count_pass <= max) -> true
      true -> false
    end
  end

  def valid?(policy, password, :new) do
    [range, char] = String.split(policy, " ")
    [min, max] = Regex.scan(~r/\d+/, range) |> Enum.map(fn [x] -> get_int(x) end)
    {a,b} = {String.at(password, min-1), String.at(password, max-1)}

    cond do
      (a == char && b == char) -> false
      (a == char || b == char) -> true
      true -> false
    end
  end

  defp get_int(str) do
    {x,_} = Integer.parse(str)
    x
  end
end
