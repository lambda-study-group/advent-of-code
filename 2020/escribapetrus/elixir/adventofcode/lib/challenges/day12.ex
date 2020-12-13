defmodule Adventofcode.DayTwelve do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(12)
    |> Enum.map(&(&1 |> String.trim))
    |> Enum.map(&(&1 |> format))
  end

  def format(instruction) do
    [[direction], [value]] = Regex.scan(~r/[A-Z]+|[0-9]+/, instruction)
    {value,_} = Integer.parse(value)
    {direction, value}
  end

end
