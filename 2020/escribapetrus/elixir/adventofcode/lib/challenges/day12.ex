defmodule Adventofcode.DayTwelve do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(12)
    |> Enum.map(&(&1 |> String.trim))
    |> Enum.map(&(&1 |> format))
  end

  def format(instruction) do
    [[op], [value]] = Regex.scan(~r/[A-Z]+|[0-9]+/, instruction)
    {value,_} = Integer.parse(value)
    {op, value}
  end

  def run([], _ship), do: []
  def run(instructions, ship) do
    [x|xs] = instructions
    Ship.run(ship, x)
    run(xs, ship)
  end

end
