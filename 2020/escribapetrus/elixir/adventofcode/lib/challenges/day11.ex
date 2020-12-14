defmodule Adventofcode.DayEleven do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(11)
    |> Enum.map(&(&1 |> String.trim))
    |> Enum.map(&(&1 |> String.graphemes))
    |> Enum.map(fn x ->
        Enum.map(x, fn y ->
          case y do
            "L" -> 0
            "#" -> 1
            _ -> :floor
          end
        end)
      end)
  end


end
