defmodule Adventofcode.DayFive do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(5)
    |> Stream.map(fn x -> String.replace(x, "\n", "") end)
  end

  def seat_map(rows \\ 127, cols \\ 7) do
    map = for x <- 0..rows, y <- 0..cols, do: %{row: x, col: y, id: (x * 8) + y}
    map
    |> Enum.reject(fn x -> x.row == 0 || x.row == rows end)
    |> Enum.sort_by(fn x -> x.row end)
  end

  def decode(string) do
    [[rows], [cols]] = Regex.scan(~r/[(F|B)]+|[(R|L)]+/, string)
    {rows,cols}
    col = cols |> get(for x <- 0..7, do: x)
    row = rows |> get(for x <- 0..127, do: x)

    %{row: row, col: col, id: (row * 8) + col}
  end

  def get("", [row]), do: row
  def get(string, rows) do
    {x,y} = String.split_at(string, 1)
    {a,b} = half(rows)
    cond do
      (x == "F" || x == "L") -> get(y, a)
      ( x == "B" || x == "R") -> get(y, b)
    end
  end

  def half(list) do
    list
    |> Enum.split(div(length(list),2))
  end

end
