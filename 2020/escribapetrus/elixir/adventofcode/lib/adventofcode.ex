defmodule Adventofcode do
  alias Adventofcode.{DayOne, DayTwo, DayThree}

  def main do
    true
  end

  def day_one do
    a = DayOne.pair(2020) |> DayOne.product
    b = DayOne.triplet(2020) |> DayOne.product

    IO.inspect({a,b})
  end

  def day_two do
    inputs = DayTwo.inputs
    a = inputs |> DayTwo.correct_count(:old)
    b = inputs |> DayTwo.correct_count(:new)
    IO.inspect({a,b})
  end

  def day_three do
    vals = [{1,1}, {3,1}, {5,1}, {7,1}, {1,2}]
    inputs = DayThree.inputs

    a = inputs |> DayThree.count_trees({3,1})

    b = Enum.map(vals, fn x ->
      inputs
      |> DayThree.count_trees(x)
    end)
    |> Enum.reduce(&(&1*&2))

    IO.inspect({a,b})
  end

end
