defmodule Adventofcode do
  alias Adventofcode.{DayOne, DayTwo, DayThree,DayFour, DayFive, DaySeven, DayEight, DayNine}

  def main do
    challenge(:nine)
  end

  def challenge(:one) do
    a = DayOne.pair(2020) |> DayOne.product
    b = DayOne.triplet(2020) |> DayOne.product

    IO.inspect({a,b})
  end

  def challenge(:two) do
    inputs = DayTwo.inputs
    a = inputs |> DayTwo.correct_count(:old)
    b = inputs |> DayTwo.correct_count(:new)
    IO.inspect({a,b})
  end

  def challenge(:three) do
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

  def challenge(:four) do
    inputs = DayFour.inputs

    inputs
    |> Stream.map(fn x -> DayFour.format_passport(x) end)
    |> Stream.filter(fn x -> DayFour.valid?(x,:old) end)
    |> Stream.filter(fn x -> DayFour.valid?(x,:new) end)
    |> Enum.count()
    |> IO.inspect()
  end

  def challenge(:five) do
    inputs = DayFive.inputs

    a = inputs = inputs
    |> Stream.map(fn x -> DayFive.decode(x) end)
    |> Enum.sort_by(fn x -> x.id end)

    b = DayFive.seat_map()
    |> Enum.reject(fn x -> x in inputs end)
    |> Enum.filter(fn x ->
        Enum.find(inputs, fn y -> x.id + 1 == y.id end)
      end)
    |> Enum.filter(fn x ->
      Enum.find(inputs, fn y -> x.id - 1 == y.id end)
    end)
    |> Enum.at(0)

    {a |> Enum.max_by(fn x -> x.id end), b}
    |> IO.inspect
  end

  def challenge(:seven) do
    inputs = DaySeven.inputs()
    inputs
    |> DaySeven.expand("shiny gold")
    |> Enum.count
    |> IO.inspect
  end

  def challenge(:eight) do
    inputs = DayEight.inputs()
    inputs
    |> DayEight.run()
    |> IO.inspect()
  end

  def challenge(:nine) do
    inputs = DayNine.inputs()
    inputs
    |> DayNine.iter(2,25918798)
    |> IO.inspect()
  end
end
