defmodule Adventofcode do
  alias Adventofcode.{DayOne, DayTwo, DayThree}


  def day_three do
    DayThree.inputs
    |> DayThree.count_trees()
    |> IO.puts
  end

end
