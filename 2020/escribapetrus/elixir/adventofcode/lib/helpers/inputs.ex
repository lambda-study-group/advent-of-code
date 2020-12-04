defmodule Adventofcode.Inputs do

  def get(nth) do
    "resources/input#{nth}"
    |> Path.expand()
    |> File.stream!
  end

end
