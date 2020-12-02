defmodule AdventofcodeTest do
  use ExUnit.Case
  doctest Adventofcode

  test "greets the world" do
    assert Adventofcode.hello() == :world
  end
end
