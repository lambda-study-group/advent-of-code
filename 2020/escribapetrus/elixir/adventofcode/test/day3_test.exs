defmodule DayThreeTest do
  use ExUnit.Case
  import Adventofcode.DayThree

  test "checks the position in a line" do
    assert iter_line(0) == 1
    assert iter_line(1) == 4
    assert iter_line(2) == 7
    assert iter_line(10) == 31
  end

end
