defmodule DayTwoTest do
  use ExUnit.Case
  import Adventofcode.DaySeven

  test "checks if a bag fits in another" do
    assert fits?("1-3 a", "abcde", :old) == true
    assert valid?("1-3 b", "cdefg", :old) == false
    assert valid?("2-9 c", "ccccccccc", :old) == true
  end

end
