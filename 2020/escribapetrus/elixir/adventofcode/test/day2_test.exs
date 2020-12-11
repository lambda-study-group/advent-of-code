defmodule DayTwoTest do
  use ExUnit.Case
  alias Adventofcode.DayTwo

  test "checks is password is valid" do
    assert DayTwo.valid?("1-3 a", "abcde", :old) == true
    assert DayTwo.valid?("1-3 b", "cdefg", :old) == false
    assert DayTwo.valid?("2-9 c", "ccccccccc", :old) == true
  end

  test "checks is password is valid according to new policy" do
    assert DayTwo.valid?("1-3 a", "abcde",:new) == true
    assert DayTwo.valid?("1-3 b", "cdefg",:new) == false
    assert DayTwo.valid?("2-9 c", "ccccccccc",:new) == false

  end

end
