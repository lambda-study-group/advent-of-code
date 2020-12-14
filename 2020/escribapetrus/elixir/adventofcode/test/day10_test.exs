defmodule DayTenTest do
  use ExUnit.Case
  alias Adventofcode.DayTen

  test "checks if x  fits in y" do
    assert DayTen.fits?(1, 0) == true
    assert DayTen.fits?(4, 1) == true
    assert DayTen.fits?(10, 5) == false
    assert DayTen.fits?(123, 7) == false
  end

end
