defmodule DayNineTest do
  use ExUnit.Case
  alias Adventofcode.DayNine

  test "checks if n is valid using preambule" do
    preambule = 1..25
    assert DayNine.valid?(26, preambule) == true
    assert DayNine.valid?(49, preambule) == true
    assert DayNine.valid?(100, preambule) == false
    assert DayNine.valid?(50, preambule) == false
  end

end
