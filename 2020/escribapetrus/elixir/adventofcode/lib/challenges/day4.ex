defmodule Adventofcode.DayFour do
  alias Adventofcode.Inputs
  alias Adventofcode.Passport

  def inputs do
    Inputs.get(4)
    |> Stream.map(fn x -> String.replace(x, "\n", "") end)
    |> Stream.flat_map(fn x -> String.split(x, " ") end)
    |> Stream.chunk_by(&(&1 != ""))
    |> Stream.filter(&(&1 != [""]))
  end

  def format_passport(data) do
    map = data |> Map.new(fn y ->
      [a,b] = String.split(y,":")
      {String.to_existing_atom(a),b} end)
    Map.merge(%Passport{},map)
  end

end



["iyr:2011", "cid:99", "ecl:amb", "eyr:2030", "hcl:#18171d", "hgt:165cm", "pid:897123249", "byr:1948"]
