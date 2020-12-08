defmodule Adventofcode.DayFour do
  alias Adventofcode.Inputs
  alias Adventofcode.Passport

  def inputs do
    Inputs.get(4)
    |> Stream.map(fn x -> String.replace(x, "\n", "") end)
    |> Stream.flat_map(fn x -> String.split(x, " ") end)
    |> Stream.chunk_by(&(&1 != ""))
    |> Stream.reject(&(&1 == [""]))
  end

  def format_passport(data) do
    map = data |> Map.new(fn y ->
      [a,b] = String.split(y,":")
      {String.to_existing_atom(a),b} end)
    Map.merge(%Passport{},map)
  end

  def valid?(passport = %Passport{}, :old) do
    passport
    |> Map.values()
    |> Enum.all?(fn x -> x != nil end)
  end

  def valid?(passport = %Passport{}, :new) do
    conditions = %{
      byr: fn x -> #check byr
        {x,_} = Integer.parse(x)
        x in 1920..2002
      end,
      iyr: fn x -> #check iyr
        {x,_} = Integer.parse(x)
        x in 2010..2020
      end,
      eyr: fn x -> #check eyr
        {x,_} = Integer.parse(x)
        x in 2020..2030
      end,
      hgt: fn x -> #check hgt
        {x,y} = Integer.parse(x)
        case y do
          "cm" -> x >= 150 && x <= 193
          "in" -> x >= 59 && x <= 76
          _ -> false
        end
      end,
      hcl: fn x -> #check hcl
        String.match?(x, ~r/^#[0-9a-f]{6}/i)
      end,
      ecl: fn x -> #check ecl
        x in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      end,
      pid: fn x -> #check pid
        String.match?(x, ~r/[0-9]{9}/)
      end,
      cid: fn _ -> true end #check cid
    }

    passport
    |> Map.merge(conditions, fn _k, x,y -> y.(x) end)
    |> Map.values()
    |> Enum.all?()
    # |> Enum.drop(1)
    # |> Map.values()
    # |> Enum.zip(Map.values(conditions))
    # |> Enum.all?(fn {x,y} -> y.(x) end)
  end

end
