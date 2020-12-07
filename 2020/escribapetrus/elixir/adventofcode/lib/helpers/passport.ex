defmodule Adventofcode.Passport do
  defstruct [:byr, :iyr, :eyr, :hgt, :hcl, :ecl, :pid, cid: "0"]

  # def new(byr, iyr, eyr, hgt,hcl, ecl, pid,cid) do
  #   %Adventofcode.Passport{
  #     byr: byr,
  #     iyr: iyr,
  #     eyr: eyr,
  #     hgt: hgt,
  #     hcl: hcl,
  #     ecl: ecl,
  #     pid: pid,
  #     cid: cid
  #   }
  # end
end
