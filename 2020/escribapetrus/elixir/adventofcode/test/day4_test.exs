defmodule DayFourTest do
  use ExUnit.Case
  alias Adventofcode.Passport
  alias Adventofcode.DayFour

  @one %Passport{ecl: "gry", pid: "860033327", eyr: "2020", hcl: "#fffffd", byr: "1937", iyr: "2017", cid: "147", hgt: "183cm"}
  @two %Passport{iyr: "2013", ecl: "amb", cid: "350", eyr: "2023", pid: "028048884", hcl: "#cfa07d", byr: "1929"}
  @three %Passport{hcl: "#ae17e1", iyr: "2013", eyr: "2024", ecl: "brn", pid: "760753108", byr: "1931", hgt: "179cm", cid: "0"}
  @four %Passport{hcl: "#cfa07d", eyr: "2025", pid: "166559648", iyr: "2011", ecl: "brn", hgt: "59in"}
  @five %Passport{eyr: "1972", cid: "100", hcl: "#18171d", ecl: "amb", hgt: "170", pid: "186cm", iyr: "2018", byr: "1926"}
  @six %Passport{iyr: "2019", hcl: "#602927", eyr: "1967", hgt: "170cm", ecl: "grn", pid: "012533040", byr: "1946"}
  @seven %Passport{hcl: "dab227", iyr: "2012", ecl: "brn", hgt: "182cm", pid: "021572410", eyr: "2020", byr: "1992", cid: "277"}
  @eight %Passport{hgt: "59cm", ecl: "zzz",  eyr: "2038", hcl: "74454a", iyr: "2023", pid: "3556412378", byr: "2007"}
  @nine %Passport{pid: "087499704", hgt: "74in", ecl: "grn", iyr: "2012", eyr: "2030", byr: "1980", hcl: "#623a2f"}
  @ten %Passport{eyr: "2029", ecl: "blu", cid: "129", byr: "1989", iyr: "2014", pid: "896056539", hcl: "#a97842", hgt: "165cm"}
  @eleven %Passport{hcl: "#888785", hgt: "164cm", byr: "2001", iyr: "2015", cid: "88", pid: "545766238", ecl: "hzl", eyr: "2022"}
  @twelve %Passport{iyr: "2010", hgt: "158cm", hcl: "#b6652a", ecl: "blu", byr: "1944", eyr: "2021", pid: "093154719"}


  test "checks if a passport entry is valid using old method" do
    assert DayFour.valid?(@four, :old) == false
    assert DayFour.valid?(@two, :old) == false
    assert DayFour.valid?(@one, :old) == true
    assert DayFour.valid?(@three, :old) == true
  end

  test "checks if a passport entry is valid using new method" do
    assert DayFour.valid?(@five, :new) == false
    assert DayFour.valid?(@six, :new) == false
    assert DayFour.valid?(@seven, :new) == false
    assert DayFour.valid?(@eight, :new) == false
    assert DayFour.valid?(@nine, :new) == true
    assert DayFour.valid?(@ten, :new) == true
    assert DayFour.valid?(@eleven, :new) == true
    assert DayFour.valid?(@twelve, :new) == true
  end

  test "format a passport from a string of data" do
    a = ["ecl:gry", "pid:860033327", "eyr:2020", "hcl:#fffffd", "byr:1937",
          "iyr:2017", "cid:147", "hgt:183cm"]
    b = ["iyr:2013", "ecl:amb", "cid:350", "eyr:2023", "pid:028048884",
          "hcl:#cfa07d", "byr:1929"]
    c = ["hcl:#ae17e1", "iyr:2013", "eyr:2024", "ecl:brn", "pid:760753108",
          "byr:1931", "hgt:179cm", "cid:0"]
    d = ["hcl:#cfa07d", "eyr:2025", "pid:166559648", "iyr:2011", "ecl:brn", "hgt:59in"]

    assert DayFour.format_passport(a) == @one
    assert DayFour.format_passport(b) == @two
    assert DayFour.format_passport(c) == @three
    assert DayFour.format_passport(d) == @four
  end

end
