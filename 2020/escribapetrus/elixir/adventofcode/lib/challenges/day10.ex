defmodule Adventofcode.DayTen do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(10)
    |> Stream.map(&(&1 |> String.trim))
    |> Stream.map(fn x ->
        {a,_b} = Integer.parse(x)
        a
      end)
    |> Enum.sort()
    |> Enum.reverse()
  end

  def plug_device(stream) do
    [x|_] = stream
    [x+3|stream]
  end

  def differences([x|[]]), do: [x]
  def differences(stream) do
    [x|[y|tail]] = stream
    [x-y | differences([y|tail])]
  end

  def split_differences(stream), do: split_differences(stream,0,0,0)
  def split_differences([],a,b,c), do: {a,b,c}
  def split_differences(stream,a,b,c) do
    [x|xs] = stream
    case x do
      1 -> split_differences(xs,a+1,b,c)
      2 -> split_differences(xs,a,b+1,c)
      3 -> split_differences(xs,a,b,c+1)
      _ -> split_differences(xs,a,b,c)
    end

  end


# arrangements :: [Int] -> Int
# arrangements nums =
#   let nums' = Vector.fromList (sort nums)
#       v     = Vector.generate (length nums) countArrangements
#       countArrangements x
#         | x == (length nums) - 1 = 1
#         | otherwise =
#             let valids = Vector.takeWhile ((<= 3) . (subtract (nums' ! x))) (Vector.drop (x + 1) nums')
#                 indices = [(x + 1)..(x + (length valids))]
#             in sum' ((v !) <$> indices)
#   in v ! 0

  def product(tup) do
    {a,_,c} = tup
    a * c
  end

  def fits?(x,y) do
    abs(x - y) <= 3
  end

  def works?([x|[]]), do: fits?(x,0)
  def works?(stream) do
    [x|[y|tail]] = stream
    cond do
      (fits?(x,y) == true) -> works?([y|tail])
      true -> false
    end
  end

end
