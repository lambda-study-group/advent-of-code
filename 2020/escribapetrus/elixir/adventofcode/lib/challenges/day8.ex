defmodule Adventofcode.DayEight do
  alias Adventofcode.Inputs

  def inputs do
    Inputs.get(8)
    |> Enum.map(&(&1 |> String.trim))
  end

  def run(stream), do: run(stream,0,0,0,[])
  def run(stream, position, acc, fix, cache) do
    if position in cache do
      run(update(inputs(),fix),0,0,fix+1,[])
    else
      instruction = stream |> Enum.at(position) |> interpret()
      case instruction do
        {"acc",val} -> run(stream,position + 1, acc + val, fix, [position|cache])
        {"nop",_} -> run(stream,position + 1, acc, fix, [position|cache])
        {"jmp",val} -> run(stream,position + val, acc, fix, [position|cache])
        _ -> acc
      end
    end
  end

  def interpret(nil), do: nil
  def interpret(string) do
    [op,val] = String.split(string)
    {val,_} = Integer.parse(val)
    {op,val}
  end

  def update(lis,position) do
    val = Enum.at(lis, position)
    cond do
      (String.match?(val, ~r/jmp/)) -> List.replace_at(lis,position, String.replace(val,"jmp","nop"))
      (String.match?(val, ~r/nop/)) -> List.replace_at(lis,position, String.replace(val,"nop","jmp"))
      true -> lis
    end
  end

end
