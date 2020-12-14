defmodule Ship do
  use Agent

  defstruct [orientation: 0, x: 0, y: 0, waypoint: %{x: 10, y: 1}]

  def start(value \\ %Ship{}) do
    Agent.start(fn -> value end)
  end

  def value(ship) do
    Agent.get(ship, &(&1))
  end

  def move(ship, {dir, val}) do
    Agent.update(ship, &(move(&1, {dir,val}, :aux)))
  end

  def move(ship,val) do
    Agent.update(ship, &(move_wp(&1, val,:aux)))
  end

  def move_wp(ship, {dir, val}) do
    Agent.update(ship, &(move_wp(&1, {dir,val}, :aux)))
  end

  def move_wp(ship,val) do
    Agent.update(ship, &(move_wp(&1, val,:aux)))
  end

  def steer(ship, {dir, val}) do
    Agent.update(ship, &(steer(&1, {dir, val}, :aux)))
  end

  def steer_wp(ship, {dir, val}) do
    Agent.update(ship, &(steer_wp(&1, {dir, val}, :aux)))
  end

  def stop(ship) do
    Agent.stop(ship)
  end

  def run(ship, instruction) do
    {op, value} = instruction
    move_wp = ["E", "N", "W", "S"]
    move = ["F"]
    steer = ["L", "R"]
    cond do
      (op in move_wp) -> move_wp(ship,{op, value})
      (op in move) -> move_wp(ship, value)
      (op in steer) -> steer_wp(ship, {op, value})
      true -> :error
    end
  end

  defp move(ship = %Ship{}, {dir, val},:aux) do
    case dir do
      "E" -> %Ship{ship | x: ship.x + val}
      "N" -> %Ship{ship | y: ship.y + val}
      "W" -> %Ship{ship | x: ship.x - val}
      "S" -> %Ship{ship | y: ship.y - val}
    end
  end

  defp move(ship = %Ship{}, val, :aux) do
    case ship.orientation do
      0 -> %Ship{ship | x: ship.x + val }
      90 -> %Ship{ship | y: ship.y + val }
      180 -> %Ship{ship | x: ship.x - val }
      270 -> %Ship{ship | y: ship.y - val }
    end
  end

  defp move_wp(ship = %Ship{}, {dir, val},:aux) do
    case dir do
      "E" -> %Ship{ship | waypoint: %{ship.waypoint | x: ship.waypoint.x + val}}
      "N" -> %Ship{ship | waypoint: %{ship.waypoint | y: ship.waypoint.y + val}}
      "W" -> %Ship{ship | waypoint: %{ship.waypoint | x: ship.waypoint.x - val}}
      "S" -> %Ship{ship | waypoint: %{ship.waypoint | y: ship.waypoint.y - val}}
    end
  end

  defp move_wp(ship = %Ship{}, val, :aux) do
    %Ship{
      ship|
      x: ship.x + (val * ship.waypoint.x),
      y: ship.y + (val * ship.waypoint.y)
    }
  end

  defp steer(ship = %Ship{}, {"L", val}, :aux) do
    res = ship.orientation + val
    cond do
      (res >= 360) -> %Ship{ship | orientation: res - 360}
      true -> %Ship{ship | orientation: res}
    end
  end

  defp steer(ship = %Ship{}, {"R", val}, :aux) do
    res = ship.orientation - val
    cond do
      (res < 0) -> %Ship{ship | orientation: 360 + res}
      true -> %Ship{ship | orientation: res}
    end
  end

  def steer_wp(ship = %Ship{}, {"L", val}, :aux), do: steer_wp(ship,{"R",360 - val}, :aux)
  def steer_wp(ship = %Ship{}, {"R", 0}, :aux), do: ship
  def steer_wp(ship = %Ship{}, {"R", val}, :aux) do
    %{x: wp_x, y: wp_y} = ship.waypoint
    steer_wp(%Ship{ship | waypoint: %{x: wp_y, y: -wp_x}}, {"R", val - 90}, :aux)
  end

  # def steer_wp(ship = %Ship{}, {"L", 0}, :aux), do: ship
  # def steer_wp(ship = %Ship{}, {"L", val}, :aux) do
  #   val = 360 - val
  #   %{x: wp_x, y: wp_y} = ship.waypoint
  #   steer_wp(%Ship{ship | waypoint: %{x: wp_y, y: -wp_x}}, {"L", val - 90}, :aux)
  # end

end
