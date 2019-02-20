defmodule RobotSimulator do
  @directions [:north, :east, :south, :west]

  defguard is_direction(direction) when direction in @directions
  defguard is_position(x, y) when is_integer(x) and is_integer(y)

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  def create(direction \\ :north, {x, y} = position \\ {0, 0})
      when is_direction(direction) and is_position(x, y) do
    %{position: position, direction: direction}
  end

  def create(direction, {x, y} = position)
      when not is_direction(direction) and is_position(x, y) do
    {:error, "invalid direction"}
  end

  def create(direction, _) do
    {:error, "invalid position"}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot :: any, instructions :: String.t()) :: any
  def simulate(robot, instructions) do
    instructions
    |> String.graphemes()
    |> Enum.reduce(robot, fn x, acc ->
      case acc do
        {:error, _} -> acc
        _ -> process_inst(acc, x)
      end
    end)
  end

  # process_inst: Updates a robot after one instruction.
  # "R", "L", and "A" are supported.
  # (Robot, String) -> Robot
  defp process_inst(robot, "R"), do: process_turn(robot, 1)
  defp process_inst(robot, "L"), do: process_turn(robot, -1)

  defp process_inst(%{direction: direction, position: position} = robot, "A") do
    %{robot | position: advance_position(position, direction)}
  end

  defp process_inst(_robot, _), do: {:error, "invalid instruction"}

  # process_turn: Updates a robot with a turn specified in number of 90 deg clockwise turns.
  # (Robot, Integer) -> Robot
  defp process_turn(%{direction: direction} = robot, direction_delta) do
    new_dir =
      @directions
      |> Enum.find_index(fn x -> x == direction end)
      |> Kernel.+(direction_delta)
      |> rem(4)
      |> (fn x -> Enum.at(@directions, x) end).()

    %{robot | direction: new_dir}
  end

  # advance_position: Gives position after taking a step forward.
  # (Position, Direction) -> Position
  defp advance_position(position, direction) do
    direction
    |> direction_vector
    |> addt(position)
  end

  # addt: Adds two tuples.
  defp addt({a, b}, {c, d}), do: {a + c, b + d}

  # direction_vector: Converts a direction (atom) into a unit vector.
  # (Direction) -> Position
  defp direction_vector(:north), do: {0, 1}
  defp direction_vector(:south), do: {0, -1}
  defp direction_vector(:west), do: {-1, 0}
  defp direction_vector(:east), do: {1, 0}

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot :: any) :: atom
  def direction(%{direction: d}), do: d

  @doc """
  Return the robot's position.
  """
  @spec position(robot :: any) :: {integer, integer}
  def position(%{position: p}), do: p
end
