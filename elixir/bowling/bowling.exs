defmodule Frame do
  defstruct rolls: [], score: nil, frame_no: 0
end

defmodule Game do
  defstruct frames: [], next_roll_frame: 1
end

defmodule Window do
  @doc """
    Window - creates a sliding window over a list

    iex> Window.window( [1, 2, 3, 4, 5, 6], 3 )
    [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]

  """
  def window([], _size), do: []

  def window([_x | xs] = list, size) do
    cond do
      length(list) >= size ->
        [Enum.take(list, size) | window(xs, size)]

      true ->
        window(xs, size)
    end
  end
end

defmodule Bowling do
  @max_frames 10
  @max_pins 10

  @doc """
    Creates a new game of bowling that can be used to store the results of
    the game
  """
  @spec start() :: any
  def start do
    %Game{}
  end

  @doc """
    Records the number of pins knocked down on a single roll. Returns `any`
    unless there is something wrong with the given number of pins, in which
    case it returns a helpful message.
  """
  @spec roll(any, integer) :: any | String.t()
  def roll(_, roll) when roll < 0, do: {:error, "Negative roll is invalid"}
  def roll(_, roll) when roll > @max_pins, do: {:error, "Pin count exceeds pins on the lane"}

  def roll(game, roll) do
    case game_over?(game) do
      true ->
        {:error, "Cannot roll after game is over"}

      false ->
        game
        |> add_roll(roll)
        |> add_scores
        |> update_next_roll_frame
        |> roll_error_check
    end
  end

  # roll_error_check :: (game) -> game or {:error, "..."}
  # Returns a game with no changes most of the time, but checks for
  # invalid roll sums. (A frame with rolls of [6, 6] is invalid and checked here.)
  def roll_error_check(game) do
    invalid_rollsums =
      game.frames
      |> Enum.map(fn f -> Enum.sum(f.rolls) end)
      |> Enum.filter(fn rollsum -> rollsum > @max_pins end)

    cond do
      Enum.count(invalid_rollsums) > 0 -> {:error, "Pin count exceeds pins on the lane"}
      true -> game
    end
  end

  # add_roll :: (game, integer) -> game
  # Records a roll by either appending it to the current frame or
  # creating a new frame with the roll value.
  defp add_roll(game, roll) do
    new_frames =
      cond do
        make_new_frame?(game) ->
          game.frames |> add_new_frame(roll)

        true ->
          game.frames |> roll_to_latest_frame(roll)
      end

    %{game | frames: new_frames}
  end

  # make_new_frame :: (game) -> boolean
  # Should the next roll of this game go in a new frame?
  # Assumes the "game.next_roll_frame" variable is set correctly.
  defp make_new_frame?(game), do: game.next_roll_frame > length(game.frames)

  # add_new_frame :: ( [frame], roll ) -> [frame]
  # Creates a new frame with a roll value stored, and prepends it to the
  # list of frames given.
  defp add_new_frame(frames, roll) do
    new_frame = %Frame{rolls: [roll], score: nil, frame_no: max_frame_no(frames) + 1}
    [new_frame | frames]
  end

  ## max_frame_no :: ( [frame] ) -> Integer
  ## The maximum frame number in the set of frames.  0 if no frames.
  defp max_frame_no([]), do: 0

  defp max_frame_no(frames) do
    frames |> Enum.map(fn x -> x.frame_no end) |> Enum.max()
  end

  # roll_to_latest_frame :: ( [frame], roll) -> [frame]
  # Updates the latest frame (the first in the list) with the roll number given.
  # In both frames and rolls, the most recent stuff is at the beginning of the list.
  defp roll_to_latest_frame([f | fs], roll) do
    new_frame = %{f | rolls: [roll | f.rolls]}
    [new_frame | fs]
  end

  # add_scores :: (game) -> game
  # Take a game and recompute all scores from rolls.
  defp add_scores(game) do
    new_frames =
      game.frames
      |> get_frames_with_context()
      |> Enum.reduce([], fn {frame, context}, acc ->
        frame = %{frame | score: score_frame(frame, context)}
        [frame | acc]
      end)

    %{game | frames: new_frames}
  end

  # get_frames_with_context :: ([frames]) -> [ { frame, [next_frames] }, ... ]
  # Given a list of frames, we will return that list in reverse order, zipped
  # with context.  Context is a 3 length list consisting of that frame andthe
  # next two frames rolled (or nil if not yet rolled.)
  #
  # Example:
  # get_frames_with_context([f5, f4, f3, f2, f1]) =
  #   [ {f1, [f1, f2, f3] },
  #     {f2, [f2, f3, f4] },
  #     {f3, [f3, f4, f5] },
  #     {f4, [f4, f5, nil] },
  #     {f5, [f5, nil, nil] } ]
  # The idea is to allow a reduce() to look at the next two frames while computing
  # the score.
  defp get_frames_with_context(frames) do
    framesR = Enum.reverse(frames)
    windows = Window.window(framesR ++ [nil, nil], 3)
    Enum.zip(framesR, windows)
  end

  # score_frame :: (frame, [frame_and_next_two_frames]) -> Int
  # Given a frame and its context created from get_frames_with_context,
  # return the score of that frame.
  defp score_frame(%{frame_no: frame_no}, _) when frame_no > @max_frames, do: 0

  defp score_frame(frame, [_ | next_frames]) do
    score = frame.rolls |> Enum.sum()

    extra_score =
      cond do
        strike?(frame) -> next_2_rolls(next_frames)
        spare?(frame) -> next_roll(next_frames)
        true -> 0
      end

    score + extra_score
  end

  # next_roll :: ([frames]) -> Int
  # Given a list of context frames, pick the next roll that happened, or 0 if there
  # is no next roll yet.
  defp next_roll([frame | _frames]) when not is_nil(frame), do: List.last(frame.rolls)
  defp next_roll(_), do: 0

  # next_2_rolls :: ([frames]) -> Int
  # Given a list of context frames, pick the next two rolls that happened and return their sum.
  # Any rolls that have not happened yet are considered 0.
  defp next_2_rolls([frame | frames]) when not is_nil(frame) do
    case length(frame.rolls) do
      2 -> frame.rolls |> Enum.sum()
      1 -> Enum.at(frame.rolls, 0) + next_roll(frames)
      _ -> 0
    end
  end

  defp next_2_rolls(_), do: 0

  # update_next_roll_frame :: (game) -> game
  # Returns a game, but increments the "game.next_roll_frame" counter if the
  # next roll should go in a new frame.
  defp update_next_roll_frame(game) do
    cond do
      needs_new_frame?(game) -> %{game | next_roll_frame: game.next_roll_frame + 1}
      true -> game
    end
  end

  # needs_new_frame? :: (game) -> boolean
  # If this game had another roll, would it need a new frame?
  defp needs_new_frame?(game) do
    [f | _fs] = game.frames
    length(f.rolls) == 2 || strike?(f)
  end

  # strike? :: (frame) -> boolean
  defp strike?(f), do: length(f.rolls) == 1 && Enum.at(f.rolls, 0) == @max_pins
  # spare? :: (frame) -> boolean
  defp spare?(f), do: length(f.rolls) > 1 && Enum.sum(f.rolls) == @max_pins

  # game_over? :: (game) -> boolean
  defp game_over?(%{frames: frames} = _game) when length(frames) < @max_frames, do: false

  defp game_over?(%{frames: frames} = game) do
    tenth_frame = frames |> Enum.filter(fn fr -> fr.frame_no == @max_frames end) |> Enum.at(0)

    cond do
      strike?(tenth_frame) -> bonus_rolls_after_last_frame(frames) == 2
      spare?(tenth_frame) -> bonus_rolls_after_last_frame(frames) == 1
      true -> game.next_roll_frame == 11
    end
  end

  defp game_over?(_), do: false

  # bonus_rolls_after_last_frame :: ([frame]) -> int
  defp bonus_rolls_after_last_frame(frames) do
    frames
    |> Enum.filter(fn fr -> fr.frame_no > @max_frames end)
    |> Enum.flat_map(fn fr -> fr.rolls end)
    |> Enum.count()
  end

  @doc """
    Returns the score of a given game of bowling if the game is complete.
    If the game isn't complete, it returns a helpful message.
  """
  @spec score(any) :: integer | String.t()
  def score(game) do
    case game_over?(game) do
      true -> do_score(game)
      false -> {:error, "Score cannot be taken until the end of the game"}
    end
  end

  defp do_score(game) do
    game.frames
    |> Enum.map(fn x -> x.score end)
    |> Enum.sum()
  end
end
