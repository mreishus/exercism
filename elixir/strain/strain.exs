defmodule Strain do
  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns true.

  Do not use `Enum.filter`.
  """
  @spec keep(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def keep([], _fun), do: []
  def keep([x | xs], fun) do
    case fun.(x) do
      true -> [x | keep(xs, fun)]
      false -> keep(xs, fun)
    end
  end

  @doc """
  Given a `list` of items and a function `fun`, return the list of items where
  `fun` returns false.

  Do not use `Enum.reject`.
  """
  @spec discard(list :: list(any), fun :: (any -> boolean)) :: list(any)
  def discard([], _fun), do: []
  def discard([x | xs], fun) do
    case fun.(x) do
      true -> discard(xs, fun)
      false -> [x | discard(xs, fun)]
    end
  end
end
