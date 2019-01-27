defmodule Anagram do
  @doc """
  Returns all candidates that are anagrams of, but not equal to, 'base'.
  """
  @spec match(String.t(), [String.t()]) :: [String.t()]
  def match(base, candidates) do
    candidates
    |> Enum.filter(fn cand -> are_anagrams(base, cand) end)
  end

  @spec are_anagrams(String.t(), String.t()) :: boolean()
  def are_anagrams(st1, st2) do
    st1d = String.downcase(st1)
    st2d = String.downcase(st2)
    char_count(st1d) == char_count(st2d) && st1d != st2d
  end

  @doc """
  Given a string, return a map of character frequency counts
      iex> Anagram.char_count("hello")
      %{"e" => 1, "h" => 1, "l" => 2, "o" => 1} 
  """
  @spec char_count(String.t()) :: %{}
  def char_count(string) do
    string
    |> String.graphemes()
    |> Enum.reduce(%{}, fn (letter, acc) ->
      Map.update(acc, letter, 1, &(&1 + 1))
    end)
  end

end
