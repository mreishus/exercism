defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(number) do

    bottles_phrase = bottles_phrase(number)
    bottles_phrase_m1 = bottles_phrase(number - 1)
    what_to_do = what_to_do(number)

    "#{String.capitalize(bottles_phrase)} of beer on the wall, #{bottles_phrase} of beer.\n"
      <> "#{what_to_do}, #{bottles_phrase_m1} of beer on the wall.\n"
  end

  defp what_to_do(0), do: "Go to the store and buy some more"
  defp what_to_do(num) do
    pronoun = pronoun(num)
  	"Take #{pronoun} down and pass it around"
  end

  defp bottles_phrase(-1), do: "99 bottles"
  defp bottles_phrase(0), do: "no more bottles"
  defp bottles_phrase(1), do: "1 bottle"
  defp bottles_phrase(num), do: "#{num} bottles"

  defp pronoun(1), do: "it"
  defp pronoun(_number), do: "one"

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range) do
    range
    |> Enum.map(&verse/1)
    |> Enum.join("\n")
  end

  def lyrics(), do: lyrics(99..00)
end
