defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number), do: romanize("", number)

  defp romanize(acc, 0), do: acc <> ""
  defp romanize(acc, n) when n >= 1 and n < 4, do: romanize(acc <> "I", n - 1)
  defp romanize(acc, 4), do: acc <> "IV"
  defp romanize(acc, n) when n >= 5 and n < 9, do: romanize(acc <> "V", n - 5)
  defp romanize(acc, 9), do: acc <> "IX"
  defp romanize(acc, n) when n >= 10 and n < 40, do: romanize(acc <> "X", n - 10)
  defp romanize(acc, n) when n >= 40 and n < 50, do: romanize(acc <> "XL", n - 40)
  defp romanize(acc, n) when n >= 50 and n < 90, do: romanize(acc <> "L", n - 50)
  defp romanize(acc, n) when n >= 90 and n < 100, do: romanize(acc <> "XC", n - 90)
  defp romanize(acc, n) when n >= 100 and n < 400, do: romanize(acc <> "C", n - 100)
  defp romanize(acc, n) when n >= 400 and n < 500, do: romanize(acc <> "CD", n - 400)
  defp romanize(acc, n) when n >= 500 and n < 900, do: romanize(acc <> "D", n - 500)
  defp romanize(acc, n) when n >= 900 and n < 1000, do: romanize(acc <> "CM", n - 900)
  defp romanize(acc, n) when n >= 1000, do: romanize(acc <> "M", n - 1000)

end
