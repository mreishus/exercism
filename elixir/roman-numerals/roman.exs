defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number), do: roman("", number)

  defp roman(acc, n) when n >= 1000, do: roman(acc <> "M", n - 1000)
  defp roman(acc, n) when n >= 900, do: roman(acc <> "CM", n - 900)
  defp roman(acc, n) when n >= 500, do: roman(acc <> "D", n - 500)
  defp roman(acc, n) when n >= 400, do: roman(acc <> "CD", n - 400)
  defp roman(acc, n) when n >= 100, do: roman(acc <> "C", n - 100)
  defp roman(acc, n) when n >= 90, do: roman(acc <> "XC", n - 90)
  defp roman(acc, n) when n >= 50, do: roman(acc <> "L", n - 50)
  defp roman(acc, n) when n >= 40, do: roman(acc <> "XL", n - 40)
  defp roman(acc, n) when n >= 10, do: roman(acc <> "X", n - 10)
  defp roman(acc, 9), do: acc <> "IX"
  defp roman(acc, n) when n >= 5, do: roman(acc <> "V", n - 5)
  defp roman(acc, 4), do: acc <> "IV"
  defp roman(acc, n) when n >= 1, do: roman(acc <> "I", n - 1)
  defp roman(acc, 0), do: acc
end
