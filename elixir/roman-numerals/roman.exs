defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    number
    |> digits()
    |> Enum.map(&romanize/1)
    |> Enum.join("")
  end

  def romanize(0), do: ""
  def romanize(1), do: "I"
  def romanize(2), do: "II"
  def romanize(3), do: "III"
  def romanize(4), do: "IV"
  def romanize(5), do: "V"
  def romanize(6), do: "VI"
  def romanize(7), do: "VII"
  def romanize(8), do: "VIII"
  def romanize(9), do: "IX"
  def romanize(n) when n >= 10 and n < 40, do: "X" <> romanize(n - 10)
  def romanize(n) when n >= 40 and n < 50, do: "XL" <> romanize(n - 40)
  def romanize(n) when n >= 50 and n < 90, do: "L" <> romanize(n - 50)
  def romanize(n) when n >= 90 and n < 100, do: "XC" <> romanize(n - 90)
  def romanize(n) when n >= 100 and n < 400, do: "C" <> romanize(n - 100)
  def romanize(n) when n >= 400 and n < 500, do: "CD" <> romanize(n - 400)
  def romanize(n) when n >= 500 and n < 900, do: "D" <> romanize(n - 500)
  def romanize(n) when n >= 900 and n < 1000, do: "CM" <> romanize(n - 900)
  def romanize(n) when n >= 1000, do: "M" <> romanize(n - 1000)

  ## Digits: Convert a positive integer into a list of "terms".
  ## Hard to explain, look at the examples.
  ## digits(1234) = [1000, 200, 30, 4]
  ## digits(30098) = [30000, 0, 0, 90, 8]
  @spec digits(pos_integer) :: [integer]
  defp digits(x), do: digits(x, 0)
  defp digits(0, _), do: []
  defp digits(x, power_of_ten) do
    digits( div(x, 10), power_of_ten+1 ) ++ [rem(x, 10) * power(10, power_of_ten)]
  end

  defp power(n, k), do: :math.pow(n, k) |> round
end
