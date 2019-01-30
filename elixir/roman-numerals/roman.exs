defmodule Roman do
  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    romanize(number)
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

end
