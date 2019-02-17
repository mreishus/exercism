defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: count(l, 0)

  defp count([], acc), do: acc
  defp count([x | xs], acc), do: count(xs, acc + 1)

  @spec reverse(list) :: list
  def reverse(l), do: reverse(l, [])

  defp reverse([], l2), do: l2
  defp reverse([x | xs], l2), do: reverse(xs, [x | l2])

  @spec map(list, (any -> any)) :: list
  def map([], _f), do: []
  def map([x | xs], f), do: [f.(x) | map(xs, f)]

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter([], _f), do: []

  def filter([x | xs], f) do
    case f.(x) do
      true -> [x | filter(xs, f)]
      false -> filter(xs, f)
    end
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc

  def reduce([x | xs], acc, f) do
    reduce(xs, f.(x, acc), f)
  end

  @spec append(list, list) :: list
  def append(a, []), do: a
  def append([], b), do: b

  def append(a, b) do
    do_append(b, reverse(a))
  end

  defp do_append([], []), do: []
  defp do_append(a, []), do: a
  defp do_append([], b), do: b

  defp do_append(a, [b | bs]) do
    do_append([b | a], bs)
  end

  @spec concat([[any]]) :: [any]
  def concat([]), do: []

  def concat([x | xs]) do
    append(x, concat(xs))
  end
end
