defmodule BinTree do
  import Inspect.Algebra

  @moduledoc """
  A node in a binary tree.

  `value` is the value of a node.
  `left` is the left subtree (nil if no subtree).
  `right` is the right subtree (nil if no subtree).
  """
  @type t :: %BinTree{value: any, left: BinTree.t() | nil, right: BinTree.t() | nil}
  defstruct value: nil, left: nil, right: nil

  # A custom inspect instance purely for the tests, this makes error messages
  # much more readable.
  #
  # BT[value: 3, left: BT[value: 5, right: BT[value: 6]]] becomes (3:(5::(6::)):)
  def inspect(%BinTree{value: v, left: l, right: r}, opts) do
    concat([
      "(",
      to_doc(v, opts),
      ":",
      if(l, do: to_doc(l, opts), else: ""),
      ":",
      if(r, do: to_doc(r, opts), else: ""),
      ")"
    ])
  end
end

defmodule LeftCrumb do
  @type t :: %LeftCrumb{value: any, tree: BinTree.t()}
  defstruct value: nil, tree: nil
end

defmodule RightCrumb do
  @type t :: %RightCrumb{value: any, tree: BinTree.t()}
  defstruct value: nil, tree: nil
end

defmodule TestZ do
  alias BinTree, as: BT

  def testme do
    t = t1()
    t |> IO.inspect()
    {t, []} |> go_left |> go_right |> go_up |> go_up |> IO.inspect()
  end

  def go_left({nil, breadcrumbs}) when is_list(breadcrumbs), do: {nil, breadcrumbs}

  def go_left({tree, breadcrumbs}) when is_list(breadcrumbs) do
    new_crumb = %LeftCrumb{value: tree.value, tree: tree.right}
    {tree.left, [new_crumb | breadcrumbs]}
  end

  def go_right({nil, breadcrumbs}) when is_list(breadcrumbs), do: {nil, breadcrumbs}

  def go_right({tree, breadcrumbs}) when is_list(breadcrumbs) do
    new_crumb = %RightCrumb{value: tree.value, tree: tree.left}
    {tree.right, [new_crumb | breadcrumbs]}
  end

  def go_up({tree, []}), do: {tree, []}

  def go_up({tree, [%LeftCrumb{} = crumb | breadcrumbs]}) do
    new_tree = %BT{
      value: crumb.value,
      left: tree,
      right: crumb.tree
    }

    {new_tree, breadcrumbs}
  end

  def go_up({tree, [%RightCrumb{} = crumb | breadcrumbs]}) do
    new_tree = %BT{
      value: crumb.value,
      left: crumb.tree,
      right: tree
    }

    {new_tree, breadcrumbs}
  end

  defp bt(value, left, right), do: %BT{value: value, left: left, right: right}
  defp leaf(value), do: %BT{value: value}

  defp t1, do: bt(1, bt(2, nil, leaf(3)), leaf(4))
end

defmodule Zipper do
  alias BinTree, as: BT

  @doc """
  Get a zipper focused on the root node.
  """
  @spec from_tree(BT.t()) :: Z.t()
  def from_tree(bt) do
    {bt, []}
  end

  @doc """
  Get the complete tree from a zipper.
  """
  @spec to_tree(Z.t()) :: BT.t()
  def to_tree({tree, []}), do: tree

  def to_tree({tree, breadcrumbs}) do
    {tree, breadcrumbs} |> up |> to_tree
  end

  @doc """
  Get the value of the focus node.
  """
  @spec value(Z.t()) :: any
  def value({tree, _}), do: tree.value

  @doc """
  Get the left child of the focus node, if any.
  """
  @spec left(Z.t()) :: Z.t() | nil
  def left({nil, breadcrumbs}) when is_list(breadcrumbs), do: {nil, breadcrumbs}

  def left({tree, breadcrumbs}) when is_list(breadcrumbs) do
    new_crumb = %LeftCrumb{value: tree.value, tree: tree.right}

    case tree.left do
      nil -> nil
      _ -> {tree.left, [new_crumb | breadcrumbs]}
    end
  end

  @doc """
  Get the right child of the focus node, if any.
  """
  @spec right(Z.t()) :: Z.t() | nil
  def right({nil, breadcrumbs}) when is_list(breadcrumbs), do: {nil, breadcrumbs}

  def right({tree, breadcrumbs}) when is_list(breadcrumbs) do
    new_crumb = %RightCrumb{value: tree.value, tree: tree.left}

    case tree.right do
      nil -> nil
      _ -> {tree.right, [new_crumb | breadcrumbs]}
    end
  end

  @doc """
  Get the parent of the focus node, if any.
  """
  @spec up(Z.t()) :: Z.t()
  def up({tree, []}), do: nil

  def up({tree, [%LeftCrumb{} = crumb | breadcrumbs]}) do
    new_tree = %BT{
      value: crumb.value,
      left: tree,
      right: crumb.tree
    }

    {new_tree, breadcrumbs}
  end

  def up({tree, [%RightCrumb{} = crumb | breadcrumbs]}) do
    new_tree = %BT{
      value: crumb.value,
      left: crumb.tree,
      right: tree
    }

    {new_tree, breadcrumbs}
  end

  @doc """
  Set the value of the focus node.
  """
  @spec set_value(Z.t(), any) :: Z.t()
  def set_value({tree, crumbs}, v) do
    new_tree = %BT{tree | value: v}
    {new_tree, crumbs}
  end

  @doc """
  Replace the left child tree of the focus node.
  """
  @spec set_left(Z.t(), BT.t()) :: Z.t()
  def set_left({tree, crumbs}, l) do
    new_tree = %BT{tree | left: l}
    {new_tree, crumbs}
  end

  @doc """
  Replace the right child tree of the focus node.
  """
  @spec set_right(Z.t(), BT.t()) :: Z.t()
  def set_right({tree, crumbs}, r) do
    new_tree = %BT{tree | right: r}
    {new_tree, crumbs}
  end
end
