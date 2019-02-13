defmodule Markdown do


  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  # r: pipeline, map shorthand, map_join
  def parse(m) do
    m
    |> String.split("\n")
    |> Enum.map_join("", &process/1)
    |> patch
  end

  # r: rename args, if -> cond, extract helper functions
  defp process(line) do
    cond do
      begin_hash?(line) -> process_header(line)
      begin_star?(line) -> process_list(line)
      true -> process_text(line)
    end
  end

  defp begin_hash?(line), do: String.starts_with?(line, "#")
  defp begin_star?(line), do: String.starts_with?(line, "*")

  defp process_header(line), do: line |> parse_header |> surround_h
  defp process_list(line), do: line |> parse_list
  defp process_text(line), do: line |> String.split |> surround_p

  # r: rename args, add inline definitions, rename function
  defp parse_header(line) do
    [hashes | text_list] = String.split(line)

    text = text_list |> Enum.join(" ")
    level = hashes |> String.length |> to_string

    {level, text}
  end

  # r: pipeline, combine string, rename args, rename function
  defp parse_list(line) do
    t = line
      |> String.trim_leading("* ")
      |> String.split()
      |> join_words_with_tags
    "<li>#{t}</li>"
  end

  # r: combine string, oneline, rename func and args
  defp surround_h({level, txt}), do: "<h#{level}>#{txt}</h#{level}>"

  # r: oneline, rename
  defp surround_p(t), do: "<p>#{join_words_with_tags(t)}</p>"

  # r: pipeline, map shorthand
  defp join_words_with_tags(t) do
    t
    |> Enum.map(&replace_md_with_tag/1)
    |> Enum.join(" ")
  end

  # r: oneline, pipeline
  defp replace_md_with_tag(w), do: w |> replace_prefix_md |> replace_suffix_md

  # r: simplify regex
  defp replace_prefix_md(w) do
    cond do
      w =~ ~r/^__/ -> String.replace(w, ~r/^__/, "<strong>", global: false)
      w =~ ~r/^_[^_+]/ -> String.replace(w, ~r/_/, "<em>", global: false)
      true -> w
    end
  end

  # r: simplify regex
  defp replace_suffix_md(w) do
    cond do
      w =~ ~r/__$/ -> String.replace(w, ~r/__$/, "</strong>")
      w =~ ~r/[^_]/ -> String.replace(w, ~r/_/, "</em>")
      true -> w
    end
  end

  # r: pipeline, combine strings
  defp patch(l) do
    l
    |> String.replace("<li>", "<ul><li>", global: false)
    |> String.replace_suffix("</li>", "</li></ul>")
  end
end
