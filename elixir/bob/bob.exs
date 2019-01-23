defmodule Bob do
  @doc """
  hey :: String -> String

  Bob is a lackadaisical teenager. In conversation, his responses are very limited.
  Bob answers 'Sure.' if you ask him a question.
  He answers 'Whoa, chill out!' if you yell at him.
  He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
  He says 'Fine. Be that way!' if you address him without actually saying anything.
  He answers 'Whatever.' to anything else.
  """

  def hey(input) do
    cond do
      empty_or_whitespace?(input) -> "Fine. Be that way!"
      question?(input) && yell?(input) -> "Calm down, I know what I'm doing!"
      question?(input) -> "Sure."
      yell?(input) -> "Whoa, chill out!"
      true -> "Whatever."
    end
  end

  # empty_or_whitespace_alt? :: String -> Bool 
  # Is input only whitespace characters or empty string?
  defp empty_or_whitespace?(input), do: String.trim(input) == ""

  # question? :: String -> Bool 
  # Does input end in a question mark?
  defp question?(input), do: String.ends_with?(input, "?")

  # yell? :: String -> Bool 
  # Does input contain at least one uppercase letter and no lowercase letters?
  defp yell?(input), do: String.upcase(input) == input && String.upcase(input) != String.downcase(input)
end
