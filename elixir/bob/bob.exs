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
      is_empty_or_whitespace(input) -> "Fine. Be that way!"
      is_question(input) && is_yell(input) -> "Calm down, I know what I'm doing!"
      is_question(input) -> "Sure."
      is_yell(input) -> "Whoa, chill out!"
      true -> "Whatever."
    end
  end

  @doc """
  is_empty_or_whitespace :: String -> Bool 
  Is input only whitespace characters or empty string?
  """
  def is_empty_or_whitespace(input), do: !Regex.match?(~r/\S/, input)

  @doc """
  is_question :: String -> Bool 
  Does input end in a question mark?
  """
  def is_question(input), do: Regex.match?(~r/\?$/, input)

  @doc """
  is_yell :: String -> Bool 
  Does input contain at least one uppercase letter and no lowercase letters?
  """
  def is_yell(input), do: String.upcase(input) == input && Regex.match?(~r/\p{Lu}/u, input)
end
