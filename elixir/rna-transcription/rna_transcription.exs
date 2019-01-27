defmodule RNATranscription do
  @dna_to_rna %{
      "G" => "C",
      "C" => "G",
      "T" => "A",
      "A" => "U",
  }

  @doc """
  Transcribes a character list representing DNA nucleotides to RNA

  ## Examples

  iex> RNATranscription.to_rna('ACTG')
  'UGAC'
  """
  @spec to_rna([char]) :: [char]
  def to_rna(dna) do
    dna
    |> List.to_string()
    |> String.graphemes()
    |> Enum.map(&transcribe/1)
    |> Enum.join()
    |> String.to_charlist()
  end

  def transcribe(char), do: Map.get(@dna_to_rna, char, char)

  def transcribe('G'), do: 'C'
  def transcribe('C'), do: 'G'
  def transcribe('T'), do: 'A'
  def transcribe('A'), do: 'U'
  def transcribe(x), do: x
end
