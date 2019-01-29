defmodule RNATranscription do
  @dna_to_rna %{
      ?G => ?C,
      ?C => ?G,
      ?T => ?A,
      ?A => ?U,
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
    |> Enum.map(&transcribe/1)
  end

  def transcribe(char), do: Map.get(@dna_to_rna, char, char)
end
