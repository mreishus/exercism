module Hamming
  def self.distance(s1 : String, s2 : String)
    if s1.size != s2.size
      raise ArgumentError.new()
    end

    0.upto(s1.size - 1)
      .select { |i| s1.chars[i] != s2.chars[i] }
      .size
  end
end
