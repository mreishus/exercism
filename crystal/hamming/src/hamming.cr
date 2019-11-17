module Hamming
  def self.distance(s1 : String, s2 : String)
    if s1.size != s2.size
      raise ArgumentError.new()
    end

    (0...s1.size)
      .count { |i| s1.chars[i] != s2.chars[i] }
  end
end
