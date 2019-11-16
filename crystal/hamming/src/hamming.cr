# Please implement your solution to hamming in this file
module Hamming
  def self.distance(s1 : String, s2 : String)
    if s1.size != s2.size
      raise ArgumentError.new()
    end
    distance = 0
    s1.chars.each_with_index do |c1, i|
      c2 = s2.chars[i]
      if c1 != c2
        distance += 1
      end
    end
    distance
  end
end

puts Hamming.distance("xxxx", "yyyy")
