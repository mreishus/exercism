module Gigasecond
  def self.from(t : Time)
    t + Time::Span.new(0, 0, 1_000_000_000)
  end
end
