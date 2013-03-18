#!ruby
#1.9

votes = { A: %w[ABC PQR XYZ],
          B: %w[PQR ABC XYZ],
          C: %w[PQR XYZ ABC],
          D: %w[XYZ ABC PQR],
          E: %w[XYZ PQR ABC] }

tally = Hash.new(0)

votes.values.each do |personal_selections|
  personal_selections.each_with_object(tally).with_index do |(vote, totals), i|
    totals[vote] += personal_selections.size - i
  end
end

p tally
