class StringAnalyzer
  def vowels?(str)
    !!(str =~ /[aeiou]+/i)
  end
end
