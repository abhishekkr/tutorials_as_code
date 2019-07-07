#!/usr/bin/env julia
#=
# ToDo
# * Text Processing, remove stopwords and punctuations
# * Tokenize words and sentences
# * Word frequency
# * Senctence score
# * Find the largest score
# * Join sentences
#
# include("lib/sampletext.jl")     ## providing SampleDocument
# tokenize(SampleDocument)        ## tokenize words
# split_sentences(SampleDocument) ## tokenize sentences

julia> include("sample-code/document-summarization.jl")
Main.TextSummarizer

julia> varinfo(TextSummarizer)
name                 size summary
–––––––––––––– –––––––––– –––––––––––––––––––––––––––––––––––––
TextSummarizer 55.644 KiB Module
summarize         0 bytes typeof(Main.TextSummarizer.summarize)
=#

"""
include("lib/document-summarization.jl")

usage: TextSummarizer.summarize(SampleDocument)
Returns summary of SampleDocument.
"""
module TextSummarizer
  export summarize

  using WordTokenizers

  include("lib/stopwords.jl")

  function word_frequencies(docx::AbstractString)
    word_freq = Dict()
    for word in tokenize(docx)
      if word in default_english_stopwords ## from stopwords.jl
        continue
      end
      if ! in(word, keys(word_freq))
        word_freq[word] = 1
      else
        word_freq[word] += 1
      end
    end
    word_freq
  end


  function relative_word_frequencies(docx::AbstractString)
    word_freq = word_frequencies(docx)
    max_freq  = maximum(values(word_freq))
    for word in keys(word_freq)
      word_freq[word] = (word_freq[word]/max_freq)
    end
    word_freq
  end


  function sentence_scores(docx::AbstractString, word_freq::Dict)
    _sentence_scores = Dict()
    words = keys(word_freq)
    for sentence in split_sentences(docx)
      for word in tokenize(lowercase(sentence))
        if ! in(word, words)
          continue
        end
        if length(split(sentence," ")) > 30
          continue
        end
        if in(sentence, keys(_sentence_scores))
          _sentence_scores[sentence] += word_freq[word]
        else
          _sentence_scores[sentence] = word_freq[word]
        end
      end
    end
    _sentence_scores
  end


  """
  summarize(docx::AbstractString)

  usage: summarize(SampleDocument)
  Return a summary of SampleDocument using extractive summary technique.
  """
  function summarize(docx::AbstractString)
    _word_freq = relative_word_frequencies(docx)
    _sentence_scores = sentence_scores(docx, _word_freq)
    ## finding N-largest using sort or datastructures heap nlargest
    summary_sentences = sort(
                            collect(
                                    zip(values(_sentence_scores), keys(_sentence_scores))
                                    ),
                            rev=true)
    summarized_list = [ w[2] for w in summary_sentences ]
    join(summarized_list, " ")
  end

end



