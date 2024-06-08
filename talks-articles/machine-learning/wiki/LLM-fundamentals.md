
## LLM Fundamentals

LLMs have become a popular term since the advent of services like ChatGPT.

With advancement in Generative AI, many big companies have been spending resources in building there own LLMs.

## What are LLMs

Large Language Models are trained AI models using Deep Learning to understand the synergy & context of characters working together to form legible text.

LLMs are a kind of Foundation Models.

Foundation Models are Deep Learning NNs which are more general purpose, and which can be used to develop ML models further to be specialized in a cost-effective way instead of training a new model from scratch.

They are focused to recognize & generate primarily text. And code being a kind of text there are some models specialized to recognize & generate code as well. These models can be tuned to any specialized form, language or usage context of texts when being trained on relevant training text.

OpenAI's GPT-3, Facebook's Meta, Google's Gemini, Minstral are few of examples that can be used to try the LLM capabilities.

There are tools like OLLAMA that can be used to easily try them on local machines, without need of any Internet based paid API.

## How LLMs Work

Currently, LLMs popularly use Transformer architecture that is based on a paper Attention Is All You Need. LLMs use unsupervised training with Transformer providing self-learning.

Initially sequence transduction models used for this purpose were like RNNs/CNNs in Encoder-Decoder configuration connected with Attention mechanism. With new research the RNNs/CNNs have been disposed of, using Self-Attention alone with Encode-Decoder. This has reduced the training time by lot.

LLMs gain the capability to predict next word to the input they receive, utilizing patterns they have gained using training parameters on continuation of enormous amount of data tokens it has been provided.

Since it's biased with all these patterns, it's still vulnerable to "hallucinations".. that is it generating untrue responses because the continuation of text fits the parameters.

## Some Popular Use-cases

* General text generation aiding in finding answers to queries from huge chunk of data or entire history of things.

* Content Summarization; as it's able to gather context of available text to extract crux of it.

* AI Assitants with the capability to map input queries to relevant responses.

* Code Generation; as code is just text with special grammar.

and more...

---
