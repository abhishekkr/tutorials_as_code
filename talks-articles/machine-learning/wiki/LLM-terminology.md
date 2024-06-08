
## LLMs Terminology

> [LLM's Fundamentals](./LLM-fundamentals.md)

The usage of several terms that convey specific meaning for the Trained ML Models; and many are using those terms as a Keyword Metric to compare with unclear interpretations on what meaning they actually convey.

Let's start with some basic terms, and proceed to level up..

### Tokens

* Are basic processable units of data that can be processed in Models. For text, a word/subwords/characters can be tokens.

### Vectors

* Are a way to represent Tokens in a numerical representation; so it can be used as input for Mathematical Algorithms used to train ML Models.

### Embedding

* Are high-dimensinal vectors that capture semantic relation among all tokens of vectors. Semantic relation primarily reflects the closeness among tokens.

> * So converting several text into embeddings help NLP tasks.
> * Embeddings are foundation to LLMs.

### Vector Databases

* Are required for LLMs, as when using Millions of Embeddings.. to efficiently have identify closely related embeddings.

> * These abstract out storage, indexing and fast similarity retrieval of Embeddings as a focused software implementation.
> * Some example are Chroma, Weviate, Pinecone, Postgres with PgVector, etc.

### Transformer

* It's a Deep Learning Architecture, that learns context & meaning by tracking relationship in sequential data.

* It utilizes self-supervised training in an encoder-decoder configuration.

* The Transformer model is a neural network that is particularly effective for NLP tasks.

* Other than LLMs; it has also been successfully used in models like Whisper which does Automatic Speech Recognition thus Transcription. With Image Transformer to generate Images; as with Diffusion Transformer in Stable Diffusion.

### Parameters

* Weights & Coefficients that ML Algorithm gets from training data are Model Parameters.

* Parameters get adjusted during training phase.

* Are factors that Model learns from training data; then utilize to make predictions. More the parameters trained on more data, generally better the model's ability to generate required output.

> Generally the sizes or parameter count of 8B, 70B, 400B like huge counts shown with LLMs to showcase their quality refer to these Model Parameters; that have gone through training data to understand the data.

### Hyper-Parameters

* Are essential components of a Model, dependent on ML Algorithm in learning phase. But not changed by Training Data & don't become part of resulting model.

* These control Model's shape or behavior. Like LLM Temperature, regulating randomness of AI's responses with lower values making response more deterministic. There is Model Size, managing weights & layers count to achieve an optimal size network.

### Hallucination

* In AI, a hallucination is a confident response by an AI that does not seem to be justified by its training data.

> * Happens when an answer wasn't available in Training Data.. but Model erroneously finds similar data to generate.
> * Can also happen if training data somehow had unverified untrue text in it.

### GPT (Generative Pre-Trained Models)

* GPT-3, GPT-4 are LLMs developed by OpenAI.

* Are kind of NLP models trained to map & generate human language like text.

* These are pre-trained on massive general datasets available; learning structure, grammar & context of languages.

* Trained on Transformer model allows handling data sequence efficiently.

### LLAMA

* is a series of LLMs trained by Meta AI; available publically for local usage & research.

* It can perform text-based tasks as text generation, paraphrasing, summarization, etc.

* Llama3 is trained in data from 30+ languages; thus supporting multi-lingual tasks as well.

### Fine-Tuning

* Allows LLMs to be trained to focus specialization on specific capabilities and/or context domains.

> * Capabilities like translation, completion, query-response, etc.
> * Context Domains like Code LLMs; e.g. Code-LLAMA a fine-tuned model based on LLAMA2.

### RAG

* Or Retrieval Augmented Generation is an application architecture designed to power LLMs to answer questions about data unavailable in the training set.

> * Primarily helpful to allow LLMs answer using latest information without constant retraining. Bridges gap in LLMs and external knowledge sources.
> * Also.. With this even an LLM like Llama trained on Public Data, is capable of answering questions about your personal finances.

* External knowledge is stored in Vector Database as embeddings for LLMs to grasp.

* It's not a perfect alternative to avoid re-training on newer data though.

### Prompt Engineering

* Involves designing prompts for the model to generate high-quality output.

* Prompts are input patterns used to guide LLMs behavior, for output to be relevant to a specificity.

> * Appropriate keywords and phrases,
> * Specifying the desired output structure, and
> * Optimizing for performance and efficiency.

### Zero-Shot Prompting

* is to prompt for a task without specific training of context specific tasks.

* Here LLMs rely on it's generalized understanding of Task to give a response.

### One-shot Prompting

* here like Zero-shot the model isn't aware of context beforehand; but is provided with one labeled example before attempting the task.

### Few-shot Prompting, aka in-context learning

* here like One-shot Prompting, when attempting a task with having access to more than one guiding examples.

### Chain-of-Thought Prompting

* enables complex reasoning by providing intermediary reasoning steps to a final task.

* This can be combined with Few-Shot Prompting to get better results on reason based context specific problems.

### Context Window

* is the number of tokens that are considered when predicting the next token.

### Modality

* is a high-level data category for Model. For example, text, images, video, and audio.

* LLMs deal in anything dealing with Texts.

### MultiModal AI

* is via Model that can process information from multiple models as Text, Image, Audio & Video.

* Usecases are like providing an input of a Tourist Spot and asking to generate a list of good spots nearby.


---
