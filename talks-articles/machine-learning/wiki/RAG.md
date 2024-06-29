
## Retrieval Augmented Generation (RAG) for LLMs

> sources: [hopsworks.ai dictionary](https://www.hopsworks.ai/dictionary/retrieval-augmented-generation-llm)

> RAG helps LLMs improve prediction by using external datastore inferences to build richer prompt, including context combination, history, & updated data.

* They can update their retrieval corpora.

* Provide citations, for verification.

* **Vector DBs** & **Feature Stores** can be used for external data.
> * Vector DBs to retrieve relevant records using similarity search.
> * Feature Stores for structured tabular data.

* RAGs work as LLMs can do **in-context learning**.

```
* RAG for LLMs

          [USER]
(prompt :1)| |'(:4 response)
           |,|
    [ Q/A System]-----(:3 query with augmented prompt)--->[LLM]
           '|
            |(:2 Context/Realtime Data Retrieval)
   [External Datastores]     
```

### Why?

* Foundation Models need KB updates, suffer hallucination, might leak private data. RAG help provide context to overcome these.

### RAG vs FineTuning?

* FineTuning further trains the foundation model to improve LLMs perf on specific context.
> * Like Codellama was trained on Llama2 for Code generation.

* RAGs are suited in case of updated/real-time data or private information.

### RAG Challenges?

* Relevance of similarity searched documents.

---
