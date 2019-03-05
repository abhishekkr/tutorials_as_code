
## Rasa NLU: Language understanding for chatbots and AI assistants

> doc suits v0.14.4

* Rasa NLU does intent classification and entity extraction.

* A sentence like `"I need to book an appointment for day after tomorrow"` will return structured data like

```
{
  "intent": "book_appointment",
  "entities": {
    "time": "day after tomorrow"
  }
}
```


### Getting Started

* [Try it out](./01-try-it-out.md)

* [Installation](./02-installation.md)

> ToDo

### User Guide

* [Choosing a Rasa NLU Pipeline](#)

* [Language Support](#)

* [Entity Extraction](#)

* [Evaluating and Improving](#)

* [Models](#)

* [Confidence and Fallback](#)

* [Intents](#)

* [FAQs](#)


## API Reference

* [Training Data Format](#)

* [Component Configuration](#)

* [Server Configuration](#)

* [HTTP API](#)

* [Python API](#)

* [Storing Models in the Cloud](#)

* [Endpoint Configuration](#)

* [Running in Docker](#)


## Developer Documentation

* [Custom Components](#)

* [Migration Guide](#)

* [License](#)

* [ChangeLog](#)

* [Getting Support](#)

---
