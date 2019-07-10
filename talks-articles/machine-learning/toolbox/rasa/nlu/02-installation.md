
## Installation

#### Setting up Rasa NLU

```
pip install rasa_nlu
```

#### Installing pipeline dependencies

* to get started `spaCy + sklearn`, works best upto few thousand set of training data

> spaCy provides a language model for english language, recommend using at least `medium` size models (`_md`) instead of default `small` size models (`en_core_web_sm`)

```
pip install rasa_nlu[spacy]
python -m spacy download en_core_web_md
python -m spacy link en_core_web_md en
```


* alternative `tensorflow`

> to use `tensorflow_embedding` pipeline one would need `tensorflow`, `scikit-learn` and `sklearn-crfsuite` libraries

```
pip install rasa_nlu[tensorflow]
```


* alternative `MITIE`

> performs well for small datasets, may deprecate in future

```
pip install git+https://github.com/mit-nlp/MITIE.git
pip install rasa_nlu[mitie]
```

> after install download [MITIE models](https://github.com/mit-nlp/MITIE/releases/download/v0.4/MITIE-models-v0.2.tar.bz2), and fetch file `total_word_feature_extractor.dat`, below is complete pipeline

```
language: "en"

pipeline:
- name: "nlp_mitie"
  model: "data/total_word_feature_extractor.dat"
- name: "tokenizer_mitie"
- name: "ner_mitie"
- name: "ner_synonyms"
- name: "intent_entity_featurizer_regex"
- name: "intent_classifier_mitie"
```


* alternative `sklearn + MITIE`

> * using fast+good intent of sklearn; good entity recognition and feature vector creation of MITIE
>
> * for more than one digit count intents, this takes too long
>
> example pipeline config is as follows

```
language: "en"

pipeline:
- name: "nlp_mitie"
  model: "data/total_word_feature_extractor.dat"
- name: "tokenizer_mitie"
- name: "ner_mitie"
- name: "ner_synonyms"
- name: "intent_entity_featurizer_regex"
- name: "intent_featurizer_mitie"
- name: "intent_classifier_sklearn"
```

---
