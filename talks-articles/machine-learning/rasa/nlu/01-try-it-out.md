
## Getting Started with Rasa NLU

* let's assume any conversation can be categorized into intents `greet`, `book_appointment` and `thankyou`

* say our users might `greet` our bot with

```
hi
hey there
hello again
```

* ways to ask for an appointment

```
I need to book an appointment
Need an appointment for
Can I book an appointment
Appointment please
```

* first for Rasa NLU is to assign any sentence under relevant `intent` categories

* then label words for `timing` entities


### Prepare NLU Training Data

* It's a list of messages expected to be received, annotated with `intent` and `entities` for Rasa NLU to extract.

* Sample training data for this is at [nlu.md](./01-try-it-out/nlu.md).


### Define Machine Learning Model

* Rasa NLU has different components together making a pipeline.

* Can define a pipeline by defining in a markdown file.

* Here we'll use pre-defined `tensorflow_embedding` pipeline defined in [nlu_config.yml](./01-try-it-out/nlu_config.yml)


### Train ML NLU Model

* Use `rasa_nlu.train` command to train the model using command below, or run [train.sh](./01-try-it-out/train.sh)

```
python -m rasa_nlu.train -c nlu_config.yml --data nlu.md -o models --fixed_model_name nlu --project current --verbose
```

* Above `--project current` and `--fixed_model_name nlu` ensures trained model to be saved at `./models/current/nlu`.


### Try it out

* can run Rasa NLU HTTP server, or as a python script like [try.py](./01-try-it-out/try.py)

---
