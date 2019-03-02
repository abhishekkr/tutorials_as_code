
### Try it out

> to run code here install Rasa Core and NLU

```
pip install rasa sklearn_crfsuite prompt_toolkit
```

Here will be build a chatbot asking and sending fun pic to cheer you up.

#### Teaching bots to understand user inputs using Rasa NLU

Start by teaching assistant to understand messages

* train NLU model with plaintext inputs to extract structured data

* the structured data is called `intent`

* first need to define use messages as in [nlu-messages.md](./nlu-messages.md)

* NLU model configuration defines how NLU model will be trained and features from text input will be extracted, in our example a predfined `tensorflow_embedding` pipeline as in [nlu-config.yml](./nlu-config.yml)

* train the NLU model using command below, it will call `rasa.nlu` and save model inside `models/current/nlu`

```
python -m rasa_nlu.train -c nlu-config.yml --data nlu-messages.md -o models --fixed_model_name nlu --project current --verbose
```

* test the model to check if bot can understand, [check-nlu.py](./check-nlu.py)


#### Teaching the bot to respond using Rasa Core

**Define Stories**

* teach your chatbot to respond using Rasa core, it will train dialogue management model and predict how bot should respond at any state

* learns from real conversational data in form of training `stories`

> * `story` is real conversation between user and bot, with user inputs expressed as `intent` and response of bot as `action` names

* sample simple story

> * starts with `##` followed by a name
>
> * lines starting with `*` are messages from user, not the actual message but the intent that represents what user means
>
> * lines with `-` at start are actions by bot, sent to user
>
> * in general an action can do anything, like calling an API
>
> * example stories at [eg-stories.md](./eg-stories.md)

```
## story1
* greet
  - utter_greet
```


**Define Domain**

* Domain defines vocab of bot

> the user inputs expected, what actions to predict, how to respond and what info to store
>
> example domain at [eg-domain.yml](eg-domain.yml)

* `intent` are things you expect user to say

* `actions` are what bot can do and say, simple actions start with `utter_` demanding a response from template; [custom actions](https://rasa.com/docs/core/customactions/) for advanced flows

* `templates` are baseline for things bot can say


**Training dialogue model**

* to train a neural net on example stories, run below command generating trained model at `models/dialogue`

```
python -m rasa_core.train -d eg-domain.yml -s eg-stories.md -o models/dialogue
```


**Talk to your Bot**

* to start full bot with Rasa Core and Rasa NLU models using [eg-bot.py](./eg-bot.py)

> updated the chatbot pythonn code to run at cli as REPL instead of in Notebook

---
