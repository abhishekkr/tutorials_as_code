#!/usr/bin/env python

from rasa_nlu.model import Interpreter
import json


interpreter = Interpreter.load("./models/current/nlu")


def print_intent(msg):
    result = interpreter.parse(msg)
    print(json.dumps(result, indent=2))


print_intent("Can you book me a dental checkup appointment for 10'o clock")
print_intent("I need an appointment for eye checkup at 10'o clock")
