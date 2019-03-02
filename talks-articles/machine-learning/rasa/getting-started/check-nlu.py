#!/usr/bin/env python

from rasa_nlu.model import Metadata, Interpreter
import json

def pprint(o):
 # small helper to make dict dumps a bit prettier
    print(json.dumps(o, indent=2))

interpreter = Interpreter.load('./models/current/nlu')
pprint(interpreter.parse(u"Hello"))
pprint(interpreter.parse(u"Heya"))
pprint(interpreter.parse(u"How are you"))
pprint(interpreter.parse(u"Damn"))
