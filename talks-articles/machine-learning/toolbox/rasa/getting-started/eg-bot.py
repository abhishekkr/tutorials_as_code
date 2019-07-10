from prompt_toolkit import prompt
from prompt_toolkit.history import FileHistory
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
#from prompt_toolkit.contrib.completers import WordCompleter

from rasa_core.agent import Agent
from rasa_core.interpreter import RasaNLUInterpreter
import time

interpreter = RasaNLUInterpreter('models/current/nlu')

agent = Agent.load('models/dialogue', interpreter=interpreter)


print("Hi! you can chat in this REPL. Type 'stop' to end the conversation.\n")
while True:
    user_input = prompt('_ ', 
                        history=FileHistory('/tmp/bot-history'),
                        auto_suggest=AutoSuggestFromHistory())

    if user_input == 'stop':
        break
    responses = agent.handle_message(user_input)
    for r in responses:
        print(r.get('text'))
