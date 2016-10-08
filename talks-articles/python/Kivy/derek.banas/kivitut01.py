import kivy

kivy.require('1.9.0')

from kivy.app import App
from kivy.uix.button import Label

"""
class name string preceding `App` is important
as in this example, data for UI can be pulled from `hellokivyy.kv`
name of this '.kv' file is preceding this calss file, gets autoloaded
"""
class HelloKivyyApp(App):
    def build(self):
        return Label()

helloKivy = HelloKivyyApp()
helloKivy.run()
