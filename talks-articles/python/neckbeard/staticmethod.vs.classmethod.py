#!/usr/bin/env python2

"""
Don't use unless utterly needed by Architecture of library, you are not writing JAVA.
"""

class Item(object):
    rock = False
    paper = True
    scissor = False

    def __init__(self, number):
        self.number = number

    @staticmethod
    def some_static_method():
        """
        Has no info on class/instance called upon.
        """
        print "just static"

    @classmethod
    def some_class_method(cls):
        """
        aware of class attributes, can instantiate new class objects
        """
        print cls.paper, cls

i = Item(10)
i.some_static_method()
i.some_class_method()
Item.some_static_method()
Item.some_class_method()

"""
classmethod usage in subclasses scenario
"""

class Animal(object):
    color = "nocolor"

    def __init__(self, color):
        color = color
        self.color = color

    @classmethod
    def baby(cls):
        return cls(cls.color)

    @staticmethod
    def speak():
        return "diggity"

class Dog(Animal):
    @staticmethod
    def speak():
        return "bowow"

    @classmethod
    def baby(cls):
        print "new Dog", cls.color
        return cls(cls.color)
        #return super(Dog, cls).baby()

a01 = Animal("white")
a02 = Animal.baby()
print a01.color, a01.speak(), a02.color, a02.speak()

dog = Dog("brown")
pup = Dog.baby()
print dog.color, dog.speak(), pup.color, pup.speak()
