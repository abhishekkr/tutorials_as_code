### super() Literal

class Alive(object):
    def __init__(self, size):
        print('I am a %s Lol Live' % size)


class Animal(Alive):
    def __init__(self, size, **kwargs):
        super(Animal, self).__init__(size)
        self.size = size

    def speak(self):
        raise NotImplementedError

    def eat(self):
        print('yumummyumamon')


class Dog(Animal):
    def __init__(self, color, **kwargs):
        super(Dog, self).__init__(kwargs['size'])
        self.color = color

    def speak(self):
        print('bark bark bark baaaark brk')


##### __mro__ : Method Resolution Order
print(Dog.__mro__)

d = Dog(color="black", size="big")
d.speak()
d.eat()
