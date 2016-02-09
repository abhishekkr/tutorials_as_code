# -*- coding: utf-8 -*-
# more practice

print "just some practice"
print "here the following stuff is just some \"flow_in_g\" text"

flow_in_g = """
\tYou go wherever you wanna go
But you'll return to the same place.
'Cuz you feel safe back down there
and no other place leave same trace.
\n\tGonna Go Back.
"""

print "<>"*10
print flow_in_g
print "<>"*10

five = 10 - 2 + 3 - 6
print "five as in %s" % five

def recipe(spice):
    starter = "souped %s" % spice
    meal = "roasted %s" % spice
    dessert = "sweeten %s" % spice
    return starter, meal, dessert

def cook(ingredient):
    starter, meal, dessert = recipe(ingredient)

    print "with just %s" % ingredient
    print "you'll eat %s followed by %s and then %s" % (
            starter, meal, dessert)

ingredient = 'corn'
cook(ingredient)

ingredient = 'baby' + ingredient
cook(ingredient)

