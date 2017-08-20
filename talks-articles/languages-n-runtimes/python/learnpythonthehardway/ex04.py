# -*- coding: utf-8 -*-
# variables and names

cars = 100
space_in_a_car = 4.0
drivers = 30
passengers = 90
cars_not_driven = cars - drivers
cars_driven = drivers
carpool_capacity = cars_driven * space_in_a_car
avg_passengers_per_car = passengers / cars_driven

print "There are", cars, "cars available."
print "Only", drivers, "drivers, so", cars_not_driven, "cars aren't driven."
print carpool_capacity, "people can travel."
print "Today,", passengers, 'need to travel.'
print "So,", avg_passengers_per_car, "need to travel per car."
