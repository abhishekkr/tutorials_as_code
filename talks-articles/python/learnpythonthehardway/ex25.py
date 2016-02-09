# -*- coding: utf-8 -*-
# even more practice

def break_words(stuff):
    return stuff.split(' ')

def sort_words(words):
    return sorted(words)

def first_word(words):
    return words.pop(0)

def last_word(words):
    return words.pop(-1)

def sort_sentence(sentence):
    return sort_words(break_words(sentence))

def first_and_last(sentence):
    words = break_words(sentence)
    first_word(words)
    last_word(words)

def first_and_last_sorted(sentence):
    words = sort_sentence(sentence)
    first_word(words)
    last_word(words)

if __name__ == '__main__':
    stuff = open('README.md').read()
    words = break_words(stuff)
    print sort_words(words)
    print first_word(words)
    print last_word(words)
    print sort_sentence(stuff)
    print first_and_last(stuff)
    print first_and_last_sorted(stuff)

