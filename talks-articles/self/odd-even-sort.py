#!/usr/bin/env python2


import pprint
import sys


def sort_pairs_base(base, somelist):
    len_somelist = len(somelist)-1
    while base<len_somelist:
        if somelist[base] > somelist[base+1]:
            swap_temp = somelist[base]
            somelist[base] = somelist[base+1]
            somelist[base+1] = swap_temp
        base += 2
    return somelist


def odd_even_sort(somelist, no_change_for):
    list_base_0 = sort_pairs_base(0, list(somelist))
    if somelist == list_base_0:
        no_change_for += 1
        if no_change_for > 1:
            return list_base_0
    else:
        no_change_for = 0

    list_base_1 = sort_pairs_base(1, list(list_base_0))
    if list_base_0 == list_base_1:
        no_change_for += 1
        if no_change_for > 1:
            return list_base_1
    else:
        no_change_for = 0

    return odd_even_sort(list_base_1, no_change_for)



if __name__ == '__main__':
    list_to_sort = sys.argv[1:]
    sorted_list = odd_even_sort(list_to_sort, 0)
    pprint.pprint(sorted_list)
