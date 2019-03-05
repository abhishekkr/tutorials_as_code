#!/usr/bin/env python

"""
Mergsort a list

https://en.wikipedia.org/wiki/Merge_sort
"""

def two_way_merge(array_lhs, array_rhs):
    sorted_array = []
    left_index = right_index = 0
    lhs_size = len(array_lhs)
    rhs_size = len(array_rhs)
    while left_index < lhs_size and right_index < rhs_size:
        if array_lhs[left_index] < array_rhs[right_index]:
            sorted_array.extend([array_lhs[left_index]])
            left_index += 1
        else:
            sorted_array.extend([array_rhs[right_index]])
            right_index += 1
    while left_index < lhs_size:
        sorted_array.extend([array_lhs[left_index]])
        left_index += 1
    while right_index < rhs_size:
        sorted_array.extend([array_rhs[right_index]])
        right_index += 1
    return sorted_array


def merge_sort(an_array):
    begin = 0
    end = len(an_array)
    if (begin+1) == end:
        return an_array
    mid = (begin+end)/2
    sorted_lhs = merge_sort(an_array[begin:mid])
    sorted_rhs = merge_sort(an_array[mid:end])
    return two_way_merge(sorted_lhs, sorted_rhs)


print(merge_sort([1, 4, 3, -10, 11, 3]))
print(merge_sort([1, 4, 13, -10, 11, 3]))
print(merge_sort([1, -4, 13, -10, 11, 3]))
print(merge_sort([21, -4, 3, -10, 1, 3]))
print(merge_sort([1, -24, 13, -10, 11, 3]))
print(merge_sort([1, -4, 23, -10, 11, 3]))
print(merge_sort([-1, 4, 13, -10, 11, 3]))



