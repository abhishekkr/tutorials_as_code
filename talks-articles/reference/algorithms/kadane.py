#!/usr/bin/env python

"""
To find sub-array with a property from an array

https://en.wikipedia.org/wiki/Maximum_subarray_problem#Kadane's_algorithm
"""

def max_kadane(a, b_array):
    new_sum = a + sum(b_array)
    if a > new_sum:
        return [a] 
    return b_array + [a]

def check_sum_kadane(max_current, max_global):
    return (sum(max_current) > sum(max_global))


def find_max_sum_subarray(fn, fn_check, an_array):
    current_k = global_k = [an_array[0]]
    for element in an_array[1:]:
        current_k = fn(element, current_k)
        if fn_check(current_k, global_k):
            global_k = current_k
    return global_k

print(find_max_sum_subarray(max_kadane, check_sum_kadane, [1, 4, 3, -10, 11, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [1, 4, 13, -10, 11, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [1, -4, 13, -10, 11, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [21, -4, 3, -10, 1, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [1, -24, 13, -10, 11, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [1, -4, 23, -10, 11, 3]))
print(find_max_sum_subarray(max_kadane, check_sum_kadane, [-1, 4, 13, -10, 11, 3]))



