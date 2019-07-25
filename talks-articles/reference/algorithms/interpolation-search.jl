#!/usr/bin/env julia

#=
# data should be sorted and works by divide and conquer
# improved variant of binary search for equally distributed data
# it tries probing the position
#
# Interpolation search finds particular item by computing probe position.
# Start from middle of list. If not a match, divide list using probe position and find new middle.
# Do binary search in 2 blocks.
# Repeat
#
# run-time complexity is O(log log n)
=#

function interpolation_search(search_for, data_array, low=1, high=-1)
  if high == -1
    high = length(data_array)
  end
  if high < low
    return -1
  end

  ## calculation of probe position
  mid = Int(floor(
          (
           low +
           ((high - low) / (data_array[high] - data_array[low])) *
           (search_for - data_array[low])
          )
        ))
  if mid < low
    return -1
  end

  if data_array[mid] == search_for
    return mid
  end

  if data_array[mid] < search_for
    return interpolation_search(search_for, data_array, mid+1, high)
  else
    return interpolation_search(search_for, data_array, low, mid-1)
  end

  -1
end

     ## 1   2   3   4   5   6   7   8   9   10
data = [10, 11, 12, 22, 24, 27, 34, 47, 56, 67]

@time println("at index: ", interpolation_search(47, data))
@time println("at index: ", interpolation_search(12, data))
@time println("at index: ", interpolation_search(7, data))
@time println("at index: ", interpolation_search(67, data))
@time println("at index: ", interpolation_search(34, data))
