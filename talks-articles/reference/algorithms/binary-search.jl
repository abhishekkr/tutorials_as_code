#!/usr/bin/env julia

#=
# data should be sorted and works by divide and conquer
# run-time complexity O(log n)
=#


function binary_search(search_for, data_array, low=1, high=-1)
  if high == -1
    high = length(data_array)
  end
  if high < low
    return -1
  end
  mid = Int(floor(low + (high - low) / 2))
  if data_array[mid] == search_for
    return mid
  end
  if data_array[mid] > search_for
    return binary_search(search_for, data_array, low, mid-1)
  else
    return binary_search(search_for, data_array, mid+1, high)
  end
end

     ## 1   2   3   4   5   6   7   8   9   10
data = [10, 11, 12, 22, 24, 27, 34, 47, 56, 67]

@time println("at index: ", binary_search(47, data))
@time println("at index: ", binary_search(12, data))
@time println("at index: ", binary_search(7, data))
@time println("at index: ", binary_search(67, data))
@time println("at index: ", binary_search(34, data))
