"""
let's write
  some nonsense long-running process to
    without using sleep
    that can consume more CPU
  and can be made lighter or heavier
    with a counter
"""

import sys
import json
import math
import random


def pow_wow(num, pow):
    return_val = 0
    count = math.pow(num, pow+2)
    while count > 0:
        magic_num = random.randint(1, count)
        if magic_num > int(count/2):
            return_val += 1
        count -= 1
    return return_val


if __name__ == "__main__":
    counter = 5
    if len(sys.argv) > 1:
        counter = int(sys.argv[1])
    results = {}
    for num in range(counter):
        pow = math.ceil(num/2)
        pow_count = pow_wow(num, pow)
        results[str((num, pow))] = pow_count
    results_json = json.dumps(results, indent=4)
    print(results_json)
