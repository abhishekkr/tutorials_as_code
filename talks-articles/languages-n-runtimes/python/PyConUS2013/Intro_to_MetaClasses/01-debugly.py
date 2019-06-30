## python 3.3 or later

from functools import wraps

def debug(funk):
  msg = funk.__qualname__
  @wrap(funk)
  def wrapper(*args, **kwargs):
    print(func.__name__)
    print(msg)
    return func(*args, **kwargs)
  return wrapper

funk = debug(funk)
