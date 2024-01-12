from functools import wraps
import logging
import time


logging.basicConfig(filename='call_latency.log',
                    encoding='utf-8',
                    level=logging.DEBUG)


def call_latency(fn):
    @wraps(fn)
    def call_latency_inner(*args, **kwargs):
        start_time = time.perf_counter()
        retval = fn(*args, **kwargs)
        time_taken = time.perf_counter() - start_time
        logging.debug(f"CallLatency: {fn.__name__} ({args}, {kwargs}), time: {time_taken:.4f} seconds")
        return retval
    return call_latency_inner
