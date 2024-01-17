from functools import wraps
import logging
import time


logging.basicConfig(filename='call_retry.log',
                    encoding='utf-8',
                    level=logging.WARNING)


def retry(delay, max=5, xbackoff=1, fail=True):
    def retry_inner(fn):
        fn.__retry_cfg__ = {
            'delay': delay,
            'max': max,
            'xbackoff': xbackoff,
            'fail': fail,
        }

        @wraps(fn)
        def retry_core(*args, **kwargs):
            delay = fn.__retry_cfg__['delay']
            max = fn.__retry_cfg__['max']
            xbackoff = fn.__retry_cfg__['xbackoff']
            fail = fn.__retry_cfg__['fail']
            logging.warn(f"{fn.__name__} queued for Retry: {fn.__retry_cfg__}")
            exception_placeholder = None
            counter = 1
            while counter <= max or max == -1:
                try:
                    retval = fn(*args, **kwargs)
                    logging.warn(f"{fn.__name__} succeeded at retry( {counter}).")
                    return retval
                except Exception as e:
                    logging.warn(f"{fn.__name__} failed at retry( {counter}).")
                    counter += 1
                    exception_placeholder = e
                time.sleep(delay)
                delay *= xbackoff
            if fail is True:
                raise exception_placeholder
        return retry_core
    return retry_inner
