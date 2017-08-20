import random
import string


UPPAERCASE=string.ascii_uppercase
LOWERCASE=string.ascii_lowercase
DIGITS=string.digits


def dice(number_of_chars=5, allowed_chars=UPPAERCASE+LOWERCASE+DIGITS):
    _dice_ = ""
    for _ in range(number_of_chars):
        _dice_ = _dice_ + random.SystemRandom().choice(allowed_chars)
    return _dice_


def diceOf(dice_count):
    _dices_ = []
    for _ in range(dice_count):
        _dices_.append(dice())
    return "_".join(_dices_)


def dice_main(flags):
    if len(flags) == 1:
        print("Usage: %s <password-complexity-count-1,2,3,4...>" % (flags[0]))
        return
    try:
        print(diceOf(int(flags[1])))
    except Exception as err:
        print(err.message)


if __name__ == "__main__":
    import sys
    dice_main(sys.argv)
