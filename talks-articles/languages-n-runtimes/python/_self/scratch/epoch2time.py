import time
import sys

def epoch_to_human(epoch_time):
    print time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(float(epoch_time)))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Wrong Parameters. Usage: `%s epoch`" % sys.argv[0]
        sys.exit(1)
    arg_epoch_time = sys.argv[1]
    epoch_to_human(arg_epoch_time)
