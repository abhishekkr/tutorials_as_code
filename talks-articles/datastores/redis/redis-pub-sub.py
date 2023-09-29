import redis
import sys


def publish(redcli, channel):
    redcli.publish(channel, "this")
    redcli.publish(channel, "is")
    redcli.publish(channel, "a")
    redcli.publish(channel, "stream")
    redcli.publish(channel, ".")
    print('done.')


def subscribe(redcli, channel):
    sub = redcli.pubsub()
    sub.subscribe(channel)
    for message in sub.listen():
        if message is not None and isinstance(message, dict):
            msg = message.get('data')
            redcli.incr(f"counter_recvd_{channel}")
            print(msg)
            if msg == '.':
                break
    print(redcli.get(f"counter_recvd_{channel}"))


if __name__ == "__main__":
    REDIS_MODE = 'pub'
    REDIS_HOST = 'localhost'
    REDIS_PORT = 6379
    channel = "test-redis"
    if len(sys.argv) >= 2:
        REDIS_MODE = sys.argv[1]
    if len(sys.argv) >= 3:
        REDIS_HOST = sys.argv[2]
    if len(sys.argv) >= 4:
        REDIS_PORT = sys.argv[3]
    redcli = redis.StrictRedis(
                REDIS_HOST,
                REDIS_PORT,
                charset="utf-8",
                decode_responses=True
    )
    if REDIS_MODE == "pub":
        print("publishing:")
        publish(redcli, channel)
    elif REDIS_MODE == "sub":
        print("subscribing:")
        subscribe(redcli, channel)
