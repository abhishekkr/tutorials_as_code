### Lambda

def add1(a,b):
    return a + b

add2 = lambda x, y: x + y

def add3(p,q,func):
    return func(p,q)

print(add1(1,2))
print(add2(1,2))
print(add3(1,2, add2))
print(add3(1,2, lambda x, y: x + y))

alwaysTrue = lambda: 10 if True else 15
print(alwaysTrue())
