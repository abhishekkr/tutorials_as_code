## user-defined types
## objects

var id = 0

type
  User = object
    id: int
    username: string
    name: string
  UserRef = ref User

proc setId(user: UserRef) =
  id.inc
  user.id = id

proc echoUser(user: UserRef) =
  echo("id: ", user.id, ", username: ", user.username, ", name: ", user.name)

id.inc
var john: User
john.id = id
john.username = "john"
echo(john)

var jane = UserRef(username: "jane", name: "Jane Doe")
setId(jane)
echoUser(jane)   # passing ref to echo failed

# type
#   People = object
#     name: string
#   Pets = object
#     name: string
# 
# let person: People = People(name: "Tom")
# let dog: Pets = Pets(name: "Tom")
# assert not ( person == dog )  ## compile error for mismatch


## tuples
type
  People = tuple
    snack: string
  Pets = tuple
    snack: string

let person: People = (snack: "Biscuit")
let dog: Pets = (snack: "Biscuit")
assert person == dog

type
  Point = (int, int, int)
let pos: Point = (x: 0, y: 0, z: 0)
let (x, _, _) = pos
assert x == pos[0]


## enums
type
  Weekday = enum
    sunday,
    monday,
    tuesday,
    wednesday,
    thursday,
    friday,
    saturday

let leave: Weekday = sunday
echo(leave)

# pragma pure makes mandatory to prefix each enum values with name of enum
type
  ReturnVal {.pure.} = enum
    One, Two, Three

let retVal = ReturnVal.Two
echo(retVal)
