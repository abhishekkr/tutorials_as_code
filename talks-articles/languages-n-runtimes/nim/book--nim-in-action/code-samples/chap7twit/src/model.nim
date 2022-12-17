import times


type
  User* = object
    id*: string
    username*: string
    createdAt*: Time
  Message* = object
    id*: string
    msg*: string
    userID*: string
    createdAt*: Time
  Subscription* = object
    userID*: string
    subscribedTo*: string
    createdAt*: Time
