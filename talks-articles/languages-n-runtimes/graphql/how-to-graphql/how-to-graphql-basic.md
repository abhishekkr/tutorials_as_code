
## How To GraphQL: GraphQL Fundamentals

[source](https://www.howtographql.com/)

* a new API standard invented by Facebook to provide a new alternative to REST; a query language for APIs

* enables declarative data fetching

> * where client specifies exactly what data is required from an API
>
> * server only exposes one endpoint and responds precisely data asked for

* FB has been using it since 2012 in their native mobile apps; although it's not just for React

> Netflix has a similar idea open-sourced as Falcor; cancelled when GraphQL open-sourced

* increased mobile usage creates need for efficient data loading; GraphQL minimizes amount of data to be transferred

* each client can get it's own kind of data over same API; supporting multiple clients with faster development

---

### Why better than REST

* Great ideas in REST: stateless servers, structured resource access.

> * the strict spec of REST has wild interpretation
>
> * not enough flexible for rapidly changing client requirements

* GraphQL developed for more flexibility and efficiency in client-server communication.

#### Example of a Blogging App

> need to show user profile, show list of all blogs, show list of all followers of user

* REST might represent as 3 following APIs: `/users/<id>`, `/users/<id>/blogs`, `/users/<id>/followers`.

> * each API will be fetched separately and passed on to view render as objects
>
> * for any new info required on view, a new API will need to be introduced and made available to view

* GraphQL will have only one API and get all required data. Query will look something as following, and response will have same structure in json.

```
query {
  User(id: "<id>") {
    name
    blogs {
      title
    }
    followers(last: 5) {
      name
    }
  }
}
```

* GraphQL avoids unnecessary data fetching from single API in order to re-use. Also skips need to have multiple APIs.

> with finer control on data read by clients, allows evolving API and deprecating API features easily

* GraphQL enables low-level perf monitoring of requests processed. It uses concept of `resolver functions` to collect data requested by client, instrumenting these give better insights about bottleneck.

* GraphQL uses strong type system to define capabilities of an API. All types are exposed by a `schema` in GraphQL Schema Definition Language (`SDL`) serving as an API contract between client and server, so they can work independently.

---

### Core Concepts

* Example of SDL defining 2 simple types, here type ending with `!` means non-nullable fields

```
type Person {
  name: String!
  age: Int!
}

type Blog {
  title: String!
}
```

* Adding a relation between Person and Blog will change them as

```
type Person {
  name: String!
  age: Int!
  blogs: [Blog!]!
}

type Blog {
  title: String!
  author: Person!
}
```

* Since data to be returned on API is decided by client, it has to send more data in request pertaining. It sends `query` info.

```
{ allPersons {
    name
  } }

// might return response like
{ "allPersons": [
  {"name": "Amar"}, {"name": "Akbar"}, {"name": "Anthony"}
]}
```

> * `allPersons` field is root of query
>
> * everything that follows `root` is called `payload`
>

* if client also need `age` all needed is change query to `{allPersons {name age}}`

* to receive say only last 5 records can use query `{allPersons(last: 5) {name age}}`

* to load all posts by the person as well, use query

```
{
  allPersons(last: 5) {
    name
    age
    posts {
      title
    }
  }
}
```

#### Writing data requires mutations

* create/update/delete are main 3 kinds of mutations

* mutation follow same query format, but start with verb of mutation

* graphql constructs get generated with unique id, so just expanding SDL like following let you query them

```
type Person {
  id:   ID!
  name: String!
  age:  Int!
  blogs: [Blog!]!
}

type Blog {
  id:    ID!
  title: String!
}
```

* can even query data while sending mutations, helpful is certain cases

```
mutation {
  createPerson(name: "Bob", age: 100) {
    id
  }
}
```

#### Realtime updates with Subscriptions

* when client subscribes, gets a steady connection for updates

```
subscription {
  newPerson {
    name
    age
  }
}
```

#### Graphql Schema

* root types: `query`, `mutation`, `subscription` defining entry point

* implementing these types in schema from example above

```
## { allPersons { name } }
type Query {
  allPersons(last: Int): [Person!]!
}

## mutation { createPerson(name: "Bob", age: 100) { id } }
type Mutation {
  createPerson(name: String!, age: String!): Person!
}

## subscription { newPerson { name age } }
type Subscription {
  newPerson: Person!
}
```

* currently only options allowed are query for allPersons, mutation for person and subscription for person

* will have to add types for any other capability desired, following might be a useful full schema

```
type Person {
  id:   ID!
  name: String!
  age:  Int!
  blogs: [Blog!]!
}

type Blog {
  id:    ID!
  title: String!
}

type Query {
  allPersons(last: Int): [Person!]!
  allBlogs(last: Int): [Blog!]!
}

type Mutation {
  createPerson(name: String!, age: String!): Person!
  updatePerson(id:ID!, name: String!, age: String!): Person!
  deletePerson(id: ID!): Person!
  createBlog(title: String!): Blog!
  updateBlog(id:ID!, title: String!): Blog!
  deleteBlog(id: ID!): Blog!
}

type Subscription {
  newPerson: Person!
  updatedPerson: Person!
  deletedPerson: Person!
  newBlog: Blog!
  updatedBlog: Blog!
  deletedBlog: Blog!
}
```

---

### Big Picture (Architecture)

* GraphQL is only a spec at [https://facebook.github.io/graphql](https://facebook.github.io/graphql)

#### Common Use Cases

* server with connected db, often for greenfield projects; can use single or multiple DBs based on load and requirement

* to integrate existing system; to build new services over older while hiding their complexity

* hybrid approach with connected db and 3rd party of legacy system, all accessed through same API


#### Resolver functions

* payload of GraphQL queries/mutations consist of set of fields

* GraphQL server has one resolver function per field, each resolver function just retrieves data for its corresponding field

* example query and resolver

```
# query
query {
  User(id: "this-is-an-id") {
    name
      friends(first: 5) {
        name
        favorites
      }
  }
}

# example Resolver
## first resolver to be implemented, return type `User` is nullable since `id` might be missing
User(id: String!): User
## second resolver, dependent on user type and return non-nullable String
name(user: User!): String!
## then resolver for favorites  based on User
favorites(user: User!): [Favorite!]!
## and resolver for friends fiven a User
friends(first: Int, user: User!): [User!]!
```


#### GraphQL clients

* client doesn't care where data is coming from as instructs the `query/mutation` directly with request and knows exactly what structure of data is received

* `declarative data fetching` instead of `imperative data fetching`

* example client libraries [relay](https://facebook.github.io/relay/), [apollo](https://github.com/apollographql/apollo-client)

---
---
