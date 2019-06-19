
## How To GraphQL: Advanced GraphQL

[source](https://www.howtographql.com/advanced/0-clients/)

### Clients

* directly send queries/mutations without constructing HTTP requests

* view-layer integration; ties well with Functional Reactive Programming; view decalres its data dependencies and wired up with an FRP layer

* caching; results of GraphQL queries can be put into the store and used when exact same query used; [blog](https://dev-blog.apollodata.com/the-concepts-of-graphql-bc68bd819be3)

* validating and optimizing queries based on schema; access to schema can be used for validations and debugging

* can use plain HTTP to fetch data, GraphQL provides ability to abstract manual work with specific clients

---

### Server

* GraphQL describes schema and a query language to use schema.

* GraphQL also describes actual execution algorithm for how those queries are transformed into results. Algo is simple at its core, query is traversed field by field executing `resolvers` for each field.

```
## following schema
type Query {
  author(id: ID!): Author
}
type Author {
  blogs: [Blog]
}
type Blog {
  title: String
  content: String
}

## following is query for blogs by an author
query {
  author(id: "arthur") {
    blogs {
      title
      content
    }
  }
}

## every field in query can be associated with a type
query: Query {
  author(id: "arthur"): Author {
    blogs: [Blog] {
      title: String
      content: String
    }
  }
}
```

* We can find resolvers in our server for every field in above example, starting at query type and traversing `breadth-first`, then assembles result in order and responds.

```
Query.author(root, {id: 'arthur'}, context) -> author
Author.blogs(author, null, context) -> blogs
for each blog in blogs
  Blog.title(blog, null, context) -> title
  Blog.content(blog, null, context) -> content
```

* GraphQL provides `default resolvers`, so not all resolver functions need to be defined.


#### Batched Resolving

* Fetching is made smarter by wrapping all fetching function in a utility to wait for all resolvers to run, making sure to fetch each item once.

```
query {
  posts {
    title
    author {
      name
      avatar
    }
  }
}

## resolver shall work like
authorLoader = new AuthorLoader()
#### Queue up a bunch of fetches
authorLoader.load(1);
authorLoader.load(2);
authorLoader.load(1);
authorLoader.load(2);
#### Then, the loader only does the minimal amount of work
fetch('/authors/1');
fetch('/authors/2');
```

* If API supports batched requests, above can be done in single request as `fetch('/authors?ids=1,2')`.

* [DataLoader](https://github.com/graphql/dataloader) can be used in JS as data-fetching layer.

---

### More GraphQL Concepts

* `Fragments` help improve structure and reusability of GraphQL code. Fragment is a collection of fields on specific type.

```
## assume following type
type User {
  name: String!
  age: Int!
  email: String!
  street: String!
  zipcode: String!
  city: String!
}

## could represent all info relating to user's address into a fragment
fragment addressDetails on User {
  name
  street
  zipcode
  city
}

## while writing a query to access address info of a user,
## use following syntax to refer to fragment and save work
{ allUsers {
  ... addressDetails
  }
}
## is equivalent to
{ allUsers {
    name
    street
    zipcode
    city
  }
}
```

#### Parameterizing Fields with Arguments

* Each field can take 0 or more args, each argument need a `name` and `type`. Default values can also be provided.

```
## part of schema
type Query {
  allUsers: [User!]!
}
type User {
  name: String!
  age: Int!
}

## can add argument to allUsers field, allows us to pass an argument
type Query {
  allUsers(olderThan: Int = -1): [User!]!
}

## can now be passed in query as
{
  allUsers(olderThan: 25) {
    name
    age
  }
}
```


#### Named Query Results with Aliases

* Lets you send multiple queries in single request.

* Since response data is shaped after structure of fields being requested. Creates issues when sending multiple queries asking same field.

```
## multiple queries
{
  User(id: "1") {
    name
  }
  User(id: "2") {
    name
  }
}

## only way to send a query like above is using Aliases for query results, as
{
  first: User(id: "1") {
    name
  }
  second: User(id: "2") {
    name
  }
}

## results would be like
{
  "first": {"name": "Arthur"},
  "second": {"name": "Betty"}
}
```


#### Advanced SDL

* In GraphQL, every type is require to have an `id`.

* Object and Scalar Types

> * Scalar types represent concrete units, 5 predefined as `String`, `Int`, `Float`, `Boolean`, `ID`
>
> * Object types have fields expressing properties of that type and composable. Like `User` and `Blog` defined above.
>
> * `Date` is an example of custom scalar type; implementation needs to be defined as validated, serialized and desrialized.


* Enums

> * Allows to define enumerations like `Weekday`, enums are special kinds of scalar types.

```
enum Weekday {
  MONDAY
  TUESDAY
  WEDNESDAY
  THURSDAY
  FRIDAY
  SATURDAY
  SUNDAY
}
```


* Interface

> * Used to describe type in abstract way. Concrete types implementing these must have fields from it.

```
interface Node {
  id: ID!
}
type User implements Node {
  id: ID!
  name: String!
  age: Int!
}
```


* Union Types

> * Used to express collection of other types.
>
> * Invokes problem of when need to ask for Child, but need to work with Person type. Need to be fixed with `conditional fragment`.

```
type Adult {
  name: String!
  work: String!
}
type Child {
  name: String!
  school: String!
}

## can define Person to be union of Adult and Child
union Person = Adult | Child

## using conditional fragment
{
  allPersons {
    name  ## to work for Adult and Child
      ... on Child {
        school
      }
      ... on Adult {
        work
      }
  }
}
```

---

### Tooling and Ecosystem

* GraphQL's Type System allows us to quickly define surface area of our APIs. It allows to clearly define API capabilities, also validate incoming queries againts a schema.

* GraphQL allows clients to ask server for schema information as well. Called `introspection`.


#### Introspection

* To query schema of a GraphQL API, query `__schema` meta field

```
query {
  __schema {
    types {
      name
    }
  }
}
```

* For example

```
## for schema definition
type Query {
  author(id: ID!): Author
}
type Author {
  name: String!
}

## introspection query would give both object types and scalar types
{
  "data": {
    "__schema": {
      "types": [
        {
          "name": "Query"
        },
        {
          "name": "Author"
        },
        {
          "name": "ID"
        },
        {
          "name": "String"
        },
        {
          "name": "__Schema"
        },
        {
          "name": "__Type"
        },
        {
          "name": "__TypeKind"
        },
        {
          "name": "__Field"
        },
        {
          "name": "__InputValue"
        },
        {
          "name": "__EnumValue"
        },
        {
          "name": "__Directive"
        },
        {
          "name": "__DirectiveLocation"
        }
      ]
    }
  }
}
```

* Introspection types can be introspected for more details, as

```
## here we query single type using '__type' meta-field, asking for its name & description
{
  __type(name: "Author") {
    name
    description
  }
}

## result
{
  "data": {
    "__type": {
      "name": "Author",
      "description": "The author",
    }
  }
}
```

* Specification goes into much detail about what fields and types are available in introspection schema. Allows doc-browsers, autocomplete, code-gen and more is possible.

> Tool like `GraphiQL`


#### GraphQL Playground

* [GraphQL Playground](https://github.com/graphcool/graphql-playground) is powerful IDE for interactively working with GraphQL API. Features editor for queries, mutations and subscriptions.

---

### Security

* Since clients have power to construct complex queries, servers must be able to handle large and edge cases properly.

#### Timeout

* To defend against large queries, no client info required. Though just a blanket cover, small queries can do enough harm.

* Dropped connection might result in strange behaviors. Rollback shall be available for timeouts.


#### Maximum Query Depth

* Since GraphQL schemas are often graphs, means client could craft complex query like

```
query Pwn {                                 # Depth: 0
  author(id: "abc-123") {                   # Depth: 1
    blogs {                                 # Depth: 2
      author {                              # Depth: 3
        blogs {                             # Depth: 4
          author {                          # Depth: 5
            ## could keep on going deeper
          }
        }
      }
    }
  }
}
```

* Knowing your schema can give an estimated legitimate query depth to be allowed, set Maximum Query Depth.

* Analyzing query document's abstract syntax tree (AST), GraphQL server can reject depth exceeding queries. AST is analyzed statically, before executing query.

* Using `graphql-ruby` with max query depth setting of `3`, can get following error

```
{
  "errors": [
    {
      "message": "Query has depth of 6, which exceeds max depth of 3"
    }
  ]
}
```


#### Query Complexity

* Certain fields in schema can be more complex to compute than others. Query complexity allows you to define how complex each field is, and restrict queries with maximum complexity.

* Commonly each field is given a complexity of 1. If maximum complexity is 2, following query will fail.

```
## complexity: 3
query {
  author(id: "abc-123") {   ## complexity: 1
    blogs {                 ## complexity: 1
      title                 ## complexity: 1
    }
  }
}

## complexity: 7
query {
  author(id: "abc-123") {   ## complexity: 1
    blogs(first: 5) {       ## complexity: 5
      title                 ## complexity: 1
    }
  }
}
```

* Statically analyzed, so no execution required.

* Hard to implement perfectly. If complexity is estimated, need to keep updated. Mutations are hard to estimate. Side-effects are hard to measure.


#### Throttling

* To stop clients making too many medium sized queries, so everyone gets equal opportunity.

* Server time required for a query is a good heuristic to throttle queries. Can set a maximum server time a client can use in time window. Server time can be added to client over time.

* Throttling based on query complexity. Same principles as for server time.


#### Summary

* None is bullet proof, a good combo might do the trick.

---

### Common Questions

* GraphQL is a query language for APIs, not DB. It's DB agnostic, can work even without DB.

* Popular GraphQL server code is available for JS, Ruby, Python, Scala, Java, Clojure, Go and dotNET.

* It's transport agnostic, can be implemented on protocols other than HTTP as well.

* Caching is difficult as clients next request is not certain. Server-side caching is a challenge. [Details](https://graphql.org/learn/caching/).

* Authentication can be implemented traditionally, like OAuth.

* For Authorization, [recommended](http://graphql.org/learn/authorization/) to delegate any data access logic to business logic layer instead of handling at GraphQL implementation. [Graphcool's permission rules](https://www.graph.cool/docs/reference/auth/overview-ohs4aek0pe) as reference.

* Successful GraphQL query is supposed to return JSON with `data` as root field for map of result. If request fails (completely or partially), second root field `errors` gets added to response which has list.

* GraphQL client like Relay and Apollo can provide client-side caching for offline caching. [Interesting approach](http://www.east5th.co/blog/2017/07/24/offline-graphql-queries-with-redux-offline-and-apollo/).

---
---
