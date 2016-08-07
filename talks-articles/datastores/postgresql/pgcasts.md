
## PG Casts

#### PG Casts Ep. A Better Null Display Character
> by Josh Branchaud, HashRokcet

So a NULL display is different than Blank value display

```
\pset null '^'
```

it will show `caret` sign for null values, can mark it to any char like null-char

---

#### PG Casts Ep. Comments
> by Jake Worth, HashRokcet

Can't rename tables but need more metadata. Can comment on almost anything like tables, rows, schemas.

```
-- ## obtuse table
create table cmbr ( id serial primary key, name varchar );

\dt+ cmbr;

-- ## add comment
comment on table cmbr is 'Cast members of PGCasts';

\dt+ cmbr;

-- ## change comment
comment on table cmbr is 'Cast members of PGCasts';

\dt+ cmbr;

-- ## remove comment
comment on table cmbr is null;

\dt+ cmbr;
```

leave comments to whosoever comes later

---

#### PG Casts Ep. Generate Fake Email Addresses
> by Josh Branchaud, HashRokcet

```
-- ## say we got a sample table
create table users (id serial primary key, email varchar not null unique);

\d users

-- ## let's generate some fake series data
select generate_series(1,10000);

-- ## using it with string concat
select 'person' || num || '@example.com' from generate_series(1,10000) as num;

-- ## now can just insert
insert into users (email) select 'person' || num || '@example.com' from generate_series(1,10000) as num;

-- ## how about some variation
insert into users (email)
select 'person' || num || '@' ||
  (case (random() * 2)::integer
    when 0 then 'gmail'
    when 1 then 'hotmail'
    when 2 then 'yahoo'
  end) || '.com'
from generate_series(1,10000) as num;
```

---

#### PG Casts Ep. Generating JSON from Relational Data
> by Jack Christensen, HashRokcet

let's have a users table for Bookmarking platform, with few users

```
create table users(
      id integer primary key,
      email varchar not null,
      name varchar not null,
      password_digest varchar not null
    );

insert into users values
  (1, 'john@example.com', 'John', '0123456789abcd'),
  (1, 'jane@example.com', 'Jane', '0123456780abce');
```

now pgsql got `row_to_json` foo

```
select row_to_json(users) from users where id=1;
```

this works but returns all columns, now we don't need sensitive columns

asking for specific columns as following loses column name in result json

```
select row_to_json(row(id,name,email)) from users where id=1;
```

can use a subquery to get required columns, then use `row_to_json`

```
select row_to_json(t) from (
  select id,name,email from users where id=1
) t;
```

now say if we have one more table of bookmarks for users

```
create table bookmarks (
      id serial primary key,
      user_id integer not null references users,
      name varchar not null,
      url varchar not null
    );

insert into bookmarks(user_id, name, url) values
  (1, 'hashrocket', 'https://www.hashrocket.com'),
  (1, 'pgsql', 'https://www.postgresql.org'),
  (2, 'youtube', 'https://www.youtube.com');
```

now getting a JSON of users record mapped with their bookmarks

```
select row_to_json(t)
from (
    select id, name, email, (
      select json_agg(row_to_json(bookmarks))
      from bookmarks
      where user_id=users.id
    ) as bookmarks
    from users
    where id=1
) t;
```

here `json_agg` aggregates json object into an array

---

#### PG Casts Ep. Intro to HStore
> by Jake Worth, HashRokcet

first load HStore extension

```
create extension hstore;
```

create hstore column to record data

```
alter table users add column session_data hstore not null default ''::hstore;

\d users;
```

now to enter into hstore, each key must be unique so making it timestamp is ok for now:w


```
update users
  set session_data = session_data || hstore ('4567-pretend-timestamp', '127.0.0.100')
  where id =1;

update users
  set session_data = session_data || hstore ('4767-pretend-timestamp', '127.0.0.101')
  where id =1;
```

can later query result by time or IPs

to delete a key-value pair

```
update users
  set session_data = delete(session_data, '4567-pretend-timestamp')
  where id = 1;:w

```

---

#### PG Casts Ep. Lateral Joins
> by Vidal Ekechukwu, HashRokcet

A developer table with developer details.
A developer activity table with developer's activity mapped.

For each developer want an array of it's activity event type with size no greater than 5.

If you try put limit on following query over activities, it first limits 5 of activities then map developers.

```
select d.id, array_agg(da.event_type) activities
  from developers d
  join (
    select event_type, developer_id from activities
    da on d.id=da.developer_id
  )
  group by d.id
  order by d.id
```

Doing it as required

```
select d.id, to_json(array_agg(da.event_type)) activities
  from developers d
  join lateral (
    select event_type, developer_id from activities
    where developer_id=d.id 
    limit 5
  ) da on d.id=da.developer_id
  group by d.id
  order by d.id
```

---
---
