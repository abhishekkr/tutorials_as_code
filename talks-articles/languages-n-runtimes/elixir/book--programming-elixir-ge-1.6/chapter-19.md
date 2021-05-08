
## Chapter.19 A More Complex Example

### Intro to Duper

* Duper will work by scanning all files in a directory tree, calculating hash for each. If `>1` files have same hash, they'll be reported as duplicates.

#### What is environment & constraints?

* it might not have enough memory to load big files in memory in whole to compute hash; need to handle big & small files both

#### What are the focal points?

* structural and temporal coupling

> * traverse through dir tree, each file once
>
> * something to calculate file hash, supporting small & big files
>
> * need a place to collect hash of each file mapped to it, like key-val
>
> * a manager that can process multiple files concurrently
>
> * then go through all entries made and check if any hash belongs to multiple files; display them correctly

* we'll create 4 servers (can be done with fewer)

> * `Results` holds results of scanning in memory
>
> * `PathFinder` responsible for returning each file path in dir tree, one at a time
>
> * `Worker` asks `PathFinder` for a path, calculates hash and passes result to `Gatherer`
>
> * `Gatherer` is the manager & coordinator, initiates the process & determines when done; fetchs results & reports

#### What are runtime characteristics?

* create `n` workers to parallelize file task to a sustainable degree

* have `hungry consumer` for Workers which ask for work; as push strategies migh leave some starving & other queued

#### What do I protect from errors?

* failing to read few files warrant a WARN not EXIT

* Results need be more ROBUST, Workers are expendable

#### How do I get this running?

* need to make sure dependency servers start before dependent; here it sequence would be `[Results, PathFinder] -> [Gatherer] -> [Worker]`

* ensuring the order in `child_spec` list passed to Supervisor will take care of starting order

* we'll add a Sub-Supervisor to manage pool of workers; could be created by Gatherer

> if all children of Supervisor are same, then that supervisor can be used to create them dynamically


### The Duper Application

* `mix new --sup duper19`

* add [results.ex](./duper19/lib/duper19/results.ex) for `Duper19.Results` with GenServer handling `:add` cast with `Map.update` for hash:file(s) key-val and `:find_duplicates` call

> add `Duper19.Results` to `children` list in `Duper19.Application`

* create a [FileSys.DirWalk](./duper19/lib/file_sys/dir_walk.ex) GenServer that returns all files in a dir and then recursively; uses [Stash](./duper19/lib/stash/stash.ex) to maintain state during any mishap

> `DirWalk` returns first file when it receives the path via `FileSys.DirWalk.ls/1`; and then every other path on `FileSys.DirWalk.next/0`
>
> it returns `:done` when no more paths are left to be returned
>
> add `FileSys.DirWalk` to `Application's child_spec list`

* add [pathfinder.ex](./duper19/lib/duper19/pathfinder.ex) with `Duper19.PathFinder` uses `DirWalk` to move through one file at a time and provide a simpler interface of `Duper19.PathFinder.next_path/0`

> add `{Duper19.PathFinder, "."}` to Application's `child_spec` list

* we'll create a Worker Supervisor managing only Workers; simply using DynamicSupervisor allowing to create arbitrary number of workers at runtime

> DynamicSupervisor encapsulates `:simple_one_for_one` strategy in regular supervisors

* it's hard to give Worker children name in `start_link`, there would be multiple servers by same name

* if a worker fails it to restart itself; if Worker Supervisor fails it's best to assume things have gone for worse; so we change the Application strategy to `one_for_all`

* [Gatherer](./duper19/lib/duper19/gatherer.ex) GenServer initiates with state of worker-count; interfaces for `Duper19.Results.add_hash_for/2` & `Duper19.Results.find_duplicates/0`; also `:done` get propogated to it for each expiring Worker to it knows when to end it all

> * we'll spin up Workers after Gatherer; since if Workers finish up before Gatherer will invoke and never know what to do
>
> * Gatherer uses callback `:kickoff` to trigger Workers; `init` uses `send_after` telling runtime to queue message to server immediately then exits and trigger `handle_info` callback

* [Worker][./duper19/lib/duper19/worker.ex] uses `send_after` to trigger receiving paths from `PathFinder` and `Gatherer` to `add_result` when hash has been calculated

* ensure all required children are added to Application


### But does it work?

* `mix run` to check if all compiles without issue; since `mix` exits once the App has running this doesn't Output anything

* `mix run --no-halt` to make Mix not exit by self

> could play with Worker count for speed


### Planning your Elixir Application

* Ask yourself 5 Questions: `What is env/constraints?`, `What are focal points?`, `What are runtime characteristics?`, `What do I protect from errors?`, and `How do I get this thing running?`.


### Next Up

> things are hard-coded here, let's investigate it next

---
