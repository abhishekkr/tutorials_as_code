
## Chapter.25 More Cool Stuff

### Writing your own Sigils

* can create strings `~s{like this}` and regexps `~r{..ge..s}`; can extend these Sigils to custom types

* writing `~s{}` would result in call `sigil_s`; passed 2 values (string between delimeters and list containing any lowercase characters immediately after delims)

* if sigil is uppercase, no string interpolation is done as for `~L`

* to override any predefined sigil, would need to explicitly `import Kernel, except: ..`

#### Picking up the Options

* we'll write a Sigil specifying Color constants like `~c{red}` gives `0xff0000`; supporting option `h` for `HSB` conversion as `{0, 100, 100}`

> Exercise MoreCoolStuff-1 and 2 and 3 are covered under [sigil\_csv.exs](chapter-25/sigil_csv.exs)


### Multi-app Umbrella Projects

* several projects might be multiple libraries just created as apps, can be made part of same Project using Umbrella Project

* create an Umbrella Project `mix new --umbrella eval`

* creating sub-projects as `cd eval/apps ; mix new sigil_line ; mix new evaluator` creating 2 sub-projects

* now to compile goto umbrella root & run tasks as `cd .. ; mix compile`

> * Umbrella structure is light, a mix file and apps dir; also a boilerplate config dir

* don't worry of Umbrella Project decision from start as can always be moved into `apps` dir easily later

> we can import Modules across, but then to test cohesively we need to run `mix test` in umbrella root


### But Wait! There's More!

> these are just highlights

---
