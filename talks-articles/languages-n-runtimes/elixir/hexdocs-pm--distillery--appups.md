
## Appups

> [source](https://hexdocs.pm/distillery/guides/appups.html)

* can generate appup under `rel` using `mix distillery.gen.appup`

* can programmatically apply changes to appups using Appup Transformation


### Reviewing genearted appups

* any `code_change`or `system_code_change` callbacks added to handle state change in special processes (`Gen*`) are required to be in the appup

> if any additional arg need by code change handler then use `:update` instruction corresponding to that module providing args as `{:advanced, args}`

* all modules changed, shall be in `:update` or `:load_module` instruction

* all added module shall be in `:add_module` & deleted modules in `:remove_module` instruction

* any custom steps needed during upgrade; can also use custom `:apply` instruction

* if also upgrading ERTS used, there shall be a `:restart_new_emulator`

* if need restart emulator for some reason, use `:restart_emulator`

---

### References for more details

* [Appup Transformation](https://hexdocs.pm/distillery/extensibility/appup_transforms.html)

* [Appup Cookbook](http://erlang.org/doc/design_principles/appup_cookbook.html)

* [Appup Manual](http://erlang.org/doc/man/appup.html)

---
