
## Nerves

> [project doc](http://nerves-project.org/)

### Introduction and requirements

* Nerves is an elixir framework for embedded s/w

* supported target platforms are generic x86\_64, multiple raspberry pi and beaglebone editions


### Setup

* [getting started](https://hexdocs.pm/nerves/getting-started.html), Nerves contain 3 parts

> * **Platform**, a customized minimal buildroot-derived linux booting directly to BEAM VM
>
> * **Framework**, ready-to-go library for Elixir modules
>
> * **Tooling**, cli to manage builds, update firmware, configure devices and more

* [installation doc](https://hexdocs.pm/nerves/installation.html)

> * has bunch of pre-requisites based of dev platform's build system
>
> * update `hex` and `rebar` even if pre-installed

* then add `nerves_bootstrap` archive to Mix env, `mix archive.install hex nerves_bootstrap`

* getting started doc has more platform specific details


### Creating a project

* to create a new Nerves app `mix nerves.new someproject`, env var `MIX_TARGET` can be used for `mix` tasks as `MIX_TARGET=rpi3 mx deps.get`

* during `deps.get` it will download appropriate system image and toolchain for required platform


### Burning the firmware

* `mix firmware.burn` to burn eveyrthing required on a drive/card required for platform


### Setting up networking

* `nerves_network` provide constructs to utilize device network, by default uses `DHCP`

* can assign static address under `config/config.exs`


### Networking firmware burning

* allows to push firmware updates over network, as much productive than physical distribution of cards per built

* `nerves_firmware_ssh` can be utilized creating upload script via `mix firmware.gen.script`


### Setting up the LED control

* `nerve_leds` package allows interaction with device LEDs


### Adding the web server

* can use `Plug.Router` available via `plug_cowboy` package

---
