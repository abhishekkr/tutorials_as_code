
## clap\_eg

> Sample project for quick reference of Clap's Derive usage.

* an argument parser; for wide variety usecases; opinionated parsing; built-in validations; and default `-h`

* 2 approaches: **Derive** and **Builder**; there is `clap_serde` for deserializer based Command

* `Derive` is easier option to c/r/u/d. `Builder` is a low-level API, favored for better compile times and flexibility like playing with argument values in-place. Interop is possible.

### Derive

* `version` & `about` get default to `Cargo.toml` if skipped.

```
/*
 * 'cargo add clap --features derive'
 */

use clap::{ArgAction, Parser, Subcommand};
use std::path::PathBuf;

#[derive(Subcommand)]
enum Commands {
    Task {
        #[arg(short, long)]
        test: bool,
        #[arg(short, long, value_parser, value_delimiter = ' ', num_args = 1..)]
        list: Option<Vec<String>>,
    },
}

#[derive(Parser)]
#[command(name = "ClapEg")]
#[command(version, about = "Example of Clap arg parser.", long_about = None)]
struct Cli {
    name: Option<String>,

    #[arg(short, long, value_name = "FILEPATH")]
    config: Option<PathBuf>,

    #[arg(short, long, action = ArgAction::Count)]
    log_level: u8,

    #[command(subcommand)]
    command: Option<Commands>,
}

fn main() {
    let cli = Cli::parse();

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        println!("Value for name: {name}");
    }

    if let Some(config_path) = cli.config.as_deref() {
        println!("Value for config: {}", config_path.display());
    }

    match cli.log_level {
        0 => println!("Emergency Logs only."),
        1 => println!("Alert and Emergency Logs."),
        2 => println!("Critical Logs and above."),
        3 => println!("At leastt Error logs."),
        4 => println!("Warning and more."),
        5 => println!("Info Logs."),
        _ => println!("Debug Mode."),
    }

    match &cli.command {
        Some(Commands::Task { test, list }) => {
            if *test {
                println!("Tasks wouldn't be run.");
            }
            if list.is_some() {
                println!("Tasks: {:?}", list);
            } else {
                println!("No Tasks.");
            }
        }
        None => {}
    }
}
```

> with usage samples of..

```
% cargo run -- -V
ClapEg 0.1.0

% cargo run -- -h
Example of Clap arg parser.

Usage: clap_eg [OPTIONS] [NAME] [COMMAND]

Commands:
  task  
  help  Print this message or the help of the given subcommand(s)

Arguments:
  [NAME]  

Options:
  -c, --config <FILEPATH>  
  -l, --log-level...       
  -h, --help               Print help
  -V, --version            Print version

% cargo run -- ABC -c ./Cargo.lock                 
Value for name: ABC
Value for config: ./Cargo.lock
Emergency Logs only.

% cargo run -- ABC task -l a b c   
Value for name: ABC
Emergency Logs only.
Tasks: Some(["a", "b", "c"])

% cargo run -- task -l a b c -t    
Emergency Logs only.
Tasks wouldn't be run.
Tasks: Some(["a", "b", "c"])

% cargo run -- task -l a b -l c
Emergency Logs only.
Tasks: Some(["a", "b", "c"])

% cargo run -- -l                 
Alert and Emergency Logs.

% cargo run -- -llll
Warning and more.
```


---
