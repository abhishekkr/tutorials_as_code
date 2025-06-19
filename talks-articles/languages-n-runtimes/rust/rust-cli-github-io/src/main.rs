use clap::Parser;
use log::{error, info};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::io::Result;

#[derive(Parser)]
struct Cli {
    pattern: String,
    path: std::path::PathBuf,
}

fn match_pattern(cli: Cli) -> Result<()> {
    let f = match File::open(cli.path) {
        Ok(file) => file,
        Err(err) => {
            error!("Unable to open file: {:?}", err,);
            std::process::exit(1); // could also panic
        }
    };
    let reader = BufReader::new(f);

    let pb = indicatif::ProgressBar::new(100);
    for line in reader.lines() {
        let lyn = line?.to_owned();
        if lyn.contains(&cli.pattern) {
            println!("{}", lyn);
        }
        pb.inc(1);
    }
    pb.finish();
    Ok(())
}

fn main() -> Result<()> {
    env_logger::init();
    info!("initiating rcli");

    let cli = Cli::parse();
    info!("pattern: {:?}, path: {:?}", cli.pattern, cli.path);
    match_pattern(cli)
}
