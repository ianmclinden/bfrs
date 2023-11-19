use bfrs::{BfMachine, Result};
use clap::{ArgAction, Parser};
use log::LevelFilter;
use std::{fs, path::PathBuf};

#[derive(Parser)]
struct Args {
    /// Program to run
    #[clap(index = 1)]
    program: Option<PathBuf>,

    /// Delay between instructions (in milliseconds)
    #[clap(short = 'd', long = "delay", default_value_t = 0)]
    step_delay: u64,

    /// Output verbosity
    #[arg(short, long, action = ArgAction::Count)]
    verbose: u8,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let console_log_level = match args.verbose {
        0 => LevelFilter::Warn,
        1 => LevelFilter::Info,
        2 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };

    simple_logger::SimpleLogger::new()
        .with_level(console_log_level)
        .init()
        .unwrap();

    let mut vm = BfMachine::default();
    vm.set_delay(args.step_delay);

    match args.program {
        Some(f) => {
            let prog = fs::read_to_string(f.clone())
                .unwrap_or_else(|_| panic!("Could not load file {:?}", f));
            vm.execute(&prog)
        }
        None => vm.interactive(),
    }
}
