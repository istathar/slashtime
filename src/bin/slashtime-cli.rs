use clap::{value_parser, Arg, ArgAction, Command};
use crossterm::{
    queue,
    style::{Color, Print, ResetColor, SetForegroundColor},
    terminal::{Clear, ClearType},
};
use slashtime::{find_local, format_line};
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let now = tz::UtcDateTime::now()?;

    let matches = Command::new("slashtime-cli")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Show the time in various places.")
        .arg(
            Arg::new("places")
                .long("places")
                .value_name("filename")
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .help("The tzlist file listing the places to show, rather than the one in the slashtime config directory."),
        )
        .arg(
            Arg::new("home")
                .help("The IANA name of a zone in the tzlist to measure offsets from, rather than the machine's own time zone."),
        )
        .get_matches();

    let places = matches.get_one::<PathBuf>("places");
    let locations = slashtime::loading::load_tzlist(places.map(PathBuf::as_path), None, &now)
        .unwrap_or_else(|e| {
            eprintln!("Unable to load tzlist: {}", e);
            std::process::exit(1);
        });

    // Offsets are measured from the location in the machine's own time zone,
    // unless a zone is named on the command line, in which case they are
    // measured from there instead.

    let pivot = match matches.get_one::<String>("home") {
        Some(name) => locations
            .iter()
            .position(|location| location.iana_zone == *name)
            .unwrap_or_else(|| {
                eprintln!("Zone \"{}\" is not present in your tzlist", name);
                std::process::exit(1);
            }),
        None => find_local(&locations).unwrap(),
    };
    let pivot = &locations[pivot];

    // Output the formatted locality, time, date, and offest for each location.

    let mut out = std::io::stdout();

    for location in &locations {
        let line = format_line(location, pivot, &now)?;

        if location.is_zulu {
            // using the macro
            queue!(
                out,
                SetForegroundColor(Color::DarkGreen),
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        } else if location.is_local {
            queue!(
                out,
                SetForegroundColor(Color::DarkCyan),
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n")
            )?;
        } else {
            queue!(
                out,
                Print(line),
                Clear(ClearType::UntilNewLine),
                ResetColor,
                Print("\n"),
            )?;
        }
    }

    Ok(())
}
