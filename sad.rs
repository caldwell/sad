// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::boxed::Box;
use std::error::Error;
use std::fs::File;
use std::io::{stdin, stdout};
use std::path::{Path, PathBuf};

use docopt::Docopt;

mod lang;

const USAGE: &'static str = "
Usage:
  sad --help
  sad   [-h] [--check] [--ugly] [--in=<format>] [--out=<format>] [<in-file>] [<out-file>]
  json2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]
  yaml2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]
  toml2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]

Options:
  -h --help          Show this message.

  -i --in=<format>   Select input format. One of: json yaml toml
                     If not specified it will try to guess the input format from
                     the <in-file> extension.

  -o --out=<format>  Select output format.
                     One of: json yaml toml ruby php go python
                     If not specified it try to guess from the <out-file>
                     extension. If there is no <out-file> then it will use the
                     input format.

  -u --ugly          Don't pretty-print the output (JSON and TOML only)

     --check         Don't output. Parse the input and report errors.
";

#[derive(Debug, serde::Deserialize)]
struct Args {
    flag_in:      Option<Format>,
    flag_out:     Option<Format>,
    flag_ugly:    bool,
    flag_check:   bool,
    arg_in_file:  Option<PathBuf>,
    arg_out_file: Option<PathBuf>,
}

#[derive(Debug, serde::Deserialize)]
enum Format {
    Yaml,
    Toml,
    Json,
    Ruby,
    Php,
    Go,
    Python,
}

fn main() -> Result<(), Box<dyn Error>> {
    let _args: Vec<String> = std::env::args().collect();
    let mut args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    // Detect input format from our exe name.
    args.flag_in = match { std::env::args().next()
                           .and_then(|a| { Path::new(&a)
                                           .file_name()
                                           .map(|f| f.to_string_lossy().to_lowercase() ) })
                           .as_deref() } {
        Some("json2") => Some(Format::Json),
        Some("yaml2") => Some(Format::Yaml),
        Some("toml2") => Some(Format::Toml),
        _             => args.flag_in,
    };

    // println!("args={:?}", args);

    let mut inf: Box<dyn std::io::Read> = match args.arg_in_file {
        Some(ref name) => Box::new(File::open(&name)?),
        None           => Box::new(stdin()),
    };

    // Try to guess the input format from the extension
    match (&args.flag_in, &args.arg_in_file) {
        (Some(_), _)              => {} // already have an explicit type, don't guess.
        (None,    None)           => { Err("Can't determine input format. Use `--in` option to specify")? },
        (None,    Some(ref name)) => { args.flag_in = Some(Format::from_filename(name)
                                                            .map_err(|e| format!("Input: {}. Use `--in` option to specify",e))?); },
    }

    let data: serde_yaml::Value =
        match args.flag_in {
            Some(Format::Json) => serde_json::from_reader(inf)?,
            Some(Format::Yaml) => serde_yaml::from_reader(inf)?,
            Some(Format::Toml) => { let mut toml_str = String::new();
                                    inf.read_to_string(&mut toml_str)?;
                                    toml::from_str(&toml_str)? },
            Some(_)            => Err("That format is not available as an input")?,
            None => unreachable!(),
        };

    if args.flag_check { return Ok(()) }

    let mut outf: Box<dyn std::io::Write> = match args.arg_out_file {
        Some(ref name) => Box::new(File::create(&name)?),
        None           => Box::new(stdout()),
    };

    // Try to guess the output format from the extension
    match (&args.flag_out, &args.arg_out_file) {
        (Some(_), _)              => {}                                 // already have an explicit type, don't guess.
        (None,    None)           => { args.flag_out = args.flag_in; }, // If no --out then assume they want to pretty print the input
        (None,    Some(ref name)) => { args.flag_out = Some(Format::from_filename(name)
                                                            .map_err(|e| format!("Output: {}. Use `--out` option to specify",e))?); },
    }

    match (args.flag_out, args.flag_ugly) {
        (Some(Format::Json), false) => serde_json::to_writer_pretty(outf, &data)?,
        (Some(Format::Json), true)  => serde_json::to_writer(outf, &data)?,
        (Some(Format::Yaml), _)     => serde_yaml::to_writer(outf, &data)?,
        (Some(Format::Toml), false) => { let toml_str = toml::to_string_pretty(&data)?;
                                         outf.write(&mut toml_str.as_bytes())?; },
        (Some(Format::Toml), true)  => { let toml_str = toml::to_string(&data)?;
                                         outf.write(&mut toml_str.as_bytes())?; },
        (Some(Format::Ruby), _)     => lang::ruby::to_writer(outf, &data)?,
        (Some(Format::Php),  _)     => lang::php::to_writer(outf, &data)?,
        (Some(Format::Go),   _)     => lang::go::to_writer(outf, &data)?,
        (Some(Format::Python),_)    => lang::python::to_writer(outf, &data)?,
        (None,_) => unreachable!(),
    }

    Ok(())
}

impl Format {
    fn from_filename(p: &Path) -> Result<Format, Box<dyn Error>> {
        match Path::new(p).extension().map(|e| e.to_string_lossy().to_lowercase()).as_deref() {
            Some("json") => Ok(Format::Json),
            Some("yml")  |
            Some("yaml") => Ok(Format::Yaml),
            Some("toml") => Ok(Format::Toml),
            Some(ref ext) => Err(format!("Couldn't guess format from `{}` extension", ext))?,
            None          => Err(format!("Couldn't guess format from `{}`", p.to_string_lossy()))?
        }
    }
}
