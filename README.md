Sad
===
_**S**calar, **A**rray, **D**ictionary_

Convert between JSON, YAML, TOML formats.

Usage
-----

    Usage:
      sad --help
      sad   [-h] [--check] [--ugly] [--in=<format>] [--out=<format>] [<in-file>] [<out-file>]
      json2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]
      yaml2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]
      toml2 [-h] [--check] [--ugly] [--out=<format>] [<in-file>] [<out-file>]

### Options:

#### `-h` `--help`

Show this message.

#### `-i` `--in=<format>`

Select input format. One of: `json` `yaml` `toml`. If not specified it will try
to guess the input format from the `<in-file>` extension.

#### `-o` `--out=<format>`

Select output format.  One of: `json` `yaml` `toml` `ruby` `php` `go` `python`
`puppet`. If not specified it try to guess from the `<out-file>` extension. If
there is no `<out-file>` then it will use the input format.

#### `-u` `--ugly`

Don't pretty-print the output (JSON and TOML only)

#### `--check`

Don't output. Parse the input and report errors.


Examples
--------

Convert `myfile.json` (assumed to be in JSON format because of the extension)
into YAML, printing it to stdout:

    sad myfile.json --out yaml

The same as above, but use stdin instead of reading the file directly:

    sad --in json --out yaml < myfile.json

Convert `myfile.json` from JSON to YAML, store it in `myfile.yaml`:

    sad myfile.json myfile.yaml

Pretty print JSON in `myfile.json` (it will assume the input format if no
`<out-file>` and no `--out` is specified):

    sad myfile.json

Ugly (compact) print JSON in `myfile.json`:

    sad --ugly myfile.json

Pretty print JSON from `myfile` and store it in `myfile.pretty` (`--out` is
required here because it can't guess the output format from the `myfile.pretty`
name):

    sad --out json myfile.json myfile.pretty

Same as above (don't need `--out` in this case since it's printing to stdout
which makes it assume you wanted to pretty print):

    sad myfile.json > myfile.pretty

Pretty print myfile.json in place:

    sad myfile.json myfile.json

**This will not work** (the shell will clobber the file before `sad` can open
it):

    # Won't work!
    #   sad myfile.json > myfile.json

If you symlink the executable to `json2`, `yaml2`, or `toml2` then running those
will hardcode the input format accordingly:

    ln -s ./sad ./json2
    ./json2 < myfile.json

License
-------
Copyright © 2023 David Caldwell <david@porkrind.org>

Licensed under the MIT License (see LICENSE.md for details)
