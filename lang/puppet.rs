// Copyright © 2023 David Caldwell <david@porkrind.org>

use serde::Serialize;
use std::io::Write;

use crate::lang::serializer::{Serializer,Language,ArrLit,MapLit,TupLit,MapKey,Result,SepStyle};
use crate::lang::string::*;

#[inline]
#[allow(dead_code)]
pub fn to_vec<T: Serialize>(value: &T) -> Result<Vec<u8>> {
    let mut writer = Vec::with_capacity(128);
    to_writer(&mut writer, value)?;
    Ok(writer)
}

#[inline]
#[allow(dead_code)]
pub fn to_string<T: Serialize>(value: &T) -> Result<String> {
    let vec = to_vec(value)?;
    let string = unsafe {
        // We do not emit invalid UTF-8.
        String::from_utf8_unchecked(vec)
    };
    Ok(string)
}

pub fn to_writer<'a, W:Write, T: Serialize>(writer: W, value: &'a T) -> Result<()> {
    let mut serializer = Serializer::new(writer,
                                         Language{array_lit:  ArrLit("[", "," ,"]"),
                                                  map_lit:    MapLit("{", " => ", ",", "}" ),
                                                  tuple_lit:  TupLit("[", ",",  "]" ),
                                                  map_key:    MapKey::AlwaysQuote,
                                                  sep_style:  SepStyle::Between,
                                                  true_lit:  "true",
                                                  false_lit: "false",
                                                  null_lit:  "undef",
                                                  strser: PuppetStringSerializer{}, },);
    serializer.serialize(value)
}

struct PuppetStringSerializer;

impl<'a> StringSerializer<'a> for PuppetStringSerializer {
    fn quote_styles(&self) -> Vec<QuoteStyle<'static>> {
        use array_lit::vec;
        let dq_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Cus as u8; 32 ],
            (Ascii::TAB  as usize): Esc::Rep('t'),
            (Ascii::LF   as usize): Esc::Rep('n'),
            (Ascii::CR   as usize): Esc::Rep('r'),
            //(Ascii::SPC  as usize): Esc::Rep('s'),
            ('"'         as usize): Esc::Rep('"'),
            ('\\'        as usize): Esc::Rep('\\'),
            ('$'         as usize): Esc::Rep('$'), // don't accidentally interpolate
        }];

        let sq_esc = vec![Esc::No; 128; {
            [0]: [Esc::Ill as u8; 32],
            ('\'' as usize): Esc::Rep('\''),
            ('\\' as usize): Esc::Rep('\\'),
        }];

        let base_here_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Ill as u8; 32 ],
            (Ascii::TAB  as usize): Esc::No,
            (Ascii::LF   as usize): Esc::No,
        }];

        let u_here_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Cus as u8; 32 ],
            (Ascii::TAB  as usize): Esc::No,
            (Ascii::LF   as usize): Esc::No,
        }];

        vec![QuoteStyle{ rep: QuoteRep::Bare,
                         escapes: EscapeTable::try_from(&sq_esc).expect("sq_esc"),
                         catenate: None,
                         multiline: Multiline::No, },
             QuoteStyle{ rep: QuoteRep::Same("'"),
                         escapes: EscapeTable::try_from(&sq_esc).expect("sq_esc"),
                         catenate: None,
                         multiline: Multiline::No, },
             QuoteStyle{ rep: QuoteRep::Same("\""),
                         escapes: EscapeTable::try_from(&dq_esc).expect("dq_esc"),
                         catenate: None,
                         multiline: Multiline::No, },
             QuoteStyle{ rep: QuoteRep::HereDoc("@({NAME})", "| {NAME}"),
                         escapes: EscapeTable::try_from(base_here_esc).expect("base_here_esc"),
                         catenate: None,
                         multiline: Multiline::Indent, },
             QuoteStyle{ rep: QuoteRep::HereDoc("@({NAME}/u)'", "{NAME}"),
                         escapes: EscapeTable::try_from(u_here_esc).expect("u_here_esc"),
                         catenate: None,
                         multiline: Multiline::Indent, },
        ]
    }

    fn custom_escape(&self, c: char) -> Option<String> {
        Some(format!(r"\u{:04x}", c as u16 /* will only ever be 0-32*/))
    }

    fn valid_bare_str(&self, s: &str) -> bool {
        use array_lit::arr;
        const RESERVED: [&str; 27] = ["and", "application", "attr", "case", "component", "consumes", "default", "define",
                                      "elsif", "environment", "false", "function", "if", "import", "in", "inherits", "node",
                                      "or", "private", "produces", "regexp", "site", "true", "type", "undef", "unit", "unless",];
        // letters, digits, hyphens (-), and underscores (_) are allowed
        const VALID_BARE_CHARS: [bool; 128] = arr![false; 128; {
            [{'a' as usize}]: [true; 26],
            [{'A' as usize}]: [true; 26],
            [{'0' as usize}]: [true; 10],
            ('-' as usize): true,
            ('_' as usize): true,
        }];
        match s.chars().next() {
            Some(c) if c.is_ascii_lowercase() => {}
            _ => return false,
        }
        if s.find(|c: char| !(c <= '\x7f' && VALID_BARE_CHARS[c as usize])).is_some() {
            return false;
        }
        // FIXME: this should be a regexp. This is probably slow.
        for r in RESERVED {
            if s == r { return false }
        }
        true
    }
}

#[cfg(test)]
mod test_sads {
    use super::*;
    use crate::ser_test;

    ser_test!(test_empty_map, r#"{}"#);

    ser_test!(test_simple_map1, r#"{ key => value }"#);
    ser_test!(test_simple_map2, r#"{ key => value, key2 => value2 }"#);
    ser_test!(test_nested_maps,
r#"{
  key => {
    nested => {
      wow => {
        how => {
          far => {
            can => { we => { go => '!' } }
          }
        }
      }
    }
  }
}"#);

    ser_test!(test_empty_array, "[]");
    ser_test!(test_simple_array1, r#"[ 4, 5, 6, 7 ]"#);
    ser_test!(test_non_homogeneous_array,
r#"[
  1,
  2,
  non-homogeneous,
  3,
  undef
]"#);

    ser_test!(test_arrays_of_arrays1,
r#"[
  [ 1, 2 ], [ 3, 4 ]
]"#);
    ser_test!(test_arrays_of_arrays2,
r#"[
  [
    [ 1, 2 ], [ 3, 4 ]
  ], [
    [ 5, 6 ], [ 7, 8 ]
  ]
]"#);
    ser_test!(test_arrays_of_arrays3,
r#"[
  [ a, b ], [ 1, 2 ]
]"#);
    ser_test!(test_arrays_of_arrays4,
r#"[
  [
    [
      [
        [
          [
            [
              [
                [
                  [
                    [ 1, 2 ], [
                      3, [
                        4, [ 5, [ 6, undef ] ]
                      ]
                    ],
                    yep,
                    7
                  ]
                ]
              ]
            ]
          ],
          more
        ]
      ]
    ]
  ]
]"#);

    ser_test!(test_mixed1,
r#"{
  array => [ 1, 2, 3, 4 ]
}"#);
    ser_test!(test_mixed2,
r#"[
  { a => [ 2, 3 ] }, { b => [ 3, 4 ] }
]"#);
    ser_test!(test_true, r#"true"#);
    ser_test!(test_false, r#"false"#);
    ser_test!(test_large,
r#"{
  name => babel,
  version => '7.20.15',
  'private' => true,
  'type' => commonjs,
  scripts => {
    postinstall => 'husky install',
    bootstrap => 'make bootstrap',
    'codesandbox:build' => 'make build-no-bundle',
    build => 'make build',
    fix => 'make fix',
    lint => 'make lint',
    test => 'make test',
    version => 'yarn --immutable-cache && git add yarn.lock',
    'test:esm' => 'node test/esm/index.js',
    'test:runtime:generate-absolute-runtime' => 'node test/runtime-integration/generate-absolute-runtime.cjs',
    'test:runtime:bundlers' => 'node test/runtime-integration/bundlers.cjs',
    'test:runtime:node' => 'node test/runtime-integration/node.cjs'
  },
  packageManager => 'yarn@3.2.4',
  devDependencies => {
    '@babel/cli' => '^7.20.7',
    '@babel/core' => '^7.20.7',
    '@babel/eslint-config-internal' => 'workspace:^',
    '@babel/eslint-parser' => 'workspace:^',
    '@babel/eslint-plugin-development' => 'workspace:^',
    '@babel/eslint-plugin-development-internal' => 'workspace:^',
    '@babel/plugin-proposal-dynamic-import' => '^7.18.6',
    '@babel/plugin-proposal-export-namespace-from' => '^7.18.9',
    '@babel/plugin-proposal-object-rest-spread' => '^7.20.7',
    '@babel/plugin-transform-modules-commonjs' => '^7.20.11',
    '@babel/plugin-transform-runtime' => '^7.19.6',
    '@babel/preset-env' => '^7.20.2',
    '@babel/preset-typescript' => '^7.18.6',
    '@babel/runtime' => '^7.20.7',
    '@rollup/plugin-babel' => '^5.3.1',
    '@rollup/plugin-commonjs' => '^22.0.2',
    '@rollup/plugin-json' => '^4.1.0',
    '@rollup/plugin-node-resolve' => '^13.3.0',
    '@rollup/plugin-replace' => '^4.0.0',
    '@types/node' => '^18.11.7',
    '@typescript-eslint/eslint-plugin' => '^5.46.0',
    '@typescript-eslint/parser' => '^5.46.0',
    babel-plugin-transform-charcodes => '^0.2.0',
    c8 => '^7.12.0',
    chalk => '^5.0.0',
    charcodes => '^0.2.0',
    core-js => '^3.26.0',
    eslint => '^8.22.0',
    eslint-formatter-codeframe => '^7.32.1',
    eslint-import-resolver-node => '^0.3.6',
    eslint-plugin-import => '^2.26.0',
    eslint-plugin-jest => '^27.1.5',
    eslint-plugin-node => '^11.1.0',
    eslint-plugin-prettier => '^4.2.1',
    glob => '^8.0.3',
    gulp => '^4.0.2',
    gulp-filter => '^7.0.0',
    gulp-plumber => '^1.2.1',
    husky => '^7.0.4',
    import-meta-resolve => '^1.1.1',
    jest => '^29.0.1',
    jest-light-runner => '^0.4.0',
    jest-worker => '^29.0.1',
    lint-staged => '^13.0.3',
    mergeiterator => '^1.4.4',
    prettier => '^2.7.1',
    rollup => '^2.78.0',
    rollup-plugin-dts => '^5.0.0',
    rollup-plugin-polyfill-node => '^0.10.2',
    rollup-plugin-terser => '^7.0.2',
    semver => '^6.3.0',
    shelljs => '^0.8.5',
    test262-stream => '^1.4.0',
    through2 => '^4.0.0',
    typescript => '~4.9.3'
  },
  workspaces => [
    'codemods/*',
    'eslint/*',
    'packages/*',
    'test/esm',
    'test/runtime-integration/*',
    benchmark
  ],
  resolutions => {
    browserslist => 'npm:4.21.3',
    caniuse-lite => 'npm:1.0.30001397',
    core-js-compat => 'npm:3.25.1',
    electron-to-chromium => 'npm:1.4.248',
    'glob-watcher/chokidar' => 'npm:^3.4.0',
    '@types/babel__core' => 'link:./nope',
    '@types/babel__traverse' => 'link:./nope',
    '@babel/parser/@babel/types' => 'workspace:*',
    'babel-plugin-polyfill-corejs2/@babel/compat-data' => 'workspace:*'
  },
  engines => { yarn => '>=1.4.0' },
  lint-staged => {
    '*.{js,cjs,mjs,ts}' => [ 'eslint --format=codeframe --cache --cache-strategy=content' ]
  },
  dependenciesMeta => {
    core-js => { built => false },
    core-js-pure => { built => false }
  },
  changelog => {
    repo => 'babel/babel',
    cacheDir => '.changelog',
    labels => {
      'PR: Spec Compliance :eyeglasses:' => ':eyeglasses: Spec Compliance',
      'PR: Breaking Change :boom:' => ':boom: Breaking Change',
      'PR: Deprecation: :loudspeaker:' => ':loudspeaker: Deprecation',
      'PR: New Feature :rocket:' => ':rocket: New Feature',
      'PR: Bug Fix :bug:' => ':bug: Bug Fix',
      'PR: Polish :nail_care:' => ':nail_care: Polish',
      'PR: Docs :memo:' => ':memo: Documentation',
      'PR: Internal :house:' => ':house: Internal',
      'PR: Performance :running_woman:' => ':running_woman: Performance',
      'PR: Revert :leftwards_arrow_with_hook:' => ':leftwards_arrow_with_hook: Revert',
      'PR: Output optimization :microscope:' => ':microscope: Output optimization'
    }
  }
}"#);
}

#[cfg(all(test,no))]
mod test_other {
    use super::*;
    #[test]
    fn test_struct() {
        #[derive(Serialize)]
        struct Test {
            int: u32,
            seq: Vec<&'static str>,
        }

        let test = Test {
            int: 1,
            seq: vec!["a", "b"],
        };
        let expected = r#"Test.new(int:1,seq:['a','b'])"#;
        assert_eq!(to_string(&test).unwrap(), expected);
    }

    #[test]
    fn test_enum() {
        #[derive(Serialize)]
        enum E {
            Unit,
            Newtype(u32),
            Tuple(u32, u32),
            Struct { a: u32 },
        }

        let u = E::Unit;
        let expected = r#"E.Unit"#;
        assert_eq!(to_string(&u).unwrap(), expected);

        let n = E::Newtype(1);
        let expected = r#"E.Newtype(1)"#;
        assert_eq!(to_string(&n).unwrap(), expected);

        let t = E::Tuple(1, 2);
        let expected = r#"E.Tuple(1,2)"#;
        assert_eq!(to_string(&t).unwrap(), expected);

        let s = E::Struct { a: 1 };
        let expected = r#"E.Struct(a:1)"#;
        assert_eq!(to_string(&s).unwrap(), expected);
    }
}

#[cfg(test)]
mod test_strings {
    use super::PuppetStringSerializer as TestStringSerializer;
    // use super::new_string_serializer as new_string_serializer;
    use crate::str_test;

    str_test!(test_control_characters,
r#"@(END/u)'
\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008	
\u000b\u000c\u000d\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f
END"#);
    str_test!(test_simple, r#"'Just a simple string.'"#);

    str_test!(test_interpolation, "", None, "Maybe I've got some ${interpolation}",
              r#"'Maybe I\'ve got some ${interpolation}'"#);

    str_test!(test_newlines, r#"@(END)
What about a few
embedded
newlines?
| END"#);

    str_test!(test_newlines_lots, r#""What\nabout\na\nwhole\nbunch\nof\nwonderful\nembedded\nnewlines?""#);

    str_test!(test_quotes, r#"'"Don\'t quote me on that"'"#);

    str_test!(test_backticks, r#"'`Lots` `of` `back``ticks`'"#);

    str_test!(test_single_quotes, r#""'Lots' 'of' 'single' 'quotes'""#);

    str_test!(test_double_quotes, r#"'"Lots" "of" "double" "quotes"'"#);

    str_test!(test_source_code,
r#"@(SOME_CODE)

// What about a giant chunk of source code??
#[derive(Debug, PartialEq)]
enum RenderMode {
    Multiline,
    Singleline,
}

struct StringWithLineLen {
    pub str: String,
    pub linelen: usize,
}

impl StringWithLineLen {
    fn new() -> StringWithLineLen {
        StringWithLineLen { str: String::new(), linelen: 0 }
    }
}

impl std::ops::AddAssign<&str> for StringWithLineLen {
    fn add_assign(&mut self, s: &str) {
        self.str += s;
        if s == "\n" { // we only print newlines by themselves
            self.linelen = 0;
        } else {
            self.linelen += s.len();
        }
    }
}


| SOME_CODE"#);

    str_test!(test_gettysburg,
r#"@(END)
Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

    Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

    But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.

| END"#);

    str_test!(test_gettysburg_indented,
               r#"@(END)
                  Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.
                  
                      Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.
                  
                      But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.
                  
                  | END"#);

    str_test!(test_very_long_string,
r#"'Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_'"#);
}