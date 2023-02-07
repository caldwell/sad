// Copyright © 2023 David Caldwell <david@porkrind.org>

use serde::Serialize;

use crate::lang::serializer::{Serializer,Language,ArrLit,MapLit,TupLit,MapKey,Result,SepStyle};


pub fn to_string<'a, 'b, T: Serialize>(value: &'a T) -> Result<String> {
    let mut serializer = Serializer::new(Language{array_lit:  ArrLit("[", "," ,"]"),
                                                  map_lit:    MapLit("{", " => ", ",", "}" ),
                                                  tuple_lit:  TupLit("[", ",",  "]" ),
                                                  map_key:    MapKey::AlwaysQuote,
                                                  sep_style:  SepStyle::Between,
                                                  true_lit:  "true",
                                                  false_lit: "false",
                                                  null_lit:  "nil",
                                                  strser: RubyStringSerializer::new(),},);
    serializer.to_string(value)
}

use crate::lang::string::*;

struct RubyStringSerializer;

impl RubyStringSerializer {
    fn new() -> StringSerializer<'static> {
        use array_lit::vec;
        let dq_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Hex as u8; 32 ],
            (Ascii::BEL  as usize): Esc::Rep('a'),
            (Ascii::BS   as usize): Esc::Rep('b'),
            (Ascii::TAB  as usize): Esc::Rep('t'),
            (Ascii::LF   as usize): Esc::Rep('n'),
            (Ascii::VT   as usize): Esc::Rep('v'),
            (Ascii::FF   as usize): Esc::Rep('f'),
            (Ascii::CR   as usize): Esc::Rep('r'),
            (Ascii::ESC  as usize): Esc::Rep('e'),
            //(Ascii::SPC  as usize): Esc::Rep('s'),
            ('"'         as usize): Esc::Rep('"'),
            ('\\'        as usize): Esc::Rep('\\'),
            ('#'         as usize): Esc::Rep('#'), // don't accidentally interpolate
        }];

        let ruby_percent_dq_escapes = |end| {
            let mut esc = dq_esc.clone();
            esc[end as usize] = end as u8;
            esc['"' as usize] = Esc::No;
            EscapeTable::try_from(&esc[..]).expect("percent_dq_escapes")
        };

        let dq_here_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Hex as u8; 32 ],
            (Ascii::BEL  as usize): Esc::Rep('a'),
            (Ascii::BS   as usize): Esc::Rep('b'),
            (Ascii::TAB  as usize): Esc::No,
            (Ascii::LF   as usize): Esc::No,
            (Ascii::VT   as usize): Esc::Rep('v'),
            (Ascii::FF   as usize): Esc::Rep('f'),
            (Ascii::CR   as usize): Esc::Rep('r'),
            (Ascii::ESC  as usize): Esc::Rep('e'),
            ('\\'        as usize): Esc::Rep('\\'),
            ('#'         as usize): Esc::Rep('#'), // don't accidentally interpolate
        }];

        let sq_esc = vec![Esc::No; 128; {
            [0]: [Esc::Ill as u8; 32],
            ('\'' as usize): Esc::Rep('\''),
            ('\\' as usize): Esc::Rep('\\'),
        }];

        let ruby_percent_sq_escapes = |end| {
            let mut esc = sq_esc.clone();
            esc[end as usize] = end as u8;
            esc['\'' as usize] = Esc::No;
            EscapeTable::try_from(&esc[..]).expect("percent_sq_escapes")
        };

        let sq_here_esc = vec![Esc::No; 128; {
            [0]: [Esc::Ill as u8; 32],
            (Ascii::TAB  as usize): Esc::No,
            (Ascii::LF   as usize): Esc::No,
        }];

        StringSerializer::new()
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("'"),
                                          escapes: EscapeTable::try_from(&sq_esc).expect("sq_esc"),
                                          catenate: Some("\\"),
                                          multiline: Multiline::No, })
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("\""),
                                          escapes: EscapeTable::try_from(&dq_esc).expect("dq_esc"),
                                          catenate: Some("\\"),
                                          multiline: Multiline::No, })
            .add_quote_styles(&[("%Q{","}"),
                                ("%Q[","]"),
                                ("%Q(",")"),
                                ("%Q<",">"),
                                ("%Q/","/"),
                                ("%Q|","|"),
                                ("%Q!","!"),
                                ("%Q@","@"),
                                ("%Q#","#"),
                                ("%Q%","%"),
                                ("%Q,",","),
                                ("%Q_","_"),
                                ("%Q.","."),
                                ("%Q*","*"),
                                ("%Q^","^"),
                               ].into_iter().map(|(start, end)| {
                                   QuoteStyle{ rep: QuoteRep::Pair(start, end),
                                               escapes: ruby_percent_dq_escapes(end.chars().next().unwrap()),
                                               catenate: None,
                                               multiline: Multiline::No, }
                               }).collect())
            .add_quote_styles(&[("%q{","}"),
                                ("%q[","]"),
                                ("%q(",")"),
                                ("%q<",">"),
                                ("%q/","/"),
                                ("%q|","|"),
                                ("%q!","!"),
                                ("%q@","@"),
                                ("%q#","#"),
                                ("%q%","%"),
                                ("%q,",","),
                                ("%q_","_"),
                                ("%q.","."),
                                ("%q*","*"),
                                ("%q^","^"),
                               ].into_iter().map(|(start, end)| {
                                   QuoteStyle{ rep: QuoteRep::Pair(start, end),
                                               escapes: ruby_percent_sq_escapes(end.chars().next().unwrap()),
                                               catenate: None,
                                               multiline: Multiline::No, }
                               }).collect())
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::HereDoc("<<~{NAME}", "{NAME}"),
                                          escapes: EscapeTable::try_from(dq_here_esc).expect("dq_here_esc"),
                                          catenate: None,
                                          multiline: Multiline::Indent, })
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::HereDoc("<<~'{NAME}'", "{NAME}"),
                                          escapes: EscapeTable::try_from(sq_here_esc).expect("sq_here_esc"),
                                          catenate: None,
                                          multiline: Multiline::Indent, })
    }
}

#[cfg(test)]
mod test_sads {
    use super::*;
    use crate::ser_test;

    ser_test!(test_empty_map, r#"{}"#);

    ser_test!(test_simple_map1, r#"{ 'key' => 'value' }"#);
    ser_test!(test_simple_map2, r#"{ 'key' => 'value', 'key2' => 'value2' }"#);
    ser_test!(test_nested_maps,
r#"{
  'key' => {
    'nested' => {
      'wow' => {
        'how' => {
          'far' => {
            'can' => { 'we' => { 'go' => '!' } }
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
  'non-homogeneous',
  3,
  nil
]"#);

    ser_test!(test_arrays_of_arrays1,
r#"[
  [ 1, 2 ],
  [ 3, 4 ]
]"#);
    ser_test!(test_arrays_of_arrays2,
r#"[
  [
    [ 1, 2 ],
    [ 3, 4 ]
  ],
  [
    [ 5, 6 ],
    [ 7, 8 ]
  ]
]"#);
    ser_test!(test_arrays_of_arrays3,
r#"[
  [ 'a', 'b' ],
  [ 1, 2 ]
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
                    [ 1, 2 ],
                    [
                      3,
                      [
                        4,
                        [
                          5,
                          [ 6, nil ]
                        ]
                      ]
                    ],
                    'yep',
                    7
                  ]
                ]
              ]
            ]
          ],
          'more'
        ]
      ]
    ]
  ]
]"#);

    ser_test!(test_mixed1,
r#"{
  'array' => [ 1, 2, 3, 4 ]
}"#);
    ser_test!(test_mixed2,
r#"[
  { 'a' => [ 2, 3 ] },
  { 'b' => [ 3, 4 ] }
]"#);
    ser_test!(test_true, r#"true"#);
    ser_test!(test_false, r#"false"#);
    ser_test!(test_large,
r#"{
  'name' => 'babel',
  'version' => '7.20.15',
  'private' => true,
  'type' => 'commonjs',
  'scripts' => {
    'postinstall' => 'husky install',
    'bootstrap' => 'make bootstrap',
    'codesandbox:build' => 'make build-no-bundle',
    'build' => 'make build',
    'fix' => 'make fix',
    'lint' => 'make lint',
    'test' => 'make test',
    'version' => 'yarn --immutable-cache && git add yarn.lock',
    'test:esm' => 'node test/esm/index.js',
    'test:runtime:generate-absolute-runtime' => 'node test/runtime-integration/generate-absolute-runtime.cjs',
    'test:runtime:bundlers' => 'node test/runtime-integration/bundlers.cjs',
    'test:runtime:node' => 'node test/runtime-integration/node.cjs'
  },
  'packageManager' => 'yarn@3.2.4',
  'devDependencies' => {
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
    'babel-plugin-transform-charcodes' => '^0.2.0',
    'c8' => '^7.12.0',
    'chalk' => '^5.0.0',
    'charcodes' => '^0.2.0',
    'core-js' => '^3.26.0',
    'eslint' => '^8.22.0',
    'eslint-formatter-codeframe' => '^7.32.1',
    'eslint-import-resolver-node' => '^0.3.6',
    'eslint-plugin-import' => '^2.26.0',
    'eslint-plugin-jest' => '^27.1.5',
    'eslint-plugin-node' => '^11.1.0',
    'eslint-plugin-prettier' => '^4.2.1',
    'glob' => '^8.0.3',
    'gulp' => '^4.0.2',
    'gulp-filter' => '^7.0.0',
    'gulp-plumber' => '^1.2.1',
    'husky' => '^7.0.4',
    'import-meta-resolve' => '^1.1.1',
    'jest' => '^29.0.1',
    'jest-light-runner' => '^0.4.0',
    'jest-worker' => '^29.0.1',
    'lint-staged' => '^13.0.3',
    'mergeiterator' => '^1.4.4',
    'prettier' => '^2.7.1',
    'rollup' => '^2.78.0',
    'rollup-plugin-dts' => '^5.0.0',
    'rollup-plugin-polyfill-node' => '^0.10.2',
    'rollup-plugin-terser' => '^7.0.2',
    'semver' => '^6.3.0',
    'shelljs' => '^0.8.5',
    'test262-stream' => '^1.4.0',
    'through2' => '^4.0.0',
    'typescript' => '~4.9.3'
  },
  'workspaces' => [
    'codemods/*',
    'eslint/*',
    'packages/*',
    'test/esm',
    'test/runtime-integration/*',
    'benchmark'
  ],
  'resolutions' => {
    'browserslist' => 'npm:4.21.3',
    'caniuse-lite' => 'npm:1.0.30001397',
    'core-js-compat' => 'npm:3.25.1',
    'electron-to-chromium' => 'npm:1.4.248',
    'glob-watcher/chokidar' => 'npm:^3.4.0',
    '@types/babel__core' => 'link:./nope',
    '@types/babel__traverse' => 'link:./nope',
    '@babel/parser/@babel/types' => 'workspace:*',
    'babel-plugin-polyfill-corejs2/@babel/compat-data' => 'workspace:*'
  },
  'engines' => { 'yarn' => '>=1.4.0' },
  'lint-staged' => {
    '*.{js,cjs,mjs,ts}' => [ 'eslint --format=codeframe --cache --cache-strategy=content' ]
  },
  'dependenciesMeta' => {
    'core-js' => { 'built' => false },
    'core-js-pure' => { 'built' => false }
  },
  'changelog' => {
    'repo' => 'babel/babel',
    'cacheDir' => '.changelog',
    'labels' => {
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
    use super::RubyStringSerializer as StringSerializer;
    use crate::str_test;

    str_test!(test_control_characters,
r#"<<~END
\x00\x01\x02\x03\x04\x05\x06\a\b	
\v\f\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\e\x1c\x1d\x1e\x1f
END"#);
    str_test!(test_simple, r#"'Just a simple string.'"#);

    str_test!(test_interpolation, "", None, "Maybe I've got some #{interpolation}",
              r#"'Maybe I\'ve got some #{interpolation}'"#);

    str_test!(test_newlines, r#"<<~END
What about a few
embedded
newlines?
END"#);

    str_test!(test_newlines_lots,
r#"<<~END
What
about
a
whole
bunch
of
wonderful
embedded
newlines?
END"#);

    str_test!(test_quotes, r#"'"Don\'t quote me on that"'"#);

    str_test!(test_backticks, r#"'`Lots` `of` `back``ticks`'"#);

    str_test!(test_single_quotes, r#""'Lots' 'of' 'single' 'quotes'""#);

    str_test!(test_double_quotes, r#"'"Lots" "of" "double" "quotes"'"#);

    str_test!(test_source_code,
r#"<<~'SOME_CODE'

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


SOME_CODE"#);

    str_test!(test_gettysburg,
r#""Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived "\
"in Liberty, and dedicated to the proposition that all men are created equal.\n\n    Now we are "\
"engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, "\
"can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion "\
"of that field, as a final resting place for those who here gave their lives that that nation might "\
"live. It is altogether fitting and proper that we should do this.\n\n    But, in a larger sense, we "\
"can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, "\
"who struggled here, have consecrated it, far above our poor power to add or detract. The world will "\
"little note, nor long remember what we say here, but it can never forget what they did here. It is "\
"for us the living, rather, to be dedicated here to the unfinished work which they who fought here "\
"have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining "\
"before us—that from these honored dead we take increased devotion to that cause for which they gave "\
"the last full measure of devotion—that we here highly resolve that these dead shall not have died in "\
"vain—that this nation, under God, shall have a new birth of freedom—and that government of the "\
"people, by the people, for the people, shall not perish from the earth.\n""#);

    str_test!(test_gettysburg_indented,
               r#""Four score and seven years ago our fathers brought forth on this continent, a new "\
                  "nation, conceived in Liberty, and dedicated to the proposition that all men are "\
                  "created equal.\n\n    Now we are engaged in a great civil war, testing whether that "\
                  "nation, or any nation so conceived and so dedicated, can long endure. We are met "\
                  "on a great battle-field of that war. We have come to dedicate a portion of that "\
                  "field, as a final resting place for those who here gave their lives that that nation "\
                  "might live. It is altogether fitting and proper that we should do this.\n\n    "\
                  "But, in a larger sense, we can not dedicate—we can not consecrate—we can not ha"\
                  "llow—this ground. The brave men, living and dead, who struggled here, have consecrated "\
                  "it, far above our poor power to add or detract. The world will little note, nor "\
                  "long remember what we say here, but it can never forget what they did here. It is "\
                  "for us the living, rather, to be dedicated here to the unfinished work which they "\
                  "who fought here have thus far so nobly advanced. It is rather for us to be here "\
                  "dedicated to the great task remaining before us—that from these honored dead we take "\
                  "increased devotion to that cause for which they gave the last full measure of "\
                  "devotion—that we here highly resolve that these dead shall not have died in vain—that "\
                  "this nation, under God, shall have a new birth of freedom—and that government of "\
                  "the people, by the people, for the people, shall not perish from the earth.\n""#);

    str_test!(test_very_long_string,
r#"'Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_si'\
'ght_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_i'\
'n_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhe'\
're_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_an'\
'ywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_spac'\
'e_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_'\
'space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with'\
'_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_'\
'with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_l'\
'ong_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_ve'\
'ry_long_with_no_space_anywhere_in_sight_'"#);
}

#[test]
#[cfg(all(test,no))]
fn test_strings() {
    use crate::lang::string::test::*;
    let ruby_serializer = RubyStringSerializer::new();
    assert_eq!(ruby_serializer.serialize("\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f",
                                         "", None),
r#"<<~END
\x00\x01\x02\x03\x04\x05\x06\a\b	
\v\f\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\e\x1c\x1d\x1e\x1f
END"#
    );
    assert_eq!(ruby_serializer.serialize("Just a simple string.",
                                         "", None), "'Just a simple string.'");
    assert_eq!(ruby_serializer.serialize("Maybe I've got some #{interpolation}",
                                         "", None), r#"'Maybe I\'ve got some #{interpolation}'"#);
    assert_eq!(ruby_serializer.serialize("What about a few\nembedded\nnewlines?",
                                         "", None), r#"<<~END
What about a few
embedded
newlines?
END"#);
    assert_eq!(ruby_serializer.serialize("What\nabout\na\nwhole\nbunch\nof\nwonderful\nembedded\nnewlines?",
                                         "", None),
r#"<<~END
What
about
a
whole
bunch
of
wonderful
embedded
newlines?
END"#);

    assert_eq!(ruby_serializer.serialize("\"Don't quote me on that\"",
                                         "", None), r#"'"Don\'t quote me on that"'"#);
    assert_eq!(ruby_serializer.serialize("'Lots' 'of' 'single' 'quotes'",
                                         "", None), r#""'Lots' 'of' 'single' 'quotes'""#);
    assert_eq!(ruby_serializer.serialize(r#""Lots" "of" "double" "quotes""#,
                                         "", None), r#"'"Lots" "of" "double" "quotes"'"#);
    assert_eq!(ruby_serializer.serialize(source_code(), "", Some("some code")),
r#"<<~'SOME_CODE'

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


SOME_CODE"#);
    assert_eq!(ruby_serializer.serialize(gettysburg_address(),
                                         "", None),
r#""Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived "\
"in Liberty, and dedicated to the proposition that all men are created equal.\n\n    Now we are "\
"engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, "\
"can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion "\
"of that field, as a final resting place for those who here gave their lives that that nation might "\
"live. It is altogether fitting and proper that we should do this.\n\n    But, in a larger sense, we "\
"can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, "\
"who struggled here, have consecrated it, far above our poor power to add or detract. The world will "\
"little note, nor long remember what we say here, but it can never forget what they did here. It is "\
"for us the living, rather, to be dedicated here to the unfinished work which they who fought here "\
"have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining "\
"before us—that from these honored dead we take increased devotion to that cause for which they gave "\
"the last full measure of devotion—that we here highly resolve that these dead shall not have died in "\
"vain—that this nation, under God, shall have a new birth of freedom—and that government of the "\
"people, by the people, for the people, shall not perish from the earth.\n""#);
    assert_eq!(ruby_serializer.serialize(gettysburg_address(),
                                         "                  ", None),
               r#""Four score and seven years ago our fathers brought forth on this continent, a new "\
                  "nation, conceived in Liberty, and dedicated to the proposition that all men are "\
                  "created equal.\n\n    Now we are engaged in a great civil war, testing whether that "\
                  "nation, or any nation so conceived and so dedicated, can long endure. We are met "\
                  "on a great battle-field of that war. We have come to dedicate a portion of that "\
                  "field, as a final resting place for those who here gave their lives that that nation "\
                  "might live. It is altogether fitting and proper that we should do this.\n\n    "\
                  "But, in a larger sense, we can not dedicate—we can not consecrate—we can not ha"\
                  "llow—this ground. The brave men, living and dead, who struggled here, have consecrated "\
                  "it, far above our poor power to add or detract. The world will little note, nor "\
                  "long remember what we say here, but it can never forget what they did here. It is "\
                  "for us the living, rather, to be dedicated here to the unfinished work which they "\
                  "who fought here have thus far so nobly advanced. It is rather for us to be here "\
                  "dedicated to the great task remaining before us—that from these honored dead we take "\
                  "increased devotion to that cause for which they gave the last full measure of "\
                  "devotion—that we here highly resolve that these dead shall not have died in vain—that "\
                  "this nation, under God, shall have a new birth of freedom—and that government of "\
                  "the people, by the people, for the people, shall not perish from the earth.\n""#);
    assert_eq!(ruby_serializer.serialize(&"Something_very_long_with_no_space_anywhere_in_sight_".repeat(20),
                                         "", None),
r#"'Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_si'\
'ght_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_i'\
'n_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhe'\
're_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_an'\
'ywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_spac'\
'e_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_'\
'space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with'\
'_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_'\
'with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_l'\
'ong_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_ve'\
'ry_long_with_no_space_anywhere_in_sight_'"#);
}
