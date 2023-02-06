// Copyright © 2023 David Caldwell <david@porkrind.org>

use serde::Serialize;

use crate::lang::serializer::{Serializer,Language,ArrLit,MapLit,TupLit,MapKey,Result,SepStyle};

pub fn to_string<'a, 'b, T: Serialize>(value: &'a T) -> Result<String> {
    let mut serializer = Serializer::new(Language{array_lit:  ArrLit("[]interface{}{", "," ,"}"),
                                                  map_lit:    MapLit("map[string]interface{}{", ":", ",", "}" ),
                                                  tuple_lit:  TupLit("[]interface{}{", ",",  "}" ),
                                                  map_key:    MapKey::AlwaysQuote,
                                                  sep_style:  SepStyle::Trailing,
                                                  true_lit:  "true",
                                                  false_lit: "false",
                                                  null_lit:  "nil",
                                                  strser: GoStringSerializer::new()});
    serializer.to_string(value)
}

use crate::lang::string::*;

struct GoStringSerializer;

impl GoStringSerializer {
    fn new() -> StringSerializer<'static> {
        use array_lit::vec;
        let dq_esc = vec![Esc::No; 128; {
            [0]: [ Esc::Hex as u8; 32 ],
            (Ascii::BEL  as usize): Esc::Rep('a'),
            (Ascii::BS   as usize): Esc::Rep('b'),
            (Ascii::FF   as usize): Esc::Rep('f'),
            (Ascii::LF   as usize): Esc::Rep('n'),
            (Ascii::CR   as usize): Esc::Rep('r'),
            (Ascii::TAB  as usize): Esc::Rep('t'),
            (Ascii::VT   as usize): Esc::Rep('v'),
            ('"'         as usize): Esc::Rep('"'),
            ('\\'        as usize): Esc::Rep('\\'),
        }];

        let bt_esc = vec![Esc::No; 128; {
            [0]: [Esc::Ill as u8; 32],
            ('`' as usize): Esc::Ill,
        }];

        StringSerializer::new()
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("\""),
                                          escapes: EscapeTable::try_from(&dq_esc).expect("dq_esc"),
                                          catenate: Some("+"),
                                          multiline: Multiline::No, })
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("`"),
                                          escapes: EscapeTable::try_from(&bt_esc).expect("bt_esc"),
                                          catenate: Some("+"), // Not built in but we can fake it
                                          multiline: Multiline::No, })
    }
}


#[cfg(test)]
mod test_sads {
    use super::*;

    fn ser(json: &str) -> String {
        let data: serde_yaml::Value = serde_json::from_str(json).expect("bad json in test");
        let out = to_string(&data).expect("serialization failed");
        println!("out:\n{}\n---\n", out);
        out
    }

    #[test]
    fn test_maps() {
        assert_eq!(ser(r#"{}"#), r#"map[string]interface{}{}"#);
        assert_eq!(ser(r#"{ "key": "value" }"#), r#"map[string]interface{}{ "key":"value", }"#);
        assert_eq!(ser(r#"{ "key": "value", "key2": "value2" }"#), r#"map[string]interface{}{ "key":"value", "key2":"value2", }"#);
    }

    #[test]
    fn test_nested_maps() {
        assert_eq!(ser(r#"{ "key": { "nested": { "wow": { "how": { "far": { "can": { "we": { "go": "!" } } } } } } } }"#),
r#"map[string]interface{}{
  "key":map[string]interface{}{
    "nested":map[string]interface{}{
      "wow":map[string]interface{}{
        "how":map[string]interface{}{
          "far":map[string]interface{}{
            "can":map[string]interface{}{ "we":map[string]interface{}{ "go":"!", }, },
          },
        },
      },
    },
  },
}"#);
    }

    #[test]
    fn test_arrays() {
        assert_eq!(ser(r#"[]"#), "[]interface{}{}");
        assert_eq!(ser(r#"[4,5,6,7]"#), r#"[]interface{}{ 4, 5, 6, 7, }"#);
        assert_eq!(ser(r#"[1,2,"non-homogeneous",3,null]"#),
r#"[]interface{}{
  1,
  2,
  "non-homogeneous",
  3,
  nil,
}"#);
    }

    #[test]
    fn test_arrays_of_arrays() {
        assert_eq!(ser(r#"[ [1,2], [3,4] ]"#),
r#"[]interface{}{
  []interface{}{ 1, 2, },
  []interface{}{ 3, 4, },
}"#);
        assert_eq!(ser(r#"[ ["a","b"], [1,2] ]"#),
r#"[]interface{}{
  []interface{}{ "a", "b", },
  []interface{}{ 1, 2, },
}"#);
        assert_eq!(ser(r#"[[[[[[[[[[[1,2],[3,[4,[5,[6,null]]]],"yep",7]]]]],"more"]]]]]"#),
r#"[]interface{}{
  []interface{}{
    []interface{}{
      []interface{}{
        []interface{}{
          []interface{}{
            []interface{}{
              []interface{}{
                []interface{}{
                  []interface{}{
                    []interface{}{ 1, 2, },
                    []interface{}{
                      3,
                      []interface{}{
                        4,
                        []interface{}{
                          5,
                          []interface{}{ 6, nil, },
                        },
                      },
                    },
                    "yep",
                    7,
                  },
                },
              },
            },
          },
          "more",
        },
      },
    },
  },
}"#);
    }

    #[test]
    fn test_mixed() {
        assert_eq!(ser(r#"{"array":[1,2,3,4]}"#),
r#"map[string]interface{}{
  "array":[]interface{}{ 1, 2, 3, 4, },
}"#);
        assert_eq!(ser(r#"[{"a":[2,3]},{"b":[3,4]}]"#),
r#"[]interface{}{
  map[string]interface{}{ "a":[]interface{}{ 2, 3, }, },
  map[string]interface{}{ "b":[]interface{}{ 3, 4, }, },
}"#);
    }

    #[test]
    fn test_bool() {
        assert_eq!(ser(r#"true"#), r#"true"#);
        assert_eq!(ser(r#"false"#), r#"false"#);
    }

    #[test]
    fn test_large() {
        assert_eq!(ser(r#"{"name":"babel","version":"7.20.15","private":true,"type":"commonjs","scripts":{"postinstall":"husky install","bootstrap":"make bootstrap","codesandbox:build":"make build-no-bundle","build":"make build","fix":"make fix","lint":"make lint","test":"make test","version":"yarn --immutable-cache && git add yarn.lock","test:esm":"node test/esm/index.js","test:runtime:generate-absolute-runtime":"node test/runtime-integration/generate-absolute-runtime.cjs","test:runtime:bundlers":"node test/runtime-integration/bundlers.cjs","test:runtime:node":"node test/runtime-integration/node.cjs"},"packageManager":"yarn@3.2.4","devDependencies":{"@babel/cli":"^7.20.7","@babel/core":"^7.20.7","@babel/eslint-config-internal":"workspace:^","@babel/eslint-parser":"workspace:^","@babel/eslint-plugin-development":"workspace:^","@babel/eslint-plugin-development-internal":"workspace:^","@babel/plugin-proposal-dynamic-import":"^7.18.6","@babel/plugin-proposal-export-namespace-from":"^7.18.9","@babel/plugin-proposal-object-rest-spread":"^7.20.7","@babel/plugin-transform-modules-commonjs":"^7.20.11","@babel/plugin-transform-runtime":"^7.19.6","@babel/preset-env":"^7.20.2","@babel/preset-typescript":"^7.18.6","@babel/runtime":"^7.20.7","@rollup/plugin-babel":"^5.3.1","@rollup/plugin-commonjs":"^22.0.2","@rollup/plugin-json":"^4.1.0","@rollup/plugin-node-resolve":"^13.3.0","@rollup/plugin-replace":"^4.0.0","@types/node":"^18.11.7","@typescript-eslint/eslint-plugin":"^5.46.0","@typescript-eslint/parser":"^5.46.0","babel-plugin-transform-charcodes":"^0.2.0","c8":"^7.12.0","chalk":"^5.0.0","charcodes":"^0.2.0","core-js":"^3.26.0","eslint":"^8.22.0","eslint-formatter-codeframe":"^7.32.1","eslint-import-resolver-node":"^0.3.6","eslint-plugin-import":"^2.26.0","eslint-plugin-jest":"^27.1.5","eslint-plugin-node":"^11.1.0","eslint-plugin-prettier":"^4.2.1","glob":"^8.0.3","gulp":"^4.0.2","gulp-filter":"^7.0.0","gulp-plumber":"^1.2.1","husky":"^7.0.4","import-meta-resolve":"^1.1.1","jest":"^29.0.1","jest-light-runner":"^0.4.0","jest-worker":"^29.0.1","lint-staged":"^13.0.3","mergeiterator":"^1.4.4","prettier":"^2.7.1","rollup":"^2.78.0","rollup-plugin-dts":"^5.0.0","rollup-plugin-polyfill-node":"^0.10.2","rollup-plugin-terser":"^7.0.2","semver":"^6.3.0","shelljs":"^0.8.5","test262-stream":"^1.4.0","through2":"^4.0.0","typescript":"~4.9.3"},"workspaces":["codemods/*","eslint/*","packages/*","test/esm","test/runtime-integration/*","benchmark"],"resolutions":{"browserslist":"npm:4.21.3","caniuse-lite":"npm:1.0.30001397","core-js-compat":"npm:3.25.1","electron-to-chromium":"npm:1.4.248","glob-watcher/chokidar":"npm:^3.4.0","@types/babel__core":"link:./nope","@types/babel__traverse":"link:./nope","@babel/parser/@babel/types":"workspace:*","babel-plugin-polyfill-corejs2/@babel/compat-data":"workspace:*"},"engines":{"yarn":">=1.4.0"},"lint-staged":{"*.{js,cjs,mjs,ts}":["eslint --format=codeframe --cache --cache-strategy=content"]},"dependenciesMeta":{"core-js":{"built":false},"core-js-pure":{"built":false}},"changelog":{"repo":"babel/babel","cacheDir":".changelog","labels":{"PR: Spec Compliance :eyeglasses:":":eyeglasses: Spec Compliance","PR: Breaking Change :boom:":":boom: Breaking Change","PR: Deprecation: :loudspeaker:":":loudspeaker: Deprecation","PR: New Feature :rocket:":":rocket: New Feature","PR: Bug Fix :bug:":":bug: Bug Fix","PR: Polish :nail_care:":":nail_care: Polish","PR: Docs :memo:":":memo: Documentation","PR: Internal :house:":":house: Internal","PR: Performance :running_woman:":":running_woman: Performance","PR: Revert :leftwards_arrow_with_hook:":":leftwards_arrow_with_hook: Revert","PR: Output optimization :microscope:":":microscope: Output optimization"}}}"#),
r#"map[string]interface{}{
  "name":"babel",
  "version":"7.20.15",
  "private":true,
  "type":"commonjs",
  "scripts":map[string]interface{}{
    "postinstall":"husky install",
    "bootstrap":"make bootstrap",
    "codesandbox:build":"make build-no-bundle",
    "build":"make build",
    "fix":"make fix",
    "lint":"make lint",
    "test":"make test",
    "version":"yarn --immutable-cache && git add yarn.lock",
    "test:esm":"node test/esm/index.js",
    "test:runtime:generate-absolute-runtime":"node test/runtime-integration/generate-absolute-runtime.cjs",
    "test:runtime:bundlers":"node test/runtime-integration/bundlers.cjs",
    "test:runtime:node":"node test/runtime-integration/node.cjs",
  },
  "packageManager":"yarn@3.2.4",
  "devDependencies":map[string]interface{}{
    "@babel/cli":"^7.20.7",
    "@babel/core":"^7.20.7",
    "@babel/eslint-config-internal":"workspace:^",
    "@babel/eslint-parser":"workspace:^",
    "@babel/eslint-plugin-development":"workspace:^",
    "@babel/eslint-plugin-development-internal":"workspace:^",
    "@babel/plugin-proposal-dynamic-import":"^7.18.6",
    "@babel/plugin-proposal-export-namespace-from":"^7.18.9",
    "@babel/plugin-proposal-object-rest-spread":"^7.20.7",
    "@babel/plugin-transform-modules-commonjs":"^7.20.11",
    "@babel/plugin-transform-runtime":"^7.19.6",
    "@babel/preset-env":"^7.20.2",
    "@babel/preset-typescript":"^7.18.6",
    "@babel/runtime":"^7.20.7",
    "@rollup/plugin-babel":"^5.3.1",
    "@rollup/plugin-commonjs":"^22.0.2",
    "@rollup/plugin-json":"^4.1.0",
    "@rollup/plugin-node-resolve":"^13.3.0",
    "@rollup/plugin-replace":"^4.0.0",
    "@types/node":"^18.11.7",
    "@typescript-eslint/eslint-plugin":"^5.46.0",
    "@typescript-eslint/parser":"^5.46.0",
    "babel-plugin-transform-charcodes":"^0.2.0",
    "c8":"^7.12.0",
    "chalk":"^5.0.0",
    "charcodes":"^0.2.0",
    "core-js":"^3.26.0",
    "eslint":"^8.22.0",
    "eslint-formatter-codeframe":"^7.32.1",
    "eslint-import-resolver-node":"^0.3.6",
    "eslint-plugin-import":"^2.26.0",
    "eslint-plugin-jest":"^27.1.5",
    "eslint-plugin-node":"^11.1.0",
    "eslint-plugin-prettier":"^4.2.1",
    "glob":"^8.0.3",
    "gulp":"^4.0.2",
    "gulp-filter":"^7.0.0",
    "gulp-plumber":"^1.2.1",
    "husky":"^7.0.4",
    "import-meta-resolve":"^1.1.1",
    "jest":"^29.0.1",
    "jest-light-runner":"^0.4.0",
    "jest-worker":"^29.0.1",
    "lint-staged":"^13.0.3",
    "mergeiterator":"^1.4.4",
    "prettier":"^2.7.1",
    "rollup":"^2.78.0",
    "rollup-plugin-dts":"^5.0.0",
    "rollup-plugin-polyfill-node":"^0.10.2",
    "rollup-plugin-terser":"^7.0.2",
    "semver":"^6.3.0",
    "shelljs":"^0.8.5",
    "test262-stream":"^1.4.0",
    "through2":"^4.0.0",
    "typescript":"~4.9.3",
  },
  "workspaces":[]interface{}{
    "codemods/*",
    "eslint/*",
    "packages/*",
    "test/esm",
    "test/runtime-integration/*",
    "benchmark",
  },
  "resolutions":map[string]interface{}{
    "browserslist":"npm:4.21.3",
    "caniuse-lite":"npm:1.0.30001397",
    "core-js-compat":"npm:3.25.1",
    "electron-to-chromium":"npm:1.4.248",
    "glob-watcher/chokidar":"npm:^3.4.0",
    "@types/babel__core":"link:./nope",
    "@types/babel__traverse":"link:./nope",
    "@babel/parser/@babel/types":"workspace:*",
    "babel-plugin-polyfill-corejs2/@babel/compat-data":"workspace:*",
  },
  "engines":map[string]interface{}{ "yarn":">=1.4.0", },
  "lint-staged":map[string]interface{}{
    "*.{js,cjs,mjs,ts}":[]interface{}{ "eslint --format=codeframe --cache --cache-strategy=content", },
  },
  "dependenciesMeta":map[string]interface{}{
    "core-js":map[string]interface{}{ "built":false, },
    "core-js-pure":map[string]interface{}{ "built":false, },
  },
  "changelog":map[string]interface{}{
    "repo":"babel/babel",
    "cacheDir":".changelog",
    "labels":map[string]interface{}{
      "PR: Spec Compliance :eyeglasses:":":eyeglasses: Spec Compliance",
      "PR: Breaking Change :boom:":":boom: Breaking Change",
      "PR: Deprecation: :loudspeaker:":":loudspeaker: Deprecation",
      "PR: New Feature :rocket:":":rocket: New Feature",
      "PR: Bug Fix :bug:":":bug: Bug Fix",
      "PR: Polish :nail_care:":":nail_care: Polish",
      "PR: Docs :memo:":":memo: Documentation",
      "PR: Internal :house:":":house: Internal",
      "PR: Performance :running_woman:":":running_woman: Performance",
      "PR: Revert :leftwards_arrow_with_hook:":":leftwards_arrow_with_hook: Revert",
      "PR: Output optimization :microscope:":":microscope: Output optimization",
    },
  },
}"#)
    }
}

#[cfg(all(test,no))]
mod test_other {
    use super::*;
    #[test]
    fn test_enum() {
        #[derive(Serialize)]
        enum E {
            Unit,
            Newtype(u32),
            Tuple(u32, u32),
            Struct { a: u32 },
        }

        assert_eq!(to_string(&E::Unit            ).unwrap(), r#"E.Unit"#);
        assert_eq!(to_string(&E::Newtype(1)      ).unwrap(), r#"E.Newtype(1)"#);
        assert_eq!(to_string(&E::Tuple(1, 2)     ).unwrap(), r#"E.Tuple( 1, 2 )"#);
        assert_eq!(to_string(&E::Struct { a: 1 } ).unwrap(), r#"E.Struct( a:1 )"#);
    }

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
}

#[cfg(test)]
mod test_strings {
    use super::*;
    use crate::lang::string::test::*;
    fn ser(prefix: &str, name: Option<&str>, s: &str) -> String {
        let go_serializer = GoStringSerializer::new();
        let out = go_serializer.serialize(s,prefix,name);
        println!("out:\n{}\n---\n", out);
        out
    }
    #[test]
    fn test_control_characters() {
        assert_eq!(ser("", None, "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f"),
r#""\x00\x01\x02\x03\x04\x05\x06\a\b\t\n\v\f\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c"+
"\x1d\x1e\x1f""#
        );
    }
    #[test]
    fn test_simple() {
        assert_eq!(ser("", None, "Just a simple string."), r#""Just a simple string.""#);
    }
    #[test]
    fn test_newlines() {
        assert_eq!(ser("", None, "What about a few\nembedded\nnewlines?"), r#""What about a few\nembedded\nnewlines?""#);
    }
    #[test]
    fn test_newlines_lots() {
        assert_eq!(ser("", None, "What\nabout\na\nwhole\nbunch\nof\nwonderful\nembedded\nnewlines?"),
                   r#""What\nabout\na\nwhole\nbunch\nof\nwonderful\nembedded\nnewlines?""#);
    }
    #[test]
    fn test_quotes() {
        assert_eq!(ser("", None, "\"Don't quote me on that\"",), r#"`"Don't quote me on that"`"#);
    }
    #[test]
    fn test_single_quotes() {
        assert_eq!(ser("", None, "`Lots` `of` `single` `quotes`"), r#""`Lots` `of` `single` `quotes`""#);
    }
    #[test]
    fn test_double_quotes() {
        assert_eq!(ser("", None, r#""Lots" "of" "double" "quotes""#), r#"`"Lots" "of" "double" "quotes"`"#);
    }
    #[test]
    fn test_source_code() {
        assert_eq!(ser("", Some("some code"), source_code()),
r#""\n// What about a giant chunk of source code??\n#[derive(Debug, PartialEq)]\nenum RenderMode {\n    "+
"Multiline,\n    Singleline,\n}\n\nstruct StringWithLineLen {\n    pub str: String,\n    pub "+
"linelen: usize,\n}\n\nimpl StringWithLineLen {\n    fn new() -> StringWithLineLen {\n        "+
"StringWithLineLen { str: String::new(), linelen: 0 }\n    }\n}\n\nimpl std::ops::AddAssign<&str> for "+
"StringWithLineLen {\n    fn add_assign(&mut self, s: &str) {\n        self.str += s;\n        if s == \"\\n\" { "+
"// we only print newlines by themselves\n            self.linelen = 0;\n        } else {\n            "+
"self.linelen += s.len();\n        }\n    }\n}\n\n""#);
    }
    #[test]
    fn test_gettysburg() {
        assert_eq!(ser("", None, gettysburg_address()),
r#""Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived "+
"in Liberty, and dedicated to the proposition that all men are created equal.\n\n    Now we are "+
"engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, "+
"can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion "+
"of that field, as a final resting place for those who here gave their lives that that nation might "+
"live. It is altogether fitting and proper that we should do this.\n\n    But, in a larger sense, we "+
"can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, "+
"who struggled here, have consecrated it, far above our poor power to add or detract. The world will "+
"little note, nor long remember what we say here, but it can never forget what they did here. It is "+
"for us the living, rather, to be dedicated here to the unfinished work which they who fought here "+
"have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining "+
"before us—that from these honored dead we take increased devotion to that cause for which they gave "+
"the last full measure of devotion—that we here highly resolve that these dead shall not have died in "+
"vain—that this nation, under God, shall have a new birth of freedom—and that government of the "+
"people, by the people, for the people, shall not perish from the earth.\n""#);
    }
    #[test]
    fn test_gettysburg_indented() {
        assert_eq!(ser("                  ", None, gettysburg_address()),
               r#""Four score and seven years ago our fathers brought forth on this continent, a new "+
                  "nation, conceived in Liberty, and dedicated to the proposition that all men are "+
                  "created equal.\n\n    Now we are engaged in a great civil war, testing whether that "+
                  "nation, or any nation so conceived and so dedicated, can long endure. We are met "+
                  "on a great battle-field of that war. We have come to dedicate a portion of that "+
                  "field, as a final resting place for those who here gave their lives that that nation "+
                  "might live. It is altogether fitting and proper that we should do this.\n\n    "+
                  "But, in a larger sense, we can not dedicate—we can not consecrate—we can not ha"+
                  "llow—this ground. The brave men, living and dead, who struggled here, have consecrated "+
                  "it, far above our poor power to add or detract. The world will little note, nor "+
                  "long remember what we say here, but it can never forget what they did here. It is "+
                  "for us the living, rather, to be dedicated here to the unfinished work which they "+
                  "who fought here have thus far so nobly advanced. It is rather for us to be here "+
                  "dedicated to the great task remaining before us—that from these honored dead we take "+
                  "increased devotion to that cause for which they gave the last full measure of "+
                  "devotion—that we here highly resolve that these dead shall not have died in vain—that "+
                  "this nation, under God, shall have a new birth of freedom—and that government of "+
                  "the people, by the people, for the people, shall not perish from the earth.\n""#);
    }
    #[test]
    fn test_very_long_string() {
        assert_eq!(ser("", None, &"Something_very_long_with_no_space_anywhere_in_sight_".repeat(20)),
r#""Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_si"+
"ght_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_i"+
"n_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhe"+
"re_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_an"+
"ywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_spac"+
"e_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with_no_"+
"space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_with"+
"_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_long_"+
"with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_very_l"+
"ong_with_no_space_anywhere_in_sight_Something_very_long_with_no_space_anywhere_in_sight_Something_ve"+
"ry_long_with_no_space_anywhere_in_sight_""#);
    }
}
