// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::fmt::{self, Display};
use serde::{ser, Serialize};
use std::collections::VecDeque;

use crate::lang::string::StringSerializer;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Message(String),
}

impl ser::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error::Message(msg.to_string())
    }
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Message(msg) => formatter.write_str(msg),
        }
    }
}

pub const HEURISTIC_LINE_LEN_LIMIT: usize = 100;  // How long can a line get before it becomes annoying?
pub const HEURISTIC_ITEM_COUNT_LIMIT: usize = 4;  // Hom many short things can a map/array have before they become noise?
pub const HEURISTIC_ITEM_DEPTH_LIMIT: usize = 1;  // How many nested structures should fit on a line?

#[derive(Debug)]
enum Opcode {
    Chunk(String),
    In,
    Out,
    Sep,
}

#[derive(Debug, PartialEq)]
enum RenderMode {
    Multiline,
    Singleline,
}

struct StringWithLineLen {
    str: String,
    linelen: usize,
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

pub struct Serializer {
    op: VecDeque<Opcode>,
    output: StringWithLineLen,
    prefix: String,
    indent: String,
    lang: Language,
}

pub struct ArrLit(pub &'static str, pub &'static str, pub &'static str);
impl ArrLit {
    pub fn start(&self) -> &'static str { self.0 }
    pub fn sep(&self)   -> &'static str { self.1 }
    pub fn end(&self)   -> &'static str { self.2 }
}

pub struct TupLit(pub &'static str, pub &'static str, pub &'static str);
impl TupLit {
    pub fn start(&self) -> &'static str { self.0 }
    pub fn sep(&self)   -> &'static str { self.1 }
    pub fn end(&self)   -> &'static str { self.2 }
}

pub struct MapLit(pub &'static str, pub &'static str, pub &'static str, pub &'static str);
impl MapLit {
    pub fn start(&self) -> &'static str { self.0 }
    pub fn keysep(&self)-> &'static str { self.1 }
    pub fn sep(&self)   -> &'static str { self.2 }
    pub fn end(&self)   -> &'static str { self.3 }
}

pub enum MapKey {
    /// Keys are required to be quoted (such as JSON)
    AlwaysQuote,
    /// Valid identifiers don't need quoting (such as ecmascript). Everything
    /// else will be quoted.
    NakedIdentifiers,
}

/// Configuration for how the separators (comma, typically) behave.
pub enum SepStyle {
    /// Put the separator (comma, typically) between each element in an array or each
    /// entry in a map.  Ex: `[1, 2, 3]`
    Between,
    /// Put the separator (comma, typically) after each element. Ex: `[1, 2, 3,]`
    Trailing,
}

pub struct Language {
    pub array_lit: ArrLit,
    pub tuple_lit: TupLit,
    pub map_lit:   MapLit,
    pub map_key:   MapKey,
    pub sep_style: SepStyle,
    pub true_lit:  &'static str,
    pub false_lit: &'static str,
    pub null_lit:  &'static str,
    pub strser:    StringSerializer<'static>,
}

impl Serializer {
    pub fn new(language: Language) -> Serializer {
        Serializer {
            op: VecDeque::new(),
            output: StringWithLineLen::new(),
            prefix: String::new(),
            indent: "  ".to_string(),
            lang: language,
        }
    }
    pub fn to_string<'a, T: Serialize>(&mut self, value: &'a T) -> Result<String> {
        self.newchunk();
        value.serialize(&mut *self)?;
        self.flush(None);
        #[cfg(feature="ser_debug")]
        for op in self.op.iter() {
            println!("{:?}", op);
        }
        Ok(std::mem::take(&mut self.output.str))
    }

    fn newline_and_indent(&mut self) {
        self.output += "\n";
        self.output += &self.prefix;
    }

    fn newline_and_indent_more(&mut self) {
        self.prefix += &self.indent;
        self.newline_and_indent();
    }

    fn newline_and_indent_less(&mut self) {
        self.prefix.truncate(self.prefix.len().saturating_sub(self.indent.len()));
        self.newline_and_indent();
    }

    fn flush(&mut self, _replace: Option<&str>) {
        let mut state = RenderMode::Multiline;
        let mut depth:i64 = 0;

        loop {
            #[cfg(feature="ser_debug")]
            println!("Next: depth={} {:?}", depth, self.op.front());
            match (&state, self.op.front()) {
                (_, None) => return,
                (RenderMode::Singleline, Some(Opcode::Sep)) => {
                    self.output += " ";
                },
                (RenderMode::Multiline, Some(Opcode::Sep)) => {
                    self.output += "\n";
                    self.output += &self.prefix;
                },
                (_, Some(Opcode::Chunk(data))) => {
                    self.output += data;
                },
                (_, Some(Opcode::In)) => {
                    if state == RenderMode::Singleline {
                        depth += 1;
                    } else {
                        state = RenderMode::Singleline;
                        let mut len = self.output.linelen;
                        let mut count = 0;
                        let mut max_depth = 0;
                        depth = 0;

                        for op in self.op.iter().skip(1) {
                            #[cfg(feature="ser_debug")]
                            println!("  Peek: len={}, count={}, depth={}, max_depth={}: {:?}", len, count, depth, max_depth, op);
                            match op {
                                Opcode::In  => { depth += 1; max_depth = std::cmp::max(depth, max_depth); },
                                Opcode::Out => { if depth == 0 { break } depth -= 1 },
                                Opcode::Sep => { len += 1 },
                                Opcode::Chunk(data) => {
                                    len += data.len();
                                    count += 1;
                                },
                            }
                            if len > HEURISTIC_LINE_LEN_LIMIT || count > HEURISTIC_ITEM_COUNT_LIMIT || depth > HEURISTIC_ITEM_DEPTH_LIMIT as i64 {
                                #[cfg(feature="ser_debug")]
                                println!("  !readability heuristic: len={}, count={}, depth={}, max_depth={}", len, count, depth, max_depth);
                                state = RenderMode::Multiline;
                                break;
                            }
                        }
                    }

                    if state == RenderMode::Multiline {
                        self.newline_and_indent_more();
                    } else {
                        self.output += " ";
                    }
                },
                (_, Some(Opcode::Out)) => {
                    if state == RenderMode::Multiline {
                        self.newline_and_indent_less();
                    } else {
                        self.output += " ";
                    }
                    if depth == 0 {
                        #[cfg(feature="ser_debug")]
                        println!("  *multiline mode: depth={}", depth);
                        state = RenderMode::Multiline;
                    }
                    depth -= 1;
                },

            }
            self.op.pop_front();
        }
    }

    fn indent(&mut self) {
        self.op.push_back(Opcode::In);
        self.newchunk();
    }
    fn outdent(&mut self) {
        self.op.push_back(Opcode::Out);
        self.newchunk();
    }
    fn newchunk(&mut self) {
        self.op.push_back(Opcode::Chunk(String::new()));
    }
    fn separator(&mut self) {
        self.op.push_back(Opcode::Sep);
    }
    fn out(&mut self, s: &str) {
        let collapse = {
            let mut i = self.op.iter();
            match (i.next_back(), i.next_back(), i.next_back()) {
                ( Some(Opcode::Chunk(last)), Some(Opcode::Sep), Some(Opcode::Chunk(prev)) ) if last == ""
                    => match (prev.chars().next_back(), s.chars().next()) {
                           (Some(','), Some('{')) |
                           (Some(','), Some('[')) => true,
                           (_,_)                  => false,
                       },
                (_,_,_) => false,
            }
        };
        if collapse {
            self.op.pop_back();
            self.op.pop_back();
        }

        let Some(Opcode::Chunk(ref mut out)) = self.op.back_mut() else { unreachable!() };
        if collapse {
            *out += " ";
        }
        *out += s;
    }

    fn ends_with(&self, s: &str) -> bool {
        let mut revop = self.op.iter().rev();
        let mut needle = s.chars().rev().peekable();

        loop {
            match revop.next() {
                None => return false, // exhausted all ops: not found!
                Some(Opcode::In) | Some(Opcode::Out) | Some(Opcode::Sep) => continue,
                Some(Opcode::Chunk(ref chunk)) => {
                    let mut haystack = chunk.chars().rev();
                    loop {
                        match (needle.peek(), haystack.next()) {
                            (Some(&n), Some(h)) if n == h => { needle.next(); continue },
                            (Some(_), Some(_))            => return false, // blatant mismatch
                            (None,    _      )            => return true,  // exhausted needle: found it!
                            (_   ,    None   )            => break,        // exhausted haystack: jump to next chunk
                        }
                    }
                }
            }
        }
    }
}

impl<'a> std::ops::AddAssign<&str> for Serializer {
    fn add_assign(&mut self, s: &str) {
        self.out(s);
    }
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();
    type Error = Error;
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_bool(self, v: bool) -> Result<()> {
        *self += if v { "true" } else { "false" };
        Ok(())
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        *self += &v.to_string();
        Ok(())
    }
    fn serialize_u64(self, v: u64) -> Result<()> {
        *self += &v.to_string();
        Ok(())
    }
    fn serialize_f64(self, v: f64) -> Result<()> {
        *self += &v.to_string();
        Ok(())
    }
    fn serialize_i8(self,  v: i8)  -> Result<()> {self.serialize_i64(i64::from(v))}
    fn serialize_i16(self, v: i16) -> Result<()> {self.serialize_i64(i64::from(v))}
    fn serialize_i32(self, v: i32) -> Result<()> {self.serialize_i64(i64::from(v))}
    fn serialize_u8(self,  v: u8)  -> Result<()> {self.serialize_u64(u64::from(v))}
    fn serialize_u16(self, v: u16) -> Result<()> {self.serialize_u64(u64::from(v))}
    fn serialize_u32(self, v: u32) -> Result<()> {self.serialize_u64(u64::from(v))}
    fn serialize_f32(self, v: f32) -> Result<()> {self.serialize_f64(f64::from(v))}

    fn serialize_char(self, v: char) -> Result<()> {
        self.serialize_str(&v.to_string())
    }
    fn serialize_str(self, v: &str) -> Result<()> {
        *self += &self.lang.strser.serialize(v, &self.prefix, None);
        Ok(())
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<()> {
        use serde::ser::SerializeSeq;
        let mut seq = self.serialize_seq(Some(v.len()))?;
        for byte in v {
            seq.serialize_element(byte)?;
        }
        seq.end()
    }
    fn serialize_none(self) -> Result<()> {
        self.serialize_unit()
    }
    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Result<()> {
        value.serialize(self)
    }
    fn serialize_unit(self) -> Result<()> {
        *self += "nil";
        Ok(())
    }
    fn serialize_unit_struct(self, name: &'static str) -> Result<()> {
        *self += ":";
        *self += name;
        Ok(())
    }
    fn serialize_unit_variant(self, name: &'static str, _variant_index: u32, variant: &'static str) -> Result<()> {
        // FIXME: probably need to make this err out if strs aren't valid ruby idents
        *self += name;
        *self += ".";
        *self += variant;
        Ok(())
    }
    fn serialize_newtype_struct<T: ?Sized + Serialize>(self, _name: &'static str, value: &T,) -> Result<()> {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: ?Sized + Serialize>(self, name: &'static str, _variant_index: u32, variant: &'static str, value: &T,) -> Result<()> {
        *self += name;
        *self += ".";
        *self += variant;
        *self += "(";
        value.serialize(&mut *self)?;
        *self += ")";
        Ok(())
    }
    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        *self += self.lang.array_lit.start();
        Ok(self)
    }
    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_struct(self, _name: &'static str, len: usize,) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_variant(self, name: &'static str, _variant_index: u32, variant: &'static str, _len: usize,) -> Result<Self::SerializeTupleVariant> {
        *self += name;
        *self += ".";
        *self += variant;
        *self += "(";
        Ok(self)
    }
    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        *self += self.lang.map_lit.start();
        Ok(self)
    }
    fn serialize_struct(self, name: &'static str, _len: usize,) -> Result<Self::SerializeStruct> {
        *self += name;
        *self += ".new(";
        Ok(self)
    }
    fn serialize_struct_variant(self, name: &'static str, _variant_index: u32, variant: &'static str, _len: usize,) -> Result<Self::SerializeStructVariant> {
        *self += name;
        *self += ".";
        *self += variant;
        *self += "(";
        Ok(self)
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with(self.lang.array_lit.start()) {
            self.indent();
        } else {
            **self += self.lang.array_lit.sep();
            self.separator();
            self.newchunk();
        }
        value.serialize(&mut **self)
    }

    // Close the sequence.
    fn end(self) -> Result<()> {
        if !self.ends_with(self.lang.array_lit.start()) {
            if let SepStyle::Trailing = self.lang.sep_style {
                *self += self.lang.array_lit.sep();
            }
            self.outdent();
        }
        *self += self.lang.array_lit.end();
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with(self.lang.tuple_lit.start()) {
            self.indent();
        } else {
            **self += self.lang.tuple_lit.sep();
            self.separator();
            self.newchunk();
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        if !self.ends_with(self.lang.tuple_lit.start()) {
            if let SepStyle::Trailing = self.lang.sep_style {
                *self += self.lang.tuple_lit.sep();
            }
            self.outdent();
        }
        *self += self.lang.tuple_lit.end();
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with(self.lang.tuple_lit.start()) {
            self.indent();
        } else {
            **self += self.lang.tuple_lit.sep();
            self.separator();
            self.newchunk();
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.outdent();
        *self += self.lang.tuple_lit.end();
        Ok(())
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with("(") {
            self.indent();
        } else {
            **self += ",";
            self.separator();
            self.newchunk();
        }
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.outdent();
        *self += ")";
        Ok(())
    }
}


impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<()> {
        if self.ends_with(self.lang.map_lit.start()) {
            self.indent();
        } else {
            **self += self.lang.map_lit.sep();
            self.separator();
            self.newchunk();
        }
        key.serialize(&mut **self)
    }
    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        **self += self.lang.map_lit.keysep();
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        if !self.ends_with(self.lang.map_lit.start()) {
            if let SepStyle::Trailing = self.lang.sep_style {
                *self += self.lang.map_lit.sep();
            }
            self.outdent();
        }
        *self += self.lang.map_lit.end();
        Ok(())
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, key: &'static str, value: &T) -> Result<()> {
        if self.ends_with("(") {
            self.indent();
        } else {
            **self += ",";
            self.separator();
            self.newchunk();
        }
        **self += key;
        **self += ":";
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.outdent();
        *self += ")";
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, key: &'static str, value: &T) -> Result<()> {
        if self.ends_with("(") {
            self.indent();
        } else {
            **self += ",";
            self.separator();
            self.newchunk();
        }
        **self += key;
        **self += ":";
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.outdent();
        *self += ")";
        Ok(())
    }
}



#[cfg(test)]
pub mod test {
    #[macro_export]
    macro_rules! ser_test {
        ( $name:ident, $expect:expr ) => {
            #[test]
            fn $name() {
                let json = $crate::lang::serializer::test::$name;
                let data: serde_yaml::Value = serde_json::from_str(json).expect(&format!("bad json in test {}", stringify!($name)));
                let out = to_string(&data).expect("serialization failed");
                let expect = $expect;
                if (out != expect) {
                    let (ofile, oline, ocol) = $crate::lang::serializer::test::$name::position();
                    panic!("Serializer output did not match expected values\n\
                            Output:\n\
                            {out}\n\
                            {tfile}:{tline}:{tcol}:  Expected:\n\
                            {expect}\n\
                            {ofile}:{oline}:{ocol}:  Original JSON:\n\
                            {json}\n",
                           tfile=file!(), tline=line!(), tcol=column!());
                }
            }
        }
    }

    macro_rules! define_test_data {
        ( $name:ident, $str:expr ) => {
            #[allow(non_upper_case_globals)]
            pub const $name : &str = $str;
            pub mod $name {
                pub const fn position() -> (&'static str,u32,u32) { (file!(), line!(), column!()) }
            }
        }
    }

    define_test_data!(test_empty_map, r#"{}"#);
    define_test_data!(test_simple_map1, r#"{ "key": "value" }"#);
    define_test_data!(test_simple_map2, r#"{ "key": "value", "key2": "value2" }"#);
    define_test_data!(test_nested_maps, r#"{ "key": { "nested": { "wow": { "how": { "far": { "can": { "we": { "go": "!" } } } } } } } }"#);

    define_test_data!(test_empty_array, r#"[]"#);
    define_test_data!(test_simple_array1, r#"[4,5,6,7]"#);
    define_test_data!(test_non_homogeneous_array, r#"[1,2,"non-homogeneous",3,null]"#);

    define_test_data!(test_arrays_of_arrays1, r#"[ [1,2], [3,4] ]"#);
    define_test_data!(test_arrays_of_arrays2, r#"[[ [1,2], [3,4] ], [ [5,6], [7,8] ]]"#);
    define_test_data!(test_arrays_of_arrays3, r#"[ ["a","b"], [1,2] ]"#);
    define_test_data!(test_arrays_of_arrays4, r#"[[[[[[[[[[[1,2],[3,[4,[5,[6,null]]]],"yep",7]]]]],"more"]]]]]"#);

    define_test_data!(test_mixed1, r#"{"array":[1,2,3,4]}"#);
    define_test_data!(test_mixed2, r#"[{"a":[2,3]},{"b":[3,4]}]"#);
    define_test_data!(test_true, r#"true"#);
    define_test_data!(test_false, r#"false"#);
    define_test_data!(test_large, r#"{"name":"babel","version":"7.20.15","private":true,"type":"commonjs","scripts":{"postinstall":"husky install","bootstrap":"make bootstrap","codesandbox:build":"make build-no-bundle","build":"make build","fix":"make fix","lint":"make lint","test":"make test","version":"yarn --immutable-cache && git add yarn.lock","test:esm":"node test/esm/index.js","test:runtime:generate-absolute-runtime":"node test/runtime-integration/generate-absolute-runtime.cjs","test:runtime:bundlers":"node test/runtime-integration/bundlers.cjs","test:runtime:node":"node test/runtime-integration/node.cjs"},"packageManager":"yarn@3.2.4","devDependencies":{"@babel/cli":"^7.20.7","@babel/core":"^7.20.7","@babel/eslint-config-internal":"workspace:^","@babel/eslint-parser":"workspace:^","@babel/eslint-plugin-development":"workspace:^","@babel/eslint-plugin-development-internal":"workspace:^","@babel/plugin-proposal-dynamic-import":"^7.18.6","@babel/plugin-proposal-export-namespace-from":"^7.18.9","@babel/plugin-proposal-object-rest-spread":"^7.20.7","@babel/plugin-transform-modules-commonjs":"^7.20.11","@babel/plugin-transform-runtime":"^7.19.6","@babel/preset-env":"^7.20.2","@babel/preset-typescript":"^7.18.6","@babel/runtime":"^7.20.7","@rollup/plugin-babel":"^5.3.1","@rollup/plugin-commonjs":"^22.0.2","@rollup/plugin-json":"^4.1.0","@rollup/plugin-node-resolve":"^13.3.0","@rollup/plugin-replace":"^4.0.0","@types/node":"^18.11.7","@typescript-eslint/eslint-plugin":"^5.46.0","@typescript-eslint/parser":"^5.46.0","babel-plugin-transform-charcodes":"^0.2.0","c8":"^7.12.0","chalk":"^5.0.0","charcodes":"^0.2.0","core-js":"^3.26.0","eslint":"^8.22.0","eslint-formatter-codeframe":"^7.32.1","eslint-import-resolver-node":"^0.3.6","eslint-plugin-import":"^2.26.0","eslint-plugin-jest":"^27.1.5","eslint-plugin-node":"^11.1.0","eslint-plugin-prettier":"^4.2.1","glob":"^8.0.3","gulp":"^4.0.2","gulp-filter":"^7.0.0","gulp-plumber":"^1.2.1","husky":"^7.0.4","import-meta-resolve":"^1.1.1","jest":"^29.0.1","jest-light-runner":"^0.4.0","jest-worker":"^29.0.1","lint-staged":"^13.0.3","mergeiterator":"^1.4.4","prettier":"^2.7.1","rollup":"^2.78.0","rollup-plugin-dts":"^5.0.0","rollup-plugin-polyfill-node":"^0.10.2","rollup-plugin-terser":"^7.0.2","semver":"^6.3.0","shelljs":"^0.8.5","test262-stream":"^1.4.0","through2":"^4.0.0","typescript":"~4.9.3"},"workspaces":["codemods/*","eslint/*","packages/*","test/esm","test/runtime-integration/*","benchmark"],"resolutions":{"browserslist":"npm:4.21.3","caniuse-lite":"npm:1.0.30001397","core-js-compat":"npm:3.25.1","electron-to-chromium":"npm:1.4.248","glob-watcher/chokidar":"npm:^3.4.0","@types/babel__core":"link:./nope","@types/babel__traverse":"link:./nope","@babel/parser/@babel/types":"workspace:*","babel-plugin-polyfill-corejs2/@babel/compat-data":"workspace:*"},"engines":{"yarn":">=1.4.0"},"lint-staged":{"*.{js,cjs,mjs,ts}":["eslint --format=codeframe --cache --cache-strategy=content"]},"dependenciesMeta":{"core-js":{"built":false},"core-js-pure":{"built":false}},"changelog":{"repo":"babel/babel","cacheDir":".changelog","labels":{"PR: Spec Compliance :eyeglasses:":":eyeglasses: Spec Compliance","PR: Breaking Change :boom:":":boom: Breaking Change","PR: Deprecation: :loudspeaker:":":loudspeaker: Deprecation","PR: New Feature :rocket:":":rocket: New Feature","PR: Bug Fix :bug:":":bug: Bug Fix","PR: Polish :nail_care:":":nail_care: Polish","PR: Docs :memo:":":memo: Documentation","PR: Internal :house:":":house: Internal","PR: Performance :running_woman:":":running_woman: Performance","PR: Revert :leftwards_arrow_with_hook:":":leftwards_arrow_with_hook: Revert","PR: Output optimization :microscope:":":microscope: Output optimization"}}}"#);
}
