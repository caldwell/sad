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
            match (i.next_back(), i.next_back()) {
                ( Some(Opcode::Chunk(last)), Some(Opcode::Chunk(prev)) ) if last == ""
                    => match (prev.chars().next_back(), s.chars().next()) {
                           (Some(','), Some('{')) |
                           (Some(','), Some('[')) => true,
                           (_,_)                  => false,
                       },
                (_,_) => false,
            }
        };
        if collapse {
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



