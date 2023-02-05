// Copyright © 2023 David Caldwell <david@porkrind.org>

use std::fmt::{self, Display};
use serde::{ser, Serialize};
use std::collections::VecDeque;

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

pub struct Serializer {
    op: VecDeque<Opcode>,
    output: StringWithLineLen,
    prefix: String,
    indent: String,
    strser: StringSerializer<'static>,
}

pub fn to_string<'a, 'b, T: Serialize>(value: &'a T) -> Result<String> {
    let mut serializer = Serializer {
        op: VecDeque::new(),
        output: StringWithLineLen::new(),
        prefix: String::new(),
        indent: "  ".to_string(),
        strser: RubyStringSerializer::new(),
    };
    serializer.newchunk();
    value.serialize(&mut serializer)?;
    serializer.flush(None);
    // for op in serializer.op {
    //     println!("{:?}", op);
    // }
    Ok(serializer.output.str)
}

impl Serializer {
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
            // println!("Next: depth={} {:?}", depth, self.op.front());
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
                            // println!("  Peek: len={}, count={}, depth={}, max_depth={}: {:?}", len, count, depth, max_depth, op);
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
                                // println!("  !readability heuristic: len={}, count={}, depth={}, max_depth={}", len, count, depth, max_depth);
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
                        // println!("  *multiline mode: depth={}", depth);
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
        let collapse =
            if let Some(Opcode::Chunk(chunk)) = self.op.back() { if chunk == "" {
                if let Some(Opcode::Chunk(chunk)) = self.op.get(self.op.len().saturating_sub(2)) {
                    match (chunk.chars().next_back(), s.chars().next()) {
                        (Some(','), Some('{')) |
                        (Some(','), Some('[')) => true,
                        (_,_)                  => false,
                    }
                } else { false }
            } else {false}} else {false};
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
        *self += &self.strser.serialize(v, &self.prefix, None);
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
        *self += "[";
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
        *self += "{";
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
        if self.ends_with("[") {
            self.indent();
        } else {
            **self += ",";
            self.separator();
            self.newchunk();
        }
        value.serialize(&mut **self)
    }

    // Close the sequence.
    fn end(self) -> Result<()> {
        self.outdent();
        *self += "]";
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with("[") {
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
        *self += "]";
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        if self.ends_with("[") {
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
        *self += "]";
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
        if self.ends_with("{") {
            self.indent();
        } else {
            **self += ",";
            self.separator();
            self.newchunk();
        }
        key.serialize(&mut **self)
    }
    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<()> {
        **self += " => ";
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.outdent();
        *self += "}";
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

use array_lit::arr;
use crate::string::*;
const RUBY_DQ: [u8; 128] = arr![Esc::No; 128; {
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

fn ruby_percent_dq_escapes(end: char) -> Vec<u8> {
    let mut esc = RUBY_DQ.to_vec();
    esc[end as usize] = end as u8;
    esc['"' as usize] = Esc::No;
    esc
}

const RUBY_DQ_HERE: [u8; 128] = arr![Esc::No; 128; {
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

const RUBY_SQ: [u8; 128] = arr![Esc::No; 128; {
    [0]: [Esc::Ill as u8; 32],
    ('\'' as usize): Esc::Rep('\''),
    ('\\' as usize): Esc::Rep('\\'),
}];

fn ruby_percent_sq_escapes(end: char) -> Vec<u8> {
    let mut esc = RUBY_SQ.to_vec();
    esc[end as usize] = end as u8;
    esc['\'' as usize] = Esc::No;
    esc
}

const RUBY_SQ_HERE: [u8; 128] = arr![Esc::No; 128; {
    [0]: [Esc::Ill as u8; 32],
    (Ascii::TAB  as usize): Esc::No,
    (Ascii::LF   as usize): Esc::No,
}];

struct RubyStringSerializer;

impl RubyStringSerializer {
    fn new() -> StringSerializer<'static> {
        StringSerializer::new()
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("'"),
                                          escapes: RUBY_SQ.to_vec(),
                                          catenate: Some("\\"),
                                          multiline: Multiline::No, })
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::Same("\""),
                                          escapes: RUBY_DQ.to_vec(),
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
                                          escapes: RUBY_DQ_HERE.to_vec(),
                                          catenate: None,
                                          multiline: Multiline::Indent, })
            .add_quote_style(&QuoteStyle{ rep: QuoteRep::HereDoc("<<~'{NAME}'", "{NAME}"),
                                          escapes: RUBY_SQ_HERE.to_vec(),
                                          catenate: None,
                                          multiline: Multiline::Indent, })
    }
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

#[test]
fn test_strings() {
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
    assert_eq!(ruby_serializer.serialize(r##"
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

"##,
                                         "", Some("some code")), r#"<<~'SOME_CODE'

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
    let gettysburg_address = r#"Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

    Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

    But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.
"#;
    assert_eq!(ruby_serializer.serialize(gettysburg_address,
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
    assert_eq!(ruby_serializer.serialize(gettysburg_address,
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
