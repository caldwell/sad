// Copyright © 2023 David Caldwell <david@porkrind.org>

use crate::lang::serializer::HEURISTIC_LINE_LEN_LIMIT;


/// Escape character lookup table.
///
/// Each index corresponds to an ascii character. Unicode characters will all go through
/// unscathed.  The value at that index controls how the character is represented in the
/// string. The ['Esc'] type defines constants to use here.
///
/// # Example
///     use array_lit::vec; // The array_lit crate allows for compact declarations here
///     let sq_esc = vec![Esc::No; 128; {
///         [0]: [Esc::Ill as u8; 32],
///         ('\'' as usize): Esc::Rep('\''),
///         ('\\' as usize): Esc::Rep('\\'),
///     }];
///     let table = EscapeTable::try_from(sq_esc).expect("sq_esc");
///
/// This uses the array_lit crate's `arr!` macro to built a lookup table suitable for
/// single quoted Ruby strings:
///
/// 1. It presets the the whole array to [`Esc::No`], meaning every character is
///    represented as itself (verbatim).
///
/// 2. It then overrides the entries for the first 32 ascii characters (all the control
///    characters) to be [`Esc::Ill`], meaning they are not representable (since Ruby
///    single quoted strings only support `\'` and `\\` escapes).
///
/// 3. It then overrides the entries for `'` and `\` to be represented by themselves (ie
///    `\'` and `\\`). Note the backslash in the output is implied.
#[derive(Debug, Clone)]
pub struct EscapeTable {
    table: Vec<u8>,
}

impl std::ops::Index<usize> for EscapeTable {
    type Output = u8;
    fn index(&self, index: usize) -> &u8 { &self.table[index] }
}

impl TryFrom<&[u8]> for EscapeTable {
    type Error = String;
    fn try_from(slice: &[u8]) -> Result<Self, Self::Error> {
        EscapeTable::try_from(slice.to_vec())
    }
}

impl TryFrom<&Vec<u8>> for EscapeTable {
    type Error = String;
    fn try_from(vec: &Vec<u8>) -> Result<Self, Self::Error> {
        EscapeTable::try_from(&vec[..])
    }
}

impl TryFrom<Vec<u8>> for EscapeTable {
    type Error = String;
    fn try_from(vec: Vec<u8>) -> Result<Self, Self::Error> {
        if vec.len() != 128 { return Err(format!("EscapeTable length was {} and not 128", vec.len())); }
        Ok(EscapeTable { table: vec })
    }
}

/// Escape character lookup table constants. See [`EscapeTable`].
pub struct Esc;

#[allow(unused,non_upper_case_globals,non_snake_case)] // We want these to look like enums
impl Esc {
    /// "No Escape". The character at this index should be printed literally.
    pub const No:  u8 = 0;
    /// The character at this index should be escaped as hex digits (ie `\x0a`)
    pub const Hex: u8 = 1;
    /// The character at this index should be escaped as octal digits (ie `\001`)
    pub const Oct: u8 = 2;
    /// The character at this index is not representable. This is useful for control characters in a single
    /// quoted string, since they cannot be escaped and we don't want control characters going though
    /// verbatim. Hitting this will take a [`QuoteStyle`] out of the running.
    pub const Ill: u8 = 3;
    /// The character at the index should be escaped by calling the [`StringSerializer::custom_escape`]
    /// function.
    pub const Cus: u8 = 4;
    /// The character at this index should be escaped as a `\` followed by the ASCII
    /// character passed in. Unicode replacement characters are not supported.
    pub const fn Rep(c: char) -> u8 { if !c.is_ascii() { panic!("escape replacements must be ascii") }
                                      c as u8 }
}

/// ASCII name constants. These are usize for convenience in building escape character
/// lookup tables ([`EscapeTable`]).
pub struct Ascii;
#[allow(unused)]
impl Ascii {
    pub const NUL: usize = '\x00' as usize;
    pub const SOH: usize = '\x01' as usize;
    pub const STX: usize = '\x02' as usize;
    pub const ETX: usize = '\x03' as usize;
    pub const EOT: usize = '\x04' as usize;
    pub const ENQ: usize = '\x05' as usize;
    pub const ACK: usize = '\x06' as usize;
    pub const BEL: usize = '\x07' as usize;
    pub const BS:  usize = '\x08' as usize;
    pub const TAB: usize = '\x09' as usize;
    pub const LF:  usize = '\x0A' as usize;
    pub const VT:  usize = '\x0B' as usize;
    pub const FF:  usize = '\x0C' as usize;
    pub const CR:  usize = '\x0D' as usize;
    pub const SO:  usize = '\x0E' as usize;
    pub const SI:  usize = '\x0F' as usize;
    pub const DLE: usize = '\x10' as usize;
    pub const DC1: usize = '\x11' as usize;
    pub const DC2: usize = '\x12' as usize;
    pub const DC3: usize = '\x13' as usize;
    pub const DC4: usize = '\x14' as usize;
    pub const NAK: usize = '\x15' as usize;
    pub const SYN: usize = '\x16' as usize;
    pub const ETB: usize = '\x17' as usize;
    pub const CAN: usize = '\x18' as usize;
    pub const EM:  usize = '\x19' as usize;
    pub const SUB: usize = '\x1A' as usize;
    pub const ESC: usize = '\x1B' as usize;
    pub const FS:  usize = '\x1C' as usize;
    pub const GS:  usize = '\x1D' as usize;
    pub const RS:  usize = '\x1E' as usize;
    pub const US:  usize = '\x1F' as usize;
    pub const SPC: usize = '\x20' as usize;
}

#[derive(Debug, Clone)]
pub enum QuoteRep<'a> {
    Same(&'a str),             // Start and end are the same
    Pair(&'a str, &'a str),    // Start and end are different
    HereDoc(&'a str, &'a str), // Start and end are templates "{NAME}" gets replaced with a heredoc style all-uppercase word
    Bare,                      // Bare word string, with validator function
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Multiline {
    No,
    Yes,
    Indent,
}

#[derive(Debug)]
pub struct QuoteStyle<'a> {
    pub rep:       QuoteRep<'a>,
    pub escapes:   EscapeTable,
    pub catenate:  Option<&'a str>,
    pub multiline: Multiline,
}

#[derive(Default, Debug)]
struct Highest<T: Default, W: Default> {
    max: T,
    who: Option<W>,
}

#[derive(Default, Debug)]
struct Lowest<T: Default, W: Default> {
    min: T,
    who: Option<W>,
}

impl<T: Default + PartialOrd + Copy, W:Default> Highest<T,W> {
    fn set(&mut self, v: T, w: W) {
        if v > self.max {
            self.max = v;
            self.who = Some(w);
        }
    }
}

impl<T:Default + PartialOrd, W:Default> Lowest<T,W> {
    fn set(&mut self, v: T, w: W) {
        if v < self.min || self.who.is_none() {
            self.min = v;
            self.who = Some(w);
        }
    }
}

pub trait StringSerializer<'a> {
    fn quote_styles(&self, ) -> Vec<QuoteStyle<'a>>;
    fn valid_bare_str(&self, _s: &str) -> bool { true }
    fn custom_escape(&self, _c: char) -> Option<String> { None }

    fn serialize(&self, s: &str, prefix: &str, name: Option<&str>) -> String {
        #[cfg(feature="str_debug")] println!("serializing [name={:?}, prefix={:?}]: {:?}", name, prefix, s);

        #[derive(Default, Debug)]
        struct Stats {
            escapes:  usize,
            len:      usize,
            max_line: usize,
        }
        #[derive(Default, Debug)]
        struct Bests {
            escapes:       Lowest<usize, usize>,
            len:           Lowest<usize, usize>,
            max_line:      Lowest<usize, usize>,
            best_in_class: Highest<usize, usize>,
        }
        let mut bests = Bests::default();
        let heredoc_name = name.unwrap_or("END").to_ascii_uppercase().replace(|c:char| !c.is_ascii_uppercase(), "_").trim_start_matches('_').to_string();
        let styles = self.quote_styles();
        let stats: Vec<_> = styles.iter().enumerate().filter_map(|(i,quote)| {
            let mut stats = Stats::default();
            let (startlen, endlen) = match (&quote.rep, &quote.multiline) {
                (QuoteRep::Bare, _)                           => if self.valid_bare_str(s) { (0, 0) } else { return None },
                (QuoteRep::Same(s),       _)                  => (s.len(), s.len()),
                (QuoteRep::Pair(s,e),     _)                  => (s.len(), e.len()),
                (QuoteRep::HereDoc(_, _), Multiline::No)      => panic!("Makes no sense."),
                (QuoteRep::HereDoc(s, e), Multiline::Yes)     => (s.replace("{NAME}", &heredoc_name).len(), e.replace("{NAME}", &heredoc_name).len()),
                (QuoteRep::HereDoc(s, e), Multiline::Indent)  => (s.replace("{NAME}", &heredoc_name).len(), e.replace("{NAME}", &heredoc_name).len() + prefix.len()),
            };
            stats.len += startlen;
            let mut pos = prefix.len() + startlen; // Assume this is where we start. Should we pass it in??
            for c in s.chars() {
                stats.max_line = std::cmp::max(pos, stats.max_line);
                if pos > HEURISTIC_LINE_LEN_LIMIT {
                    if let Some(catenate) = quote.catenate {
                        stats.len += catenate.len() + prefix.len() + endlen + startlen;
                        pos = prefix.len()+startlen;
                    }
                }
                if c > '\x7f' { stats.len+=1; pos+=1; continue; } // Just let unicode slide
                match (quote.escapes[c as usize], &quote.multiline) {
                    (Esc::No, Multiline::No)     if c == '\n'            => { #[cfg(feature="str_debug")] println!("[{}] {:?} reject naked \\n in Multiline::No", i, c);
                                                                              return None; }
                    (Esc::No, Multiline::Yes) |
                    (Esc::No, Multiline::Indent) if c == '\n'            => { stats.len += 1 + prefix.len(); pos = prefix.len(); }
                    (Esc::No, Multiline::Yes) |
                    (Esc::No, Multiline::Indent) if c == '\t'            => { stats.len += 1; pos += 1; }
                    (Esc::No, _)                 if c.is_ascii_control() => { #[cfg(feature="str_debug")] println!("[{}] {:?} reject naked control char", i, c);
                                                                              return None; } // Don't ever serialize unescaped control characters
                    (Esc::No, _)                                         => { stats.len += 1; pos += 1; },
                    (Esc::Ill , _)                                       => { #[cfg(feature="str_debug")] println!("[{}] {:?} reject Ill", i, c);
                                                                              return None; }
                    (Esc::Hex , _) | (Esc::Oct , _)                      => { stats.len += 4; pos += 4; stats.escapes += 1; }
                    (Esc::Cus , _)                                       => { let Some(esc) = self.custom_escape(c) else { return None };
                                                                              stats.len += esc.len(); pos += esc.len(); stats.escapes += 1; }
                    (_        , _)                                       => { stats.len += 2; pos += 2; stats.escapes += 1; }
                }
            }
            pos += endlen;
            stats.len += endlen;
            stats.max_line = std::cmp::max(pos, stats.max_line);
            bests.escapes.set(stats.escapes, i);
            bests.len.set(stats.len, i);
            bests.max_line.set(stats.max_line, i);
            Some((i, stats))
        }).collect();

        if stats.len() == 0 { panic!("Truly unrepresentable string??? This shouldn't happen!") }

        #[cfg(feature="str_debug")]
        {
            println!("---");
            for (i, s) in stats.iter() {
                println!("stats for [{}]={:?}", i, s);
            }
        }

        for (i, _) in stats.iter() {
            bests.best_in_class.set(if bests.escapes.who.unwrap()  == *i {1} else {0} +
                                    if bests.max_line.who.unwrap() == *i {1} else {0} +
                                    if bests.len.who.unwrap()      == *i {1} else {0}, *i);
        }
        #[cfg(feature="str_debug")]
        println!("bests={:?}", bests);

        let stats = &stats[stats.binary_search_by_key(&bests.best_in_class.who.unwrap(), |&(i, _)| i).expect("This better work")].1;
        let quote = &styles[bests.best_in_class.who.unwrap()];

        let mut heredoc_str_start: String;
        let mut heredoc_str_end:   String;

        let (start, end) = match (&quote.rep, &quote.multiline) {
            (QuoteRep::Bare, _)      => ("",""),
            (QuoteRep::Same(s), _)  => (*s,*s),
            (QuoteRep::Pair(s,e), _) => (*s,*e),
            (QuoteRep::HereDoc(_, _), Multiline::No)      => panic!("Makes no sense."),
            (QuoteRep::HereDoc(s, e), Multiline::Yes)     => { heredoc_str_start = s.replace("{NAME}", &heredoc_name);
                                                               heredoc_str_start.push('\n');
                                                               heredoc_str_end = "\n".to_string();
                                                               heredoc_str_end.push_str(&e.replace("{NAME}", &heredoc_name));
                                                               (heredoc_str_start.as_str(), heredoc_str_end.as_str()) },
            (QuoteRep::HereDoc(s, e), Multiline::Indent)  => { heredoc_str_start = s.replace("{NAME}", &heredoc_name);
                                                               heredoc_str_start.push('\n');
                                                               heredoc_str_start.push_str(prefix);
                                                               heredoc_str_end = "\n".to_string();
                                                               heredoc_str_end.push_str(prefix);
                                                               heredoc_str_end.push_str(&e.replace("{NAME}", &heredoc_name));
                                                               (heredoc_str_start.as_str(), heredoc_str_end.as_str()) },
        };

        let mut rep = String::with_capacity(stats.len);
        let mut pos = prefix.len(); // Assume this is where we start. Should we pass it in??
        rep += start;
        match (&quote.rep, &quote.multiline) {
            (QuoteRep::HereDoc(_,_), Multiline::Yes)    => pos = 0,
            (QuoteRep::HereDoc(_,_), Multiline::Indent) => pos = prefix.len(),
            (_,                      _                ) => pos += start.len(),
        };

        // Try to keep everything < 100 cols, but don't make the text too skinny.
        let wrap_pos = std::cmp::max(60, HEURISTIC_LINE_LEN_LIMIT.saturating_sub(prefix.len())) + prefix.len();
        let word_wrap_max = (wrap_pos - prefix.len()) / 5; // Go back 20% looking to word wrap (12 chars at len 60, 20 at len 100)
        let mut wrapped_residue = String::with_capacity(word_wrap_max);

        for c in s.chars() {
            if pos > wrap_pos {
                if let Some(catenate) = quote.catenate {
                    wrapped_residue.clear();
                    if let Some((back, _)) = rep.chars().rev().enumerate().take(word_wrap_max).find(|(_,c)| *c==' ') {
                        wrapped_residue.push_str(rep.get(rep.len()-back..).unwrap());
                        rep.truncate(rep.len()-back);
                    }
                    rep.push_str(end);
                    rep += catenate;
                    rep.push('\n');
                    rep += prefix;
                    rep += start;
                    rep += &wrapped_residue;
                    pos = prefix.len() + start.len();
                }
            }
            if c > '\x7f' { rep.push(c); pos +=1; continue; } // Just let unicode slide
            match (quote.escapes[c as usize], &quote.multiline) {
                (Esc::No,  Multiline::Yes)    if c == '\n'            => { rep.push(c);
                                                                           pos = 0; }
                (Esc::No,  Multiline::Indent) if c == '\n'            => { rep.push(c);
                                                                           rep += prefix;
                                                                           pos = prefix.len(); }
                (Esc::No,  Multiline::Yes) |
                (Esc::No,  _)                                         => { rep.push(c); pos += 1; },
                (Esc::Hex, _)                                         => { rep += &format!(r"\x{:02x}", c as u8); pos += 4; }
                (Esc::Oct, _)                                         => { rep += &format!(r"\{:03o}",  c as u8); pos += 4; }
                (Esc::Cus, _)                                         => { let esc = self.custom_escape(c).unwrap(); // already checked, above
                                                                           rep += &esc; pos += esc.len(); }
                (replace,  _)                                         => { rep.push('\\');
                                                                           rep.push(replace as char); pos += 2; }
            }
        }

        rep += end;

        #[cfg(feature="str_debug")]
        println!(">>>> length {}\n{}\n>>>>\n", rep.len(), rep);
        rep
    }
}

#[cfg(test)]
pub mod test {


    #[macro_export]
    macro_rules! str_test {
        ($test_name:ident, $expect:expr) => {
            str_test!(@internal $test_name, $expect,
                      $crate::lang::string::test::$test_name::data(),
                      $crate::lang::string::test::$test_name::position());
        };

        ($test_name:ident, $prefix:expr, $key:expr, $orig:expr, $expect:expr) => {
            str_test!(@internal $test_name, $expect,
                      ($prefix.into(), $key.into(), $orig.into()),
                      (file!(), line!(), column!()));
        };

        (@internal $test_name:ident, $expect:expr, $data:expr, $opos:expr) => {
            #[test]
            fn $test_name() {
                $crate::lang::serializer::test::str_tester(|data: $crate::lang::string::test::TestDataType| {
                               let (prefix, key, orig) = data;
                               let serializer = TestStringSerializer{};
                    use $crate::lang::string::StringSerializer;
                               (serializer.serialize(&orig,prefix,key), orig)
                           },
                           $data,
                           $expect,
                           (file!(), line!(), column!()),
                           $opos,
                           "String Serializer")
            }
        };
    }


    pub type TestDataType=(&'static str, Option<&'static str>, String);
    use crate::define_test_data;

    define_test_data!(test_control_characters, "", None, "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f");
    define_test_data!(test_simple, "", None, "Just a simple string.");
    define_test_data!(test_newlines, "", None, "What about a few\nembedded\nnewlines?");
    define_test_data!(test_newlines_lots, "", None, "What\nabout\na\nwhole\nbunch\nof\nwonderful\nembedded\nnewlines?");
    define_test_data!(test_quotes, "", None, "\"Don't quote me on that\"");
    define_test_data!(test_backticks, "", None, "`Lots` `of` `back``ticks`");
    define_test_data!(test_single_quotes, "", None, "'Lots' 'of' 'single' 'quotes'");
    define_test_data!(test_double_quotes, "", None, r#""Lots" "of" "double" "quotes""#);
    define_test_data!(test_source_code, "", Some("some code"), r##"
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

"##);
    define_test_data!(test_gettysburg, "", None, r#"Four score and seven years ago our fathers brought forth on this continent, a new nation, conceived in Liberty, and dedicated to the proposition that all men are created equal.

    Now we are engaged in a great civil war, testing whether that nation, or any nation so conceived and so dedicated, can long endure. We are met on a great battle-field of that war. We have come to dedicate a portion of that field, as a final resting place for those who here gave their lives that that nation might live. It is altogether fitting and proper that we should do this.

    But, in a larger sense, we can not dedicate—we can not consecrate—we can not hallow—this ground. The brave men, living and dead, who struggled here, have consecrated it, far above our poor power to add or detract. The world will little note, nor long remember what we say here, but it can never forget what they did here. It is for us the living, rather, to be dedicated here to the unfinished work which they who fought here have thus far so nobly advanced. It is rather for us to be here dedicated to the great task remaining before us—that from these honored dead we take increased devotion to that cause for which they gave the last full measure of devotion—that we here highly resolve that these dead shall not have died in vain—that this nation, under God, shall have a new birth of freedom—and that government of the people, by the people, for the people, shall not perish from the earth.
"#);
    define_test_data!(test_gettysburg_indented, "                  ", None, crate::lang::string::test::test_gettysburg::data().2);
    define_test_data!(test_very_long_string, "", None, "Something_very_long_with_no_space_anywhere_in_sight_".repeat(20));
}