// Copyright © 2023 David Caldwell <david@porkrind.org>

use serde::Serialize;

use crate::lang::serializer::{Serializer,Language,ArrLit,MapLit,TupLit,MapKey,Result};


pub fn to_string<'a, 'b, T: Serialize>(value: &'a T) -> Result<String> {
    let mut serializer = Serializer::new(Language{array_lit:  ArrLit("[", "," ,"]"),
                                                  map_lit:    MapLit("{", " => ", ",", "}" ),
                                                  tuple_lit:  TupLit("[", ",",  "]" ),
                                                  map_key:    MapKey::AlwaysQuote,
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
