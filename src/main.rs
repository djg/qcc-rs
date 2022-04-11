use std::{
    cell::RefCell,
    io::{self, stdin},
    slice,
};

macro_rules! as_str {
    ($x:expr) => {
        unsafe { ::std::str::from_utf8_unchecked($x) }
    };
}

struct CookLit<'a> {
    iter: slice::Iter<'a, u8>,
}

impl Iterator for CookLit<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        let mut ch = *self.iter.next()?;
        if ch == b'\\' && self.iter.as_ref().get(0) == Some(&b'n') {
            self.iter.next();
            ch = b'\n';
        };
        Some(ch)
    }
}

fn cook(s: &[u8]) -> CookLit<'_> {
    CookLit { iter: s.iter() }
}

struct SymTab<'a> {
    symtab: Vec<&'a [u8]>,
}

impl<'a> SymTab<'a> {
    fn new() -> SymTab<'a> {
        SymTab { symtab: vec![] }
    }

    fn find(&mut self, s: &[u8]) -> Option<usize> {
        self.symtab.iter().position(|&sym| sym == s)
    }

    fn add<'b: 'a>(&mut self, s: &'b [u8]) -> usize {
        self.find(s).unwrap_or_else(|| {
            self.symtab.push(s);
            self.symtab.len() - 1
        })
    }

    fn str(&self, n: usize) -> &[u8] {
        self.symtab[n]
    }

    fn iter(&self) -> slice::Iter<'_, &'a [u8]> {
        self.symtab.iter()
    }
}

struct Context<'a> {
    symtab: RefCell<SymTab<'a>>,
    glo: RefCell<Vec<u8>>,
}

impl Default for Context<'_> {
    fn default() -> Self {
        Context {
            symtab: RefCell::new(SymTab::new()),
            glo: RefCell::new(Vec::with_capacity(0x1000)),
        }
    }
}

impl Context<'_> {
    const ALIGNMENT: [u8; 8] = [0; 8];
    fn add_global(&self, src: impl Iterator<Item = u8>) -> (usize, usize) {
        let mut glo = self.glo.borrow_mut();
        {
            let b = glo.len();
            glo.extend(src);
            let e = glo.len();
            glo.extend_from_slice(&Self::ALIGNMENT[(e & !0x8)..]);
            debug_assert_eq!(glo.len() & 0x7, 0);
            (b, e)
        }
    }
}

#[derive(Debug, PartialEq)]
enum Tok<'i> {
    Op(&'i [u8]),
    ILit(usize),
    SLit(usize, usize),
    Sym(usize),
}

struct Lexer<'i> {
    ctx: &'i Context<'i>,
    input: &'i [u8],
    pos: usize,
}

impl<'i> Lexer<'i> {
    fn bump(&mut self, count: usize) -> usize {
        let opos = self.pos;
        self.pos += count;
        opos
    }

    #[inline(always)]
    fn remaining(&self) -> &[u8] {
        &self.input[self.pos..]
    }

    fn peek_nth(&mut self, n: usize) -> Option<u8> {
        self.input.get(self.pos + n).copied()
    }

    fn getq(&mut self) -> Option<u8> {
        self.peek_nth(0).map(|ch| match ch {
            b'\\' if self.peek_nth(1) == Some(b'n') => {
                self.pos += 2;
                b'\n'
            }
            c => {
                self.pos += 1;
                c
            }
        })
    }

    fn split(&mut self, len: usize) -> &'i [u8] {
        let opos = self.bump(len);
        &self.input[opos..self.pos]
    }

    fn cconst(&mut self) -> Tok {
        self.pos += 1;
        let ch = self.getq().expect("syntax error");
        match self.peek_nth(0) {
            Some(b'\'') => {
                self.pos += 1;
            }
            _ => panic!("syntax error"),
        }
        Tok::ILit(ch as _)
    }

    fn id(&mut self) -> Tok {
        let len = self
            .remaining()
            .iter()
            .position(|&ch| !matches!(ch, b'a'..=b'z' | b'A'..=b'Z' | b'_'))
            .unwrap_or_else(|| self.remaining().len());
        let idx = self.ctx.symtab.borrow_mut().add(self.split(len));
        Tok::Sym(idx)
    }

    fn ilit(&mut self) -> Tok {
        let mut n = 0;
        while let Some(ch) = self.peek_nth(0) {
            match ch {
                b'0'..=b'9' => {
                    n = n * 10 + (ch - b'0') as usize;
                    self.pos += 1
                }
                _ => break,
            }
        }
        Tok::ILit(n)
    }

    fn slit(&mut self) -> Tok {
        self.pos += 1;
        let len = self
            .remaining()
            .iter()
            .position(|&ch| ch == b'"')
            .expect("unexpected EOF in string literal");
        self.pos += 1;

        let (b, e) = self.ctx.add_global(cook(self.split(len)));
        Tok::SLit(b, e)
    }

    fn op(&mut self) -> Tok {
        macro_rules! long_ops {
            () => (b"++" | b"--" | b"&&" | b"||" | b"==" | b"<=" | b">=" | b"!=" | b">>" | b"<<")
        }

        let len = match self.remaining().split_at(2) {
            (long_ops!(), _) => 2,
            _ => 1,
        };
        Tok::Op(self.split(len))
    }

    fn skip(&mut self) -> Option<u8> {
        while let Some(ch) = self.peek_nth(0) {
            match ch {
                b'\t' | b' ' | b'\r' | b'\n' => self.pos += 1,
                b'/' if Some(b'*') == self.peek_nth(1) => {
                    self.pos += 2;
                    self.com();
                }
                ch => return Some(ch),
            }
        }
        None
    }

    fn com(&mut self) {
        while let Some(ch) = self.peek_nth(0) {
            match ch {
                b'*' if Some(b'/') == self.peek_nth(1) => {
                    self.pos += 2;
                    break;
                }
                _ => self.pos += 1,
            }
        }
    }

    fn next(&mut self) -> Tok {
        if let Some(ch) = self.skip() {
            match ch {
                b'0'..=b'9' => self.ilit(),
                b'"' => self.slit(),
                b'\'' => self.cconst(),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.id(),
                _ => self.op(),
            }
        } else {
            Tok::Op(b"EOF")
        }
    }
}

fn lexer<'i>(ctx: &'i Context<'i>, input: &'i [u8]) -> Lexer<'i> {
    Lexer { ctx, input, pos: 0 }
}

fn read_input(mut input: impl io::Read) -> io::Result<Vec<u8>> {
    let mut v = vec![];
    input.read_to_end(&mut v)?;
    Ok(v)
}

fn pptok(mut lexer: Lexer) {
    use Tok::*;

    loop {
        match lexer.next() {
            Op(b"EOF") => {
                println!("End of input stream");
                return;
            }
            Op(s) => println!("Operator '{}'", as_str!(s)),
            ILit(n) => println!("Int literal {}", n),
            SLit(b, e) => println!("Str literal {:?}", as_str!(&lexer.ctx.glo.borrow()[b..=e])),
            Sym(idx) => println!(
                "Symbol '{}' ({})",
                as_str!(lexer.ctx.symtab.borrow().str(idx)),
                idx
            ),
        }
    }
}

fn pptoks() -> io::Result<()> {
    let ctx = Context::default();
    let input = read_input(stdin())?;
    let lexer = lexer(&ctx, &input);
    pptok(lexer);
    Ok(())
}

fn main() -> io::Result<()> {
    match std::env::args().nth(1).as_deref().unwrap_or("-blk") {
        "-lex" => pptoks()?,
        "-blk" => {}
        _ => todo!(),
    }
    Ok(())
}

#[cfg(test)]
mod test {
    use super::{lexer, Context, Tok};

    #[test]
    fn tokenize() {
        let ctx = Context::default();
        assert_eq!(lexer(&ctx, b"").next(), Tok::Op(b"EOF"));
        assert_eq!(lexer(&ctx, b"42").next(), Tok::ILit(42));
        assert_eq!(lexer(&ctx, b"'*'").next(), Tok::ILit(42));
        assert_eq!(lexer(&ctx, b"'\n'").next(), Tok::ILit(10));
        assert_eq!(lexer(&ctx, b"+").next(), Tok::Op(b"+"));
        assert_eq!(lexer(&ctx, b">>").next(), Tok::Op(b">>"));
        assert_eq!(lexer(&ctx, b">-").next(), Tok::Op(b">"));
        assert_eq!(lexer(&ctx, b"_abc").next(), Tok::Sym(0));
        assert_eq!(ctx.symtab.borrow().str(0), b"_abc");
        assert_eq!(lexer(&ctx, b"\"Hello, World!\"").next(), Tok::SLit(0, 13));
        assert_eq!(&ctx.glo.borrow()[0..14], b"Hello, World!\0");
    }
}
