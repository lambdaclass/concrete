use std::path::PathBuf;

use crate::ast::CompileUnit;
use error::Diagnostics;
use lexer::Lexer;
use salsa::Accumulator;

pub mod error;
mod lexer;
pub mod tokens;

pub mod grammar {
    #![allow(dead_code, unused_imports, unused_variables)]

    pub use self::grammar::*;
    use lalrpop_util::lalrpop_mod;

    lalrpop_mod!(pub grammar);
}

#[salsa::interned]
pub struct ProgramSource<'db> {
    #[return_ref]
    pub input: String,
    #[return_ref]
    pub path: PathBuf,
}

// Todo: better error handling
#[salsa::tracked]
pub fn parse_ast<'db>(
    db: &'db dyn salsa::Database,
    source: ProgramSource<'db>,
) -> Option<CompileUnit> {
    let lexer = Lexer::new(source.input(db));
    let parser = grammar::CompileUnitParser::new();

    match parser.parse(source.path(db), lexer) {
        Ok(ast) => Some(ast),
        Err(e) => {
            Diagnostics(e).accumulate(db);
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use super::{grammar, lexer::Lexer};
    use crate::ast;

    #[test]
    fn parse_simple_program() {
        let source = r##"
mod ModuleName {
    import Std.io.{print};

    const MY_CONSTANT: u8 = 2;
    const MY_CONSTANT2: bool = true;
    const MY_CONSTANT3: string = "hello world!";

    pub fn my_func(hello: u64) -> u64 {
        let mut x: u64 = hello / 2;
        x = x + 4;
        x = x - 2;
        x = x % 2;

        match x {
            0 -> {
                return 2;
            },
            1 -> {
                let y: u64 = x * 2;
                return y * 10;
            },
        }

        if x == 2 {
            return 0;
        }

        let lol: u64 = if x == 3 {
            return 4;
        } else {
            return 5;
        };

        print("hello world\nwith newlines and \" escapes ");
        my_func((4 * 2) / 5 + 2 + 4 - (-5 - -6));

        while x > 0 {
            x = x - 1;
        }

        return x;
    }
}
        "##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_factorial() {
        let source = r##"mod FactorialModule {
    pub fn factorial(x: u64) -> u64  {
        return match x {
            0 -> {
                return 1;
            },
            n -> {
                return n * factorial(n-1);
            },
        };
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(&PathBuf::new(), lexer).unwrap();
    }

    #[test]
    fn parse_sum() {
        let source = r##"mod FactorialModule {
    pub fn add(x: u64, y: u64, z: u64) -> u64  {
        return x + y + z;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_unary() {
        let source = r##"mod MyMod {
    pub fn myfunc() -> u64  {
        return 2 - -2;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_empty_mod() {
        let source = r##"mod MyMod {
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_empty_fn() {
        let source = r##"mod MyMod {
            fn hello() {}
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_union_declaration() {
        let source = r##"mod MyMod {
    union Foo {
        bar: i32,
        baz: i64,
    }

    fn main() -> i32 {
        let mut foo: Foo = Foo { bar: 1 };

		// unsafe!
		let bar: i32 = foo.bar;
	}
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_for() {
        let source = r##"mod MyMod {
    fn hello() {
        let mut result: i64 = 0;

        for (let n: usize = 1; n <= limit; n = n + 1) {
            result = result + n;
        }

        return result;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_enum() {
        let source = r##"mod MyMod {
    enum Foo {
        Bar,
		Baz { n1: i32, n2: i32 },
		Qux = 3,
    }

    fn main() -> i32 {
        let mut foo: Foo = Foo.Bar;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_for_while() {
        let source = r##"mod MyMod {
    fn hello() {
        let mut result: i64 = 0;

        let n: i64 = 1;
        for (; n <= limit ;) {
            result = result + n;
            n = n + 1;
        }

        n = 1;
        for (n <= limit) {
            result = result + n;
            n = n + 1;
        }

        return result;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_for_loop() {
        let source = r##"mod MyMod {
    fn hello() {
        let mut result: i64 = 0;

        let n: i64 = 1;
        for (;;) {
            result = result + n;
            n = n + 1;
        }

        for {
            result = result + n;
            n = n + 1;
        }

        return result;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_array() {
        let source = r##"mod MyMod {
    fn hello() {
        let mut arr: [u32; 3] = [1, 2, 3];

        return arr[1];
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_nested_array() {
        let source = r##"mod MyMod {
    fn hello() {
        let mut arr: [[u32; 2]; 2] = [[1, 2], [3, 4]];

        return arr[1][0];
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_intrinsic() {
        let source = r##"mod MyMod {
    #[intrinsic = "simdsomething"]
    pub extern fn myintrinsic();
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_impl() {
        let source = r##"mod MyMod {
    struct A {
        a: i32,
        b: i32,
    }

    impl A {
        pub fn hello(&self, other: i32) -> i32 {
            return self.a * other;
        }
    }

    pub fn main() -> i32 {
        let x: A = A {
            a: 2,
            b: 3,
        };

        return x.hello(4);
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_impl_mut() {
        let source = r##"mod MyMod {
    struct A {
        a: i32,
        b: i32,
    }

    impl A {
        pub fn hello(&mut self, other: i32) -> i32 {
            return self.a * other;
        }
    }

    pub fn main() -> i32 {
        let x: A = A {
            a: 2,
            b: 3,
        };

        return x.hello(4);
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        parser.parse(Path::new(""), lexer).unwrap();
    }

    #[test]
    fn parse_doc_strings() {
        let source = r##"
/// Documentation for `MyMod`.
mod MyMod {
    /// The PI number.
    ///
    /// Note: Rounded to an integer.
    const ROUNDED_PI: u8 = 3;

    /// Increment a number.
    pub fn my_func(x: u64) -> u64 {
        return x + 1;
    }
}
"##;
        let lexer = Lexer::new(source);
        let parser = grammar::CompileUnitParser::new();
        let module = parser.parse(Path::new(""), lexer).unwrap();

        let const_item = match &module.modules[0].contents[0] {
            ast::modules::ModuleDefItem::Constant(x) => x,
            _ => unreachable!(),
        };
        let fn_item = match &module.modules[0].contents[1] {
            ast::modules::ModuleDefItem::Function(x) => x,
            _ => unreachable!(),
        };

        assert_eq!(
            module.modules[0].doc_string,
            Some(ast::common::DocString {
                contents: vec![" Documentation for `MyMod`.".to_string()],
                span: ast::common::Span::new(1, 31),
            }),
        );
        assert_eq!(
            const_item.decl.doc_string,
            Some(ast::common::DocString {
                contents: vec![
                    " The PI number.".to_string(),
                    "".to_string(),
                    " Note: Rounded to an integer.".to_string(),
                ],
                span: ast::common::Span::new(48, 111),
            }),
        );
        assert_eq!(
            fn_item.decl.doc_string,
            Some(ast::common::DocString {
                contents: vec![" Increment a number.".to_string()],
                span: ast::common::Span::new(147, 170),
            }),
        );
    }
}
