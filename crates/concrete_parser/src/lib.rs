use concrete_ast::Program;
use error::Diagnostics;
use lexer::Lexer;
use salsa::Accumulator;

pub mod db;
pub mod error;
mod lexer;
pub mod tokens;
pub use db::Db;

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
    pub path: String,
}

// Todo: better error handling
#[salsa::tracked]
pub fn parse_ast<'db>(db: &'db dyn crate::db::Db, source: ProgramSource<'db>) -> Option<Program> {
    let lexer = Lexer::new(source.input(db));
    let parser = grammar::ProgramParser::new();

    match parser.parse(lexer) {
        Ok(ast) => Some(ast),
        Err(e) => {
            Diagnostics(e).accumulate(db);
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{grammar, lexer::Lexer};

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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn parse_sum() {
        let source = r##"mod FactorialModule {
    pub fn add(x: u64, y: u64, z: u64) -> u64  {
        return x + y + z;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn parse_unary() {
        let source = r##"mod MyMod {
    pub fn myfunc() -> u64  {
        return 2 - -2;
    }
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn parse_empty_mod() {
        let source = r##"mod MyMod {
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn parse_empty_fn() {
        let source = r##"mod MyMod {
            fn hello() {}
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
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
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }

    #[test]
    fn parse_intrinsic() {
        let source = r##"mod MyMod {
    #[intrinsic = "simdsomething"]
    pub extern fn myintrinsic();
}"##;
        let lexer = Lexer::new(source);
        let parser = grammar::ProgramParser::new();
        parser.parse(lexer).unwrap();
    }
}
