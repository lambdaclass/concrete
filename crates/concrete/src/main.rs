use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    concrete_driver::main()
}
