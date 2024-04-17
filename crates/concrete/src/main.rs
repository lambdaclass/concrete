use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    Ok(concrete_driver::main()?)
}
