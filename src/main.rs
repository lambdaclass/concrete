use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    Ok(concrete::driver::main()?)
}
