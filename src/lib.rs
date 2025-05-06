pub mod interpret;
pub mod parser;
pub mod repl;
#[macro_use]
pub mod token;

// to support CodeCrafters output requirements, don't use miette fancy printing
pub fn error_print(e: &miette::Report) {
    #[cfg(feature = "pretty-print")]
    eprintln!("{:?}", e);
    #[cfg(not(feature = "pretty-print"))]
    println!("{}", e);
}
