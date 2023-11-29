#[derive(Debug)]
pub enum CompileError {
    UnexpectedToken(String),
    TypeError(String),
}