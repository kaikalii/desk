mod lex;
mod parse;

fn main() {
    let path = "examples/test.desk";
    let input = std::fs::read_to_string(path).unwrap();
    let tokens = lex::lex(path, &input).unwrap_or_else(|e| panic!("{e}"));
    for token in &tokens {
        println!("{:?}", token);
    }
}
