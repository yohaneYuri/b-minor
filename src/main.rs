use compiler::grammar::ProgramParser;

fn main() {
    let src = "
        square: function integer (x: integer) = {
            if (1 > 2) {
                return 1;
            } else {
                return '2';
            }
        }
    ";

    ProgramParser::new()
        .parse(src)
        .unwrap()
        .semantic_check()
        .iter()
        .for_each(|e| eprintln!("{e}"));
}
