use compiler::{ast::NodeIdGenerator, grammar::ProgramParser};

fn main() {
    let src = "
        square: function integer (x: integer, y: array [] integer) = {
            if (1 > 2) {
                return 1;
            } else {
                return '2';
            }
        }
    ";

    let mut id_gen = NodeIdGenerator::new();
    let program = ProgramParser::new().parse(&mut id_gen, src).unwrap();
    println!("{:?}", program);

    program
        .semantic_check()
        .iter()
        .for_each(|e| eprintln!("{e}"));
}
