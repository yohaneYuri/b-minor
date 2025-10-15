use lalrpop_util::lalrpop_mod;

lalrpop_mod!(
    #[rustfmt::skip]
    #[allow(clippy::ptr_arg)]
    pub grammar
);

pub mod ast;
pub mod semantic;
