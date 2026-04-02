#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Function {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Function {
                param_types,
                return_type,
            } => {
                let params_str = param_types
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn({}) -> {}", params_str, return_type)
            }
        }
    }
}
