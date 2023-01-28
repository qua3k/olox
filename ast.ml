type literal = NIL | TRUE | FALSE | NUMBER of float | STRING of string

type expression =
  | ASSIGNMENT of {
      ident : literal;
      equal : Lexer.token_type;
      expr : expression;
    }
  | BINARY of {
      left : expression;
      operator : Lexer.token_type;
      right : expression;
    }
  | GROUPING of {
      left : Lexer.token_type;
      expr : expression;
      right : Lexer.token_type;
    }
  | LITERAL of literal
  | UNARY of { operator : Lexer.token_type; expr : expression }
