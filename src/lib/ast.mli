include module type of Ast_private.Ast_core
include module type of Ast_private.Ast_eq
include module type of Ast_private.Ast_pp

module Core = Ast_private.Ast_core
module Eq = Ast_private.Ast_eq
module Pp = Ast_private.Ast_pp
module Factory = Ast_private.Ast_factory