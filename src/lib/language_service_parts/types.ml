type position = {
  line: int;
  character: int;
}

type range = {
  start_pos: position;
  end_pos: position;
}

type diagnostic_severity =
  | Error
  | Warning
  | Information

type diagnostic = {
  range: range;
  severity: diagnostic_severity;
  message: string;
  source: string;
}

type symbol_kind =
  | Function
  | Variable

type document_symbol = {
  name: string;
  kind: symbol_kind;
  range: range;
  selection_range: range;
}

type definition = {
  name: string;
  filename: string;
  range: range;
}

type rename_kind =
  | RenameValue
  | RenameConstructor

type rename_result = {
  range: range;
  filename: string;
  edits: range list;
  kind: rename_kind;
}

type hover_info = {
  range: range;
  contents: string;
}

type semantic_token_kind =
  | SemanticFunction
  | SemanticVariable
  | SemanticType

type semantic_token = {
  range: range;
  kind: semantic_token_kind;
  declaration: bool;
}

type completion_item = {
  label: string;
  kind: int;
  detail: string option;
  documentation: string option;
}

type completion_list = {
  items: completion_item list;
  is_incomplete: bool;
}

type completion_source =
  | CompletionLocal
  | CompletionTopLevel
  | CompletionBuiltin
  | CompletionConstructor

type completion_symbol = {
  kind: semantic_token_kind;
  range: range;
  detail: string option;
  documentation: string option;
  source: completion_source;
}

type analysis_result = {
  diagnostics: diagnostic list;
}

type parsed_typed_result = {
  typed_program: Ast.typed Ast.program;
  type_definitions: Type_env.typedefinition_env;
  diagnostics: diagnostic list;
}

type scoped_symbol = {
  kind: semantic_token_kind;
}

type definition_symbol = {
  filename: string;
  range: range;
}

let semantic_token_range (token : semantic_token) : range = token.range
let semantic_token_kind (token : semantic_token) : semantic_token_kind = token.kind
let semantic_token_is_declaration (token : semantic_token) : bool = token.declaration

let completion_empty_list = {
  items = [];
  is_incomplete = false;
}
