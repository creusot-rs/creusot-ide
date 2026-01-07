open Linol.Lsp.Types

type test_item = {
  id: string;    (* Unique identifier *)
  label: string; (* Displayed to users *)
  range: Range.t;
  } [@@deriving yojson_of]
