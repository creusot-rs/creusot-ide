open Lsp.Types
open Ppx_yojson_conv_lib.Yojson_conv

type test_item = {
  id: string;    (* Unique identifier *)
  label: string; (* Displayed to users *)
  range: Range.t;
} [@@deriving yojson_of]
