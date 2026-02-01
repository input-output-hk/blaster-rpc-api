import Lean.Data.Json

namespace AfvRpcApi

open Lean

structure Range where
  startLine : Nat
  startCharacter : Nat
  endLine : Nat
  endCharacter : Nat
  deriving ToJson, FromJson, Inhabited, Repr

structure Theorem where
  id : String
  label : String
  path : String
  range : Range
  deriving ToJson, FromJson, Inhabited, Repr

inductive TheoremStatus where
  | undetermined
  | running
  | valid
  | invalid

def TheoremStatus.toString (s : TheoremStatus) : String :=
  match s with
  | .undetermined => "undetermined"
  | .running => "running"
  | .valid => "valid"
  | .invalid => "invalid"

instance : ToJson TheoremStatus where
  toJson s := toJson s.toString

instance : FromJson TheoremStatus where
  fromJson? j := do
    let s <- fromJson? j
    match s with
    | "undetermined" => return .undetermined
    | "running" => return .running
    | "valid" => return .valid
    | "invalid" => return .invalid
    | _ => throw s!"Unknown theorem status: {s}"

instance : Inhabited TheoremStatus where
  default := .undetermined

instance : Repr TheoremStatus where
  reprPrec s _ := repr s.toString

structure TheoremResult where
  «theorem» : Theorem
  time : Option Nat
  status : TheoremStatus
  details : Json
  deriving ToJson, FromJson, Inhabited

instance : Repr TheoremResult where
  reprPrec r _ := s!"TheoremResult({r.«theorem».id}, {r.status.toString}, {r.time})"

inductive NodeType where
  | folder
  | file

def NodeType.toString (t : NodeType) : String :=
  match t with
  | .folder => "folder"
  | .file => "file"

instance : ToString NodeType where
  toString := NodeType.toString

instance : ToJson NodeType where
  toJson t := toJson t.toString

instance : FromJson NodeType where
  fromJson? j := do
    let s <- fromJson? j
    match s with
    | "folder" => return .folder
    | "file" => return .file
    | _ => throw s!"Unknown node type: {s}"

instance : Inhabited NodeType where
  default := .folder

instance : Repr NodeType where
  reprPrec t _ := repr t.toString

inductive Node where
  | folder (path : String) (label : String) (children : Array Node)
  | file (path : String) (label : String) (theorems : Array Theorem)

def Node.nodeType (n : Node) : NodeType :=
  match n with
  | .folder .. => .folder
  | .file .. => .file

def Node.label (n : Node) : String :=
  match n with
  | .folder _ label _ => label
  | .file _ label _ => label

def Node.path (n : Node) : String :=
  match n with
  | .folder path _ _ => path
  | .file path _ _ => path

mutual
  partial def Node.toJson (n : Node) : Json :=
    match n with
    | .folder path label children =>
      Json.mkObj [
        ("type", Lean.toJson NodeType.folder),
        ("path", Lean.toJson path),
        ("label", Lean.toJson label),
        ("children", Json.arr (children.map Node.toJson))
      ]
    | .file path label theorems =>
      Json.mkObj [
        ("type", Lean.toJson NodeType.file),
        ("path", Lean.toJson path),
        ("label", Lean.toJson label),
        ("theorems", Lean.toJson theorems)
      ]

  partial def Node.fromJson? (j : Json) : Except String Node := do
    let nodeType <- j.getObjValAs? NodeType "type"
    let path <- j.getObjValAs? String "path"
    let label <- j.getObjValAs? String "label"
    match nodeType with
    | .folder =>
      let childrenJson <- j.getObjValAs? (Array Json) "children"
      let children <- childrenJson.mapM Node.fromJson?
      return .folder path label children
    | .file =>
      let theorems <- j.getObjValAs? (Array Theorem) "theorems"
      return .file path label theorems
end

instance : ToJson Node where
  toJson := Node.toJson

instance : FromJson Node where
  fromJson? := Node.fromJson?

instance : Inhabited Node where
  default := .folder "" "" #[]

instance : Repr Node where
  reprPrec n _ := s!"Node({n.nodeType}, {n.label})"

structure Workspace where
  path : String
  label : String
  children : Array Node
  deriving ToJson, FromJson, Inhabited, Repr

structure WorkspaceParams where
  workspacePaths : Array String
  deriving ToJson, FromJson, Inhabited, Repr

end AfvRpcApi
