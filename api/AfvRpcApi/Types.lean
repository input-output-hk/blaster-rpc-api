import Lean.Data.Json

namespace AfvRpcApi

open Lean

structure Range where
  startLine : Nat
  startCharacter : Nat
  endLine : Nat
  endCharacter : Nat
  deriving ToJson, FromJson, Inhabited, Repr

structure Test where
  id : String
  label : String
  path : String
  range : Range
  deriving ToJson, FromJson, Inhabited, Repr

inductive TestStatus where
  | undetermined
  | running
  | valid
  | invalid

def TestStatus.toString (s : TestStatus) : String :=
  match s with
  | .undetermined => "undetermined"
  | .running => "running"
  | .valid => "valid"
  | .invalid => "invalid"

instance : ToJson TestStatus where
  toJson s := toJson s.toString

instance : FromJson TestStatus where
  fromJson? j := do
    let s <- fromJson? j
    match s with
    | "undetermined" => return .undetermined
    | "running" => return .running
    | "valid" => return .valid
    | "invalid" => return .invalid
    | _ => throw s!"Unknown test status: {s}"

instance : Inhabited TestStatus where
  default := .undetermined

instance : Repr TestStatus where
  reprPrec s _ := repr s.toString

structure TestResult where
  test : Test
  time : Option Nat
  status : TestStatus
  details : Json
  deriving ToJson, FromJson, Inhabited

instance : Repr TestResult where
  reprPrec r _ := s!"TestResult({r.test.id}, {r.status.toString}, {r.time})"

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
  | file (path : String) (label : String) (tests : Array Test)

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
    | .file path label tests =>
      Json.mkObj [
        ("type", Lean.toJson NodeType.file),
        ("path", Lean.toJson path),
        ("label", Lean.toJson label),
        ("tests", Lean.toJson tests)
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
      let tests <- j.getObjValAs? (Array Test) "tests"
      return .file path label tests
end

instance : ToJson Node where
  toJson := Node.toJson

instance : FromJson Node where
  fromJson? := Node.fromJson?

instance : Inhabited Node where
  default := .folder "" "" #[]

instance : Repr Node where
  reprPrec n _ := s!"Node({n.nodeType}, {n.label})"

structure TestTree where
  path : String
  label : String
  children : Array Node
  deriving ToJson, FromJson, Inhabited, Repr

structure TestTreeParams where
  testTreePaths : Array String
  deriving ToJson, FromJson, Inhabited, Repr

structure RunTestParams where
  testIds : Array String
  deriving ToJson, FromJson, Inhabited, Repr

structure GetTestResultParams where
  testId : String
  deriving ToJson, FromJson, Inhabited, Repr

end AfvRpcApi
