import Lean.Data.Json

namespace BlasterRpcApi.InvalidResultFixture

open Lean

private def hashPrefix := "d03abcb194238e97b78fde2ad23965150fdf72b156731759b60f7d493836adb"

private def steps : Json := Json.arr <| (#["a", "b", "c", "d", "e", "f", "g", "h"]).mapIdx fun index suffix =>
  let discarded : Bool := index >= 4
  let number := toString (index + 1)
  Json.mkObj [
    ("txHash", toJson (hashPrefix ++ suffix)),
    ("data", Json.mkObj (if discarded then [] else [
      ("datum", toJson ("datum" ++ number)),
      ("redeemer", toJson ("redeemer" ++ number)),
      ("variable1", toJson (number ++ number ++ number ++ number))])),
    ("discarded", toJson discarded)]

private def scriptAddress := "addr1wy8ccvgzslpjf9yhrprvmqulpmjpkpxf8c0hvtjwvw8n6pqdcrnp0"
private def walletAddress := "addr1qygsresrr4z5zggkwee8kz808ea3mzn7prhgfmralnqxvjvz57923as3zdhkgevjhq24qefkj074en0efej63ejhn2asau4xh2"

private def originalDatum := r#"{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"636d0d0118a8933ac167d4c448150bb325deaf7a4fdfb44adc7f2f5a"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"636d0d0118a8933ac167d4c448150bb325deaf7a4fdfb44adc7f2f5a"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"e39b5f40aa85fbc121a625d777a776eca1cb4c923426949c997d8828"}]}]}]}]},{"constructor":0,"fields":[]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"636d0d0118a8933ac167d4c448150bb325deaf7a4fdfb44adc7f2f5a"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[{"constructor":0,"fields":[{"bytes":"e39b5f40aa85fbc121a625d777a776eca1cb4c923426949c997d8828"}]}]}]}]},{"constructor":0,"fields":[]},{"constructor":0,"fields":[{"bytes":"f5808c2c990d86da54bfc97d89cee6efa20cd8461616359478d96b4c"},{"bytes":"b950921763ce10c2600579460a4a40ba0c286bfab6497acd09ae0cd8b2ef687d"}]},{"constructor":0,"fields":[{"constructor":0,"fields":[]},{"constructor":0,"fields":[{"int":"193007890"}]},{"int":"63970881068"},{"constructor":0,"fields":[]}]},{"int":"2000000"},{"constructor":1,"fields":[]}]}"#

private def utxo (hash : String) (index : Nat) (address : String) (consumed : Bool)
    (lovelace : Nat := 12507620) (assets : Array Json := #[]) (datum : Option String := none) : Json :=
  let reference := s!"{hash}#{index}"
  Json.mkObj ([
    ("type", toJson "utxo"), ("identifier", toJson reference), ("label", toJson s!"UTxO #{index}"),
    ("consumed", toJson consumed), ("address", toJson address), ("utxo", toJson reference),
    ("value", Json.mkObj [("lovelace", toJson lovelace), ("assets", Json.arr assets)])
  ] ++ datum.toList.map (fun value => ("datum", toJson value)))

private def transaction (hash : String) (index : Nat) (inputs outputs : Array Json) : Json :=
  Json.mkObj [
    ("tx", Json.mkObj [
      ("type", toJson "tx"), ("identifier", toJson hash), ("label", toJson s!"Transaction #{index}"),
      ("id", toJson hash), ("fee", toJson (1682177 : Nat)),
      ("inputCount", toJson inputs.size), ("outputCount", toJson outputs.size)]),
    ("inputs", Json.arr inputs), ("outputs", Json.arr outputs)]

def result : Json :=
  let firstHash := hashPrefix ++ "c"
  let secondHash := hashPrefix ++ "d"
  let firstInput := utxo "31596ecbdcf102c8e5c17e75c65cf9780996285879d18903f035964f3a7499a8" 0 scriptAddress true
    12507620 #[] (some originalDatum)
  let secondInput := utxo "26741de9ee337aa24ebeea078832a901c4deacce261e142a23186b1691513863" 1
    "addr1wywecz65rtwrqrqemhrtn7mrczl7x2c4pqc9hfjmsa3dc7cr5pvqw" true 1124910 #[Json.mkObj [
      ("name", toJson "BUIDLERFEST2026"), ("policyId", toJson "e1ddde8138579e255482791d9fba0778cb1f5c7b435be7b3e42069de"),
      ("quantity", toJson (1 : Nat))]] (some "foobar")
  let firstOutput := utxo firstHash 0 walletAddress true
  let secondOutput := utxo firstHash 1 walletAddress true
  let thirdOutput := utxo firstHash 2 walletAddress false
  let thirdInput := utxo "31596ecbdcf102c8e5c17e75c65cf9780996285879d18903f035964f3a7499a9" 3 scriptAddress true
  Json.mkObj [
    ("steps", steps),
    ("graph", Json.arr #[
      transaction firstHash 1 #[firstInput, secondInput] #[firstOutput, secondOutput, thirdOutput],
      transaction secondHash 2 #[firstOutput, secondOutput, thirdInput]
        #[utxo secondHash 0 scriptAddress false, utxo secondHash 1 scriptAddress false]])]

end BlasterRpcApi.InvalidResultFixture
