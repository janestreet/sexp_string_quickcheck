open! Core
open! Import

type t = string [@@deriving sexp_of]

let does_parse s = Parsexp.Many.parse_string s |> Result.is_ok

let quickcheck_generator =
  Quickcheck.Generator.recursive_union
    [ [%quickcheck.generator: Atom_string.t] ]
    ~f:(fun self ->
      [ (let%map.Quickcheck.Generator subsexps = Quickcheck.Generator.list self in
         String.concat ~sep:" " (List.concat [ [ "(" ]; subsexps; [ ")" ] ]))
      ])
  |> Quickcheck.Generator.filter ~f:does_parse
;;

let quickcheck_shrinker =
  Quickcheck.Shrinker.filter [%quickcheck.shrinker: string] ~f:does_parse
;;

let quickcheck_observer = Quickcheck.Observer.of_hash (module String)

module Compare_sexps = struct
  type nonrec t = t

  let%template[@mode m = (local, global)] compare a b =
    (Comparable.lift [@mode m])
      ([%compare: Sexp.t list] [@mode.explicit m])
      ~f:(fun s -> Parsexp.Many.parse_string_exn s)
      a
      b
  ;;
end
