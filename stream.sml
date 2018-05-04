structure Stream :> sig
  type instream

  (* create a stream from mutable read function *)
  val fromFun : (unit -> string) -> instream

  val inputN : instream * int -> string * instream
  val inputLine : instream -> (string * instream) option
  val inputAll : instream -> string * instream
end = struct
  datatype instream = Nil
                    | Cons of { car : Substring.substring,
                                cdr :  instream option ref,
                                read : unit -> Substring.substring }

  val emptySlice = Substring.full ""

  fun fromFun read =
        (Cons { car = emptySlice,
                cdr = ref NONE,
                read = Substring.full o read })

  fun extend (Cons {car, cdr = cdr as (ref NONE), read}) =
        let
          val fragment = read ()
        in
          if Substring.size fragment = 0
          then 
            cdr := SOME Nil
          else
            cdr := SOME (Cons { car = fragment,
                                cdr = ref NONE,
                                read = read })
        end
    | extend (Cons {car, cdr = ref (SOME _), read}) = ()
    | extend (Nil) = ()

  val revcat = Substring.concat o rev

  fun inputN (strm, n) =
        let
          fun receive (strm, n, fragments) =
                case strm of
                     Nil => (revcat fragments, Nil)
                   | Cons {car, cdr, read} =>
                       if Substring.size car >= n then
                         let
                           val (ss, ss') = Substring.splitAt (car, n)
                         in
                           (revcat (ss::fragments), Cons { car = ss',
                                                           cdr = cdr,
                                                           read = read })
                         end
                       else (
                         extend strm;
                         case cdr of
                              ref (SOME next) =>
                                receive (next, n - Substring.size car, car::fragments)
                            | ref NONE =>
                                raise Fail "should never reach here")
        in
          receive (strm, n, [])
        end

  fun inputLine strm =
        let
          fun receive (strm, fragments) =
                case strm of
                     Nil =>
                       let
                         val line = revcat fragments
                       in
                         if String.size line = 0 then NONE
                         else SOME (line ^ "\n", Nil)
                       end
                   | Cons {car, cdr, read} =>
                       let
                         val (ls, rs) = Substring.splitl (fn c => c <> #"\n") car
                       in
                         if Substring.isEmpty rs then (
                           extend strm;
                           case cdr of
                                ref (SOME next) =>
                                  receive (next, ls::fragments)
                              | ref NONE =>
                                  raise Fail "should never reach here")
                         else
                           let
                             val line = revcat (ls::fragments) ^ "\n"
                             val rs' = Substring.triml 1 rs
                           in
                             SOME (line , Cons { car = rs',
                                                 cdr = cdr,
                                                 read = read })
                           end
                       end
        in
          receive (strm, [])
        end

  fun inputAll strm =
        let
          fun receive (strm, fragments) = (
                extend strm;
                case strm of
                     Nil => (revcat fragments, Nil)
                   | Cons {car, cdr = ref (SOME cdr), ...} =>
                       receive (cdr, car::fragments)
                   | Cons {cdr = ref NONE, ...} =>
                       raise Fail "should never reach here")
        in
          receive (strm, [])
        end
end
