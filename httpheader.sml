structure HttpHeader :> sig
  type header

  val fromList : (string * string) list -> header
  val toList : header -> (string * string) list

  val contains : header * string -> bool
  val lookupAll : header * string -> string list

  val fromStream : TextIO.StreamIO.instream -> header * TextIO.StreamIO.instream

  val toString : header -> string
end = struct
  type header = (string * string) list

  fun fromList l = l
  fun toList h = h

  fun equalsIgnoringCase name (name', _) =
        String.map Char.toLower name = String.map Char.toLower name'

  fun contains (header, name) =
        List.exists (equalsIgnoringCase name) header

  fun lookupAll (header, name) =
        map #2 (List.filter (equalsIgnoringCase name) header)

  infix |>
  fun (x |> f) = f x

  fun trim s =
        s |> Substring.dropl (Char.isSpace) |> Substring.dropr (Char.isSpace)

  fun parseHeaderEntry line =
        let
          val (name, rest) = Substring.splitl (fn c => c <> #":") line
          val name = trim name
          val value = rest
            |> Substring.dropl (fn c => c = #":")
            |> trim
        in
          (Substring.string name, Substring.string value)
        end

  fun fromStream strm =
        let
          fun input (strm, acc) =
                case TextIO.StreamIO.inputLine strm of
                     NONE =>  (rev acc, strm)
                   | SOME (line, strm') =>
                       if line = "\r\n" orelse line = "\n" then
                         (rev acc, strm')
                       else
                         input (strm', parseHeaderEntry line::acc)
        in
          input (strm, [])
        end

  fun toString header =
        String.concatWith "\r\n" (map (fn (name, value) => name ^ ": " ^ value) header) ^ "\r\n"
end
