structure HttpHeader :> sig
  type header

  val fromList : (string * string) list -> header
  val toList : header -> (string * string) list

  val contains : header * string -> bool

  val fromStream : TextIO.StreamIO.instream -> header * TextIO.StreamIO.instream

  val toString : header -> string
end = struct
  type header = (string * string) list

  fun fromList l = l
  fun toList h = h

  fun contains ([], _) = false
    | contains ((name', _)::entries, name) =
        if String.map Char.toLower name = String.map Char.toLower name'
        then true
        else contains (entries, name)

  fun parseHeaderEntry line =
        let
          fun isNewLine c = c = #"\r" orelse c = #"\n"
          val line = Substring.dropr isNewLine (Substring.full line)
          val (name, line') = Substring.splitl (fn c => c <> #":") line
          val value = Substring.dropl (fn c => c = #":" orelse Char.isSpace c) line'
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
