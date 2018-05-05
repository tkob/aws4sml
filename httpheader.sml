structure HttpHeader :> sig
  type header

  val fromList : (string * string) list -> header option
  val toList : header -> (string * string) list

  val contains : header * string -> bool
  val lookupAll : header * string -> string list
  val lookup : header * string -> string option

  val fromStream : ('strm -> (string * 'strm) option)
                   -> 'strm
                   -> (header * 'strm) option

  val toString : header -> string
end = struct
  type header = (string * string) list

  fun all p s = Substring.foldl (fn (c, b) => p c andalso b) true s

  fun isTchar #"!" = true
    | isTchar #"#" = true
    | isTchar #"$" = true
    | isTchar #"%" = true
    | isTchar #"&" = true
    | isTchar #"'" = true
    | isTchar #"*" = true
    | isTchar #"+" = true
    | isTchar #"-" = true
    | isTchar #"." = true
    | isTchar #"^" = true
    | isTchar #"_" = true
    | isTchar #"`" = true
    | isTchar #"|" = true
    | isTchar #"~" = true
    | isTchar c = Char.isDigit c orelse Char.isAlpha c
  fun isObsText c = Char.ord c >= 0x80 andalso Char.ord c <= 0xff
  fun isVchar c = Char.isGraph c orelse isObsText c

  fun isToken s = not (Substring.isEmpty s) andalso all isTchar s
  val isFieldName = isToken
  fun isFieldContent s =
        not (Substring.isEmpty s)
        andalso (isVchar (Substring.sub (s, 0)))
        andalso (isVchar (Substring.sub (s, Substring.size s - 1)))
        andalso all (fn c => isVchar c orelse c = #" " orelse c = #"\t") s

  fun fromList l =
        let
          fun isValidHeaderField (name, value) =
                isFieldName (Substring.full name)
                andalso isFieldContent (Substring.full value)
        in
          if List.all isValidHeaderField l then SOME l else NONE
        end

  fun toList h = h

  fun equalsIgnoringCase name (name', _) =
        String.map Char.toLower name = String.map Char.toLower name'

  fun contains (header, name) =
        List.exists (equalsIgnoringCase name) header

  fun lookupAll (header, name) =
        map #2 (List.filter (equalsIgnoringCase name) header)

  fun lookup (header, name) =
        case lookupAll (header, name) of
             [] => NONE
           | v::_ => SOME v

  infix |>
  fun (x |> f) = f x

  fun trim s =
        s |> Substring.dropl (Char.isSpace) |> Substring.dropr (Char.isSpace)

  fun parseHeaderField line =
        let
          val (name, rest) = Substring.splitl (fn c => c <> #":") line
          val name = trim name
          val value = rest
            |> Substring.dropl (fn c => c = #":")
            |> trim
        in
          if isFieldName name andalso isFieldContent value
          then SOME (Substring.string name, Substring.string value)
          else NONE
        end

  fun fromStream inputLine strm =
        let
          fun input (strm, acc) =
                case inputLine strm of
                     NONE => SOME (rev acc, strm)
                   | SOME (line, strm') =>
                       let
                         fun isNewLine c = c = #"\r" orelse c = #"\n"
                         val line = Substring.dropr isNewLine (Substring.full line)
                       in
                         if Substring.isEmpty line then
                           SOME (rev acc, strm')
                         else
                           case parseHeaderField line of
                                NONE => NONE
                              | SOME headerField =>
                                  input (strm', headerField::acc)
                       end
        in
          input (strm, [])
        end

  fun toString header =
        String.concatWith "\r\n" (map (fn (name, value) => name ^ ": " ^ value) header) ^ "\r\n"
end
