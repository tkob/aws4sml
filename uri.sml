(* RFC 3986 *)
structure URI :> sig
  type uri

  structure Path : sig
    type path

    val fromString : string -> path option
    val toString : path -> string
    val removeDotSegments : path -> path
    val canonicalize : path -> path
  end

  structure Query : sig
    type query

    val fromList : (string * string) list -> query
    val toList : query -> (string * string) list

    val isEmpty : query -> bool

    val fromString : string -> query option
    val toString : query -> string
  end

  val host : uri -> string option
  val toString : uri -> string

end = struct
    fun isUnreserved #"-" = true
      | isUnreserved #"." = true
      | isUnreserved #"_" = true
      | isUnreserved #"~" = true
      | isUnreserved c = Char.isAlpha c orelse Char.isDigit c

  fun percentEncode s =
        let
          fun encodeChar c =
                if isUnreserved c then
                  String.str c
                else
                  "%" ^ StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (Char.ord c))
        in
          String.concat (map encodeChar (explode s))
        end
  fun percentDecode s =
        let
          val s = Substring.full s
          fun isHex c =
            Char.isDigit c
            orelse (Char.ord c >= Char.ord #"A" andalso Char.ord c <= Char.ord #"F")
            orelse (Char.ord c >= Char.ord #"a" andalso Char.ord c <= Char.ord #"f")
          fun default (s, cs) =
                case Substring.getc s of
                     NONE => SOME (implode (rev cs))
                   | SOME (c, s') =>
                       if c = #"%" then
                         hexh (s', cs)
                       else
                         default (s', c::cs)
          and hexh (s, cs) =
                case Substring.getc s of
                     NONE => NONE
                   | SOME (c, s') =>
                       if isHex c then
                         hexl (s', c, cs)
                       else
                         NONE
          and hexl (s, hh, cs) =
                case Substring.getc s of
                     NONE => NONE
                   | SOME (c, s') =>
                       if isHex c then
                         default (s', valOf (Char.fromCString (implode [#"\\", #"x", hh, c]))::cs)
                       else
                         NONE
        in
          default (s, [])
        end

  type authority = {
    userInfo : string option,
    host : string,
    port : int option
  }

  structure Path = struct
    type path = string list

    fun isGenDelims #":" = true
      | isGenDelims #"/" = true
      | isGenDelims #"?" = true
      | isGenDelims #"#" = true
      | isGenDelims #"[" = true
      | isGenDelims #"]" = true
      | isGenDelims #"@" = true
      | isGenDelims _ = false

    fun isSubDelims #"!" = true
      | isSubDelims #"$" = true
      | isSubDelims #"&" = true
      | isSubDelims #"'" = true
      | isSubDelims #"(" = true
      | isSubDelims #")" = true
      | isSubDelims #"*" = true
      | isSubDelims #"+" = true
      | isSubDelims #"," = true
      | isSubDelims #";" = true
      | isSubDelims #"=" = true
      | isSubDelims _ = false

    fun isReserved c = isGenDelims c orelse isSubDelims c

    fun isDelimiter #"/" = true
      | isDelimiter _ = false

    fun parseIpath s =
          let
            val fields = map Substring.string (Substring.fields isDelimiter s)
            fun decode (field, NONE) = NONE
              | decode (field, SOME segments) =
                  case percentDecode field of
                       NONE => NONE
                     | SOME segment => SOME (segment::segments)
          in
            List.foldr decode (SOME []) fields
          end

    fun fromString s = parseIpath (Substring.full s)
    fun toString path = String.concatWith "/" (map percentEncode path)

    fun removeDotSegments input =
          let
            val inputBuffer = Substring.full (toString input)
            fun append suffix ss =
                  Substring.full (Substring.concat [ss, Substring.full suffix])
            fun prepend prefix ss =
                  Substring.full (Substring.concat [Substring.full prefix, ss])
            fun removeLastSegment [] = []
              | removeLastSegment (buffer as "/"::_) = buffer
              | removeLastSegment (_::"/"::buffer) = buffer
              | removeLastSegment (_::[]) = []
              | removeLastSegment (_::_::_) =
                  raise Fail "should never reach here"
            fun loop inputBuffer outputBuffer =
                  (* While the input buffer is not empty *)
                  if Substring.isEmpty inputBuffer
                  then valOf (fromString (concat (rev outputBuffer)))
                  else
                    (* A *)
                    if Substring.isPrefix "../" inputBuffer then
                      loop (Substring.triml 3 inputBuffer) outputBuffer
                    else if Substring.isPrefix "./" inputBuffer then
                      loop (Substring.triml 2 inputBuffer) outputBuffer
                    (* B *)
                    else if Substring.isPrefix "/./" inputBuffer then
                      loop (prepend "/" (Substring.triml 3 inputBuffer)) outputBuffer
                    else if Substring.string inputBuffer = "/." then
                      loop (Substring.full "/") outputBuffer
                    (* C *)
                    else if Substring.isPrefix "/../" inputBuffer then
                      loop
                        (prepend "/" (Substring.triml 4 inputBuffer))
                        (removeLastSegment outputBuffer)
                    else if Substring.string inputBuffer = "/.." then
                      loop
                        (Substring.full "/")
                        (removeLastSegment outputBuffer)
                    (* D *)
                    else if Substring.string inputBuffer = "." then
                      loop (Substring.full "") outputBuffer
                    else if Substring.string inputBuffer = ".." then
                      loop (Substring.full "") outputBuffer
                    (* E *)
                    else
                      let
                        val (inputBuffer, outputBuffer) =
                          if Substring.sub (inputBuffer, 0) = #"/" then
                            (Substring.triml 1 inputBuffer, "/"::outputBuffer)
                          else
                            (inputBuffer, outputBuffer)
                        val (firstSegment, inputBuffer) =
                          Substring.splitl (fn c => c <> #"/") inputBuffer
                      in
                        loop inputBuffer (Substring.string firstSegment::outputBuffer)
                      end
          in
            loop inputBuffer []
          end

    fun canonicalize path = removeDotSegments path

  end

  structure Query = struct
    type query = (string * string) list

    fun fromList l = l
    fun toList h = h

    val isEmpty = List.null

    fun fromString s =
          let
            val s = Substring.dropl (fn c => c = #"?") (Substring.full s)
            val parameters = Substring.tokens (fn c => c = #"&") s
            fun parseParameter param =
                  let
                    val (name, rest) = Substring.splitl (fn c => c <> #"=") param
                    val value = Substring.dropl (fn c => c = #"=") rest
                  in
                    (Substring.string name, Substring.string value)
                  end
          in
            SOME (map parseParameter parameters)
          end

    fun toString query =
          let
            fun parameterToString (name, value) =  name ^ "=" ^ value
          in
            String.concatWith "&" (map parameterToString query)
          end
  end

  type uri = {
    scheme : string,
    authority : authority option,
    path : Path.path,
    query : Query.query,
    fragment : string option
  }

  fun host ({authority = NONE, ...}: uri) = NONE
    | host ({authority = SOME {host, ...}, ...} : uri) = SOME host

  fun toString {scheme, authority, path, query, fragment} =
        let
          val hierPart =
            case authority of
                 NONE => ""
               | SOME {userInfo, host, port} =>
                   "//"
                   ^ (case userInfo of
                           NONE => ""
                         | SOME userInfo => userInfo ^ "@")
                   ^ host
                   ^ (case port of
                           NONE => ""
                         | SOME port => ":" ^ Int.toString port)
                   ^ Path.toString path
        in
          scheme ^ ":" ^ hierPart
          ^ (if Query.isEmpty query then "" else "?" ^ Query.toString query)
          ^ (case fragment of NONE => "" | SOME fragment => "#" ^ fragment)
        end
end
