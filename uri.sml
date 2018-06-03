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

    fun isUnreserved #"-" = true
      | isUnreserved #"." = true
      | isUnreserved #"_" = true
      | isUnreserved #"~" = true
      | isUnreserved c = Char.isAlpha c orelse Char.isDigit c

  fun percentEncodeChar c =
        "%" ^ StringCvt.padLeft #"0" 2 (Int.fmt StringCvt.HEX (Char.ord c))

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
    datatype segment_or_slash = Segment of string | Slash
    type path = segment_or_slash list

    fun isDelimiter #"/" = true
      | isDelimiter _ = false

    fun fromString s =
          let
            val ss = Substring.full s
            fun consume (ss, path) =
                  if Substring.isEmpty ss then SOME (rev path)
                  else
                    if Substring.sub (ss, 0) = #"/" then
                      consume (Substring.triml 1 ss, Slash::path)
                    else
                      let
                        val (segment, ss') = Substring.splitl (fn c => c <> #"/") ss
                      in
                        case percentDecode (Substring.string segment) of
                             NONE => NONE
                           | SOME segment =>
                               consume (ss', Segment segment::path)
                      end
          in
            consume (ss, [])
          end

    fun toString path =
          let
            fun percentEncodeChar' c =
              if isUnreserved c orelse isSubDelims c orelse c = #":" orelse c = #"@" then
                String.str c
              else
                percentEncodeChar c
            fun percentEncode s =
                  String.concat (map percentEncodeChar' (explode s))
            fun encode Slash = "/"
              | encode (Segment segment) = percentEncode segment
          in
            String.concat (map encode path)
          end

    fun removeDotSegments input =
          let
            fun removeLastSegment [] = []
              | removeLastSegment (buffer as Slash::_) = buffer
              | removeLastSegment (Segment _::Slash::buffer) = buffer
              | removeLastSegment (Segment _::[]) = []
              | removeLastSegment (Segment _::Segment _::_) =
                  raise Fail "should never reach here"
            fun loop [] outputBuffer = rev outputBuffer
                (* A *)
              | loop (Segment ".."::Slash::inputBuffer) outputBuffer =
                  loop inputBuffer outputBuffer
              | loop (Segment "."::Slash::inputBuffer) outputBuffer =
                  loop inputBuffer outputBuffer
                (* B *)
              | loop (Slash::Segment "."::Slash::inputBuffer) outputBuffer =
                  loop (Slash::inputBuffer) outputBuffer
              | loop (Slash::Segment "."::inputBuffer) outputBuffer =
                  loop (Slash::inputBuffer) outputBuffer
                (* C *)
              | loop (Slash::Segment ".."::Slash::inputBuffer) outputBuffer =
                      loop (Slash::inputBuffer) (removeLastSegment outputBuffer)
              | loop (Slash::Segment ".."::inputBuffer) outputBuffer =
                      loop (Slash::inputBuffer) (removeLastSegment outputBuffer)
                (* D *)
              | loop (Segment "."::[]) outputBuffer =
                      loop [] outputBuffer
              | loop (Segment ".."::[]) outputBuffer =
                      loop [] outputBuffer
                (* E *)
              | loop (Segment segment::inputBuffer) outputBuffer =
                  loop inputBuffer (Segment segment::outputBuffer)
              | loop (Slash::Segment segment::inputBuffer) outputBuffer =
                  loop inputBuffer (Segment segment::Slash::outputBuffer)
              | loop (Slash::inputBuffer) outputBuffer =
                  loop inputBuffer (Slash::outputBuffer)
          in
            loop input []
          end

    fun canonicalize path = removeDotSegments path

  end

  structure Query = struct
    type query = string

    fun fromList l =
          let
            fun percentEncodeChar' c =
              if isUnreserved c orelse isSubDelims c
                  orelse c = #":" orelse c = #"@"
                  orelse c = #"/" orelse c = #"?" then
                String.str c
              else
                percentEncodeChar c
            fun percentEncode s =
                  String.concat (map percentEncodeChar' (explode s))
            fun parameterToString (name, value) =
                  percentEncode name ^ "=" ^ percentEncode value
          in
            String.concatWith "&" (map parameterToString l)
          end

    fun toList s =
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
            map parseParameter parameters
          end

    fun isEmpty query = String.size query = 0

    fun fromString s = SOME s

    fun toString query = query
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
