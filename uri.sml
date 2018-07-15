(* RFC 3986 *)
structure URI :> sig
  type uri

  structure Authority : sig
    type authority

    val fromString : string -> authority option
  end

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

    val fromString : string -> query option
    val toString : query -> string
  end

  structure Fragment : sig
    type fragment

    val mkFragment : string -> fragment
    val getString : fragment -> string

    val fromString : string -> fragment option
    val toString : fragment -> string
  end

  val scheme : uri -> string option
  val userInfo : uri -> string option
  val host : uri -> string option
  val port : uri -> int option
  val path : uri -> Path.path
  val query : uri -> Query.query option
  val fragment : uri -> Fragment.fragment option

  val fromString : string -> uri option
  val toString : uri -> string

end = struct
    infix >>=
    fun (SOME x) >>= k = k x
      | NONE     >>= k = NONE

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

  structure Authority = struct
    type authority = {
      userInfo : string option,
      host : string,
      port : int option
    }

    fun fromString s =
          let
            val s = Substring.full s
            val (userInfo, s) =
              if Substring.isSubstring "@" s then
                let
                  val (userInfo, s) =
                    Substring.splitl (fn c => c <> #"@") s
                in
                  (Option.map SOME (percentDecode (Substring.string userInfo)), Substring.triml 1 s)
                end
              else
                (SOME NONE, s)
            val (host, s) =
              let
                val (host, s) = Substring.splitl (fn c => c <> #":") s
              in
                (percentDecode (Substring.string host), s)
              end
            val port =
              if Substring.isEmpty s
              then SOME NONE
              else Option.map SOME (Int.fromString (Substring.string (Substring.triml 1 s)))
          in
            userInfo >>= (fn userInfo =>
            host     >>= (fn host =>
            port     >>= (fn port =>
            SOME {
              userInfo = userInfo,
              host = host,
              port = port })))
          end
  end

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
    type query = string (* percent-encoded representation *)

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
            val s = Substring.full s
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

    fun fromString s =
          (* just validate, not actually decode *)
          case percentDecode s of
               NONE => NONE
             | SOME _ => SOME s

    fun toString query = query
  end

  structure Fragment = struct
    type fragment = string

    fun mkFragment s = s
    fun getString fragment = fragment

    fun fromString s = percentDecode s
    fun toString fragment =
          let
            fun percentEncodeChar' c =
              if isUnreserved c orelse isSubDelims c
                  orelse c = #":" orelse c = #"@"
                  orelse c = #"/" orelse c = #"?" then
                String.str c
              else
                percentEncodeChar c
          in
            String.concat (map percentEncodeChar' (explode fragment))
          end
  end

  type uri = {
    scheme : string option,
    authority : Authority.authority option,
    path : Path.path,
    query : Query.query option,
    fragment : Fragment.fragment option
  }

  fun scheme (uri : uri) = #scheme uri

  fun userInfo ({authority = NONE, ...}: uri) = NONE
    | userInfo ({authority = SOME {userInfo, ...}, ...} : uri) = userInfo

  fun host ({authority = NONE, ...}: uri) = NONE
    | host ({authority = SOME {host, ...}, ...} : uri) = SOME host

  fun port ({authority = NONE, ...}: uri) = NONE
    | port ({authority = SOME {port, ...}, ...} : uri) = port

  fun path ({path = path, ...} : uri) = path

  fun query (uri : uri) = #query uri

  fun fragment (uri : uri) = #fragment uri

  fun fromString s =
        let
          val s = Substring.full s
          val (scheme, s') =
            let
              fun isSchemeChar c =
                    Char.isAlpha c orelse Char.isDigit c
                    orelse c = #"+" orelse c = #"-" orelse c = #"."
              val (scheme, s') = Substring.splitl isSchemeChar s
            in
              if (not (Substring.isEmpty scheme)) andalso
                 Char.isAlpha (Substring.sub (scheme, 0)) andalso
                 (not (Substring.isEmpty s')) andalso
                 Substring.sub (s', 0) = #":"
              then
                (SOME (Substring.string scheme), Substring.triml 1 s')
              else
                (NONE, s)
            end
          val (authority, s'') =
            if Substring.isPrefix "//" s'
            then
              let
                val (authority, s'') =
                  Substring.splitl (fn c => c <> #"/" andalso c <> #"?" andalso c <> #"#") (Substring.triml 2 s')
              in
                (Option.map SOME (Authority.fromString (Substring.string authority)), s'')
              end
            else
              (SOME NONE, s')
          val (path, s''') =
            Substring.splitl (fn c => c <> #"?" andalso c <> #"#") s''
          val path = Path.fromString (Substring.string path)
          val (query, fragment) =
              Substring.splitl (fn c => c <> #"#") s'''
          val query =
            if Substring.isEmpty query then SOME NONE
            else Option.map SOME (Query.fromString (Substring.string (Substring.triml 1 query)))
          val fragment =
            if Substring.isEmpty fragment then SOME NONE
            else Option.map SOME (Fragment.fromString (Substring.string (Substring.triml 1 fragment)))
        in
          authority >>= (fn authority =>
          path      >>= (fn path =>
          query     >>= (fn query =>
          fragment  >>= (fn fragment =>
          SOME {
            scheme = scheme,
            authority = authority,
            path = path,
            query = query,
            fragment = fragment }))))
        end

  fun toString {scheme, authority, path, query, fragment} =
        let
          val authority =
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
        in
          (case scheme of NONE => "" | SOME scheme => scheme ^ ":")
          ^ authority
          ^ Path.toString path
          ^ (case query of NONE => "" | SOME query => "?" ^ Query.toString query)
          ^ (case fragment of NONE => "" | SOME fragment => "#" ^ Fragment.toString fragment)
        end
end
