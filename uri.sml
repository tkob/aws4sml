structure URI :> sig
  type uri

  structure Path : sig
    type segment
    type path = segment list
    val fromString : string -> path
    val toString : path -> string
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
  type authority = {
    userInfo : string option,
    host : string,
    port : int option
  }

  structure Path = struct
    type segment = string
    type path = segment list

    fun removeDotSegments input =
          let
                (* A and D *)
            fun aAndD (".."::input) = aAndD input
              | aAndD ("."::input) = aAndD input
              | aAndD input = input
            fun loop [] output = rev output
                (* B *)
              | loop ("."::[])    output = loop [] (""::output) (* /. *)
              | loop ("."::input) output = loop input output    (* /./ *)
                (* C *)
              | loop (".."::[])    []          = loop []      []           (* C /.. *)
              | loop (".."::[])    (""::[])    = loop []      (""::""::[]) (* C /.. *)
              | loop (".."::[])    (_::output) = loop []      (""::output) (* C /.. *)
              | loop (".."::input) []          = loop input   []           (* C /../ *)
              | loop (".."::input) (""::[])    = loop input   (""::[])     (* C /../ *)
              | loop (".."::input) (_::output) = loop input   output       (* C /../ *)
                (* E *)
              | loop (segment::input) output = loop input (segment::output)
          in
            loop (aAndD input) []
          end

    fun canonicalize path = removeDotSegments path

    fun isDelimiter #"/" = true
      | isDelimiter _ = false

    fun parseIpath s : path =
          if Substring.isEmpty s then []
          else map Substring.string (Substring.fields isDelimiter s)

    fun fromString s = parseIpath (Substring.full s)
    fun toString path = String.concatWith "/" path
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
