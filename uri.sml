structure URI :> sig
  type uri

  structure Path : sig
    type path
    val fromString : string -> path
    val toString : path -> string
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
    type path = string

    fun fromString path = path
    fun toString path = path
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
                   ^ path
        in
          scheme ^ ":" ^ hierPart
          ^ (if Query.isEmpty query then "" else "?" ^ Query.toString query)
          ^ (case fragment of NONE => "" | SOME fragment => "#" ^ fragment)
        end
end
