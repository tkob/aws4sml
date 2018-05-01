structure HttpRequest :> sig
  type request = {
    method: string,
    path : URI.Path.path,
    query : URI.Query.query,
    header: HttpHeader.header,
    messageBody: string
  }

  val fromStream :
        { inputLine: ('strm -> (string * 'strm) option),
          inputN: 'strm * int -> string * 'strm,
          inputAll: 'strm -> string * 'strm }
        -> 'strm -> request option

  val toString : request -> string
end = struct
  type request = {
    method: string,
    path : URI.Path.path,
    query : URI.Query.query,
    header: HttpHeader.header,
    messageBody: string
  }

  fun parseFirstLine line =
        let
          fun neg pred x = not (pred x)
          val line = Substring.full line
          val (method, line) = Substring.splitl (neg Char.isSpace) line
          val line = Substring.dropl Char.isSpace line
          val (path, line) =
            Substring.splitl (fn c => not (Char.isSpace c) andalso c <> #"?") line
          val (query, _) = Substring.splitl (neg Char.isSpace) line
        in
          case URI.Query.fromString (Substring.string query) of
               NONE => NONE
             | SOME query =>
                 SOME { method = Substring.string method,
                        path = URI.Path.fromString (Substring.string path),
                        query = query }
        end

  fun fromStream {inputLine, inputN, inputAll} strm =
        case inputLine strm of
             NONE => NONE
           | SOME (line, strm') =>
               case parseFirstLine line of
                    NONE => NONE
                  | SOME {method, path, query} =>
                      let
                        val (header, strm'') =
                          HttpHeader.fromStream inputLine strm'
                        val (messageBody, _) = inputAll strm''
                      in
                        SOME { method = method,
                               path = path,
                               query = query,
                               header = header,
                               messageBody = messageBody }
                      end

  fun toString {method, path, query, header, messageBody} =
        let
          val requestTarget =
            URI.Path.toString path ^ (if URI.Query.isEmpty query
                                      then ""
                                      else "?" ^ URI.Query.toString query)
        in
          method ^ " " ^ requestTarget ^ " " ^ "HTTP/1.1" ^ "\r\n" ^
          HttpHeader.toString header ^
          "\r\n" ^
          messageBody
        end
end
