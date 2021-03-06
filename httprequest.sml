structure HttpRequest :> sig
  type request = {
    method: string,
    path : URI.Path.path,
    query : URI.Query.query option,
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
    query : URI.Query.query option,
    header: HttpHeader.header,
    messageBody: string
  }

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

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
          URI.Path.fromString (Substring.string path) >>= (fn path =>
          (if Substring.size query = 0
           then SOME NONE
           else
            let
              val query = Substring.string (Substring.triml 1 query)
            in
              Option.map SOME (URI.Query.fromString query)
            end)
                                                      >>= (fn query =>
            SOME { method = Substring.string method,
                   path = path,
                   query = query }))
        end

  fun fromStream {inputLine, inputN, inputAll} strm =
        inputLine strm                        >>= (fn (line, strm') =>
        parseFirstLine line                   >>= (fn {method, path, query} =>
        HttpHeader.fromStream inputLine strm' >>= (fn (header, strm'') =>
        let
          val (messageBody, _) = inputAll strm''
        in
          SOME { method = method,
                 path = path,
                 query = query,
                 header = header,
                 messageBody = messageBody }
        end)))

  fun toString {method, path, query, header, messageBody} =
        let
          val requestTarget =
            URI.Path.toString path ^ (case query of
                                           NONE => ""
                                         | SOME query =>
                                             "?" ^ URI.Query.toString query)
        in
          method ^ " " ^ requestTarget ^ " " ^ "HTTP/1.1" ^ "\r\n" ^
          HttpHeader.toString header ^
          "\r\n" ^
          messageBody
        end
end
