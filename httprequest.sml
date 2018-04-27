structure HttpRequest :> sig
  type request = {
    method: string,
    path : URI.Path.path,
    query : URI.Query.query,
    header: HttpHeader.header,
    messageBody: string
  }

  val toString : request -> string
end = struct
  type request = {
    method: string,
    path : URI.Path.path,
    query : URI.Query.query,
    header: HttpHeader.header,
    messageBody: string
  }

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
