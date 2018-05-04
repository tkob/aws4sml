structure HttpResponse :> sig
  type response = {
    status: string,
    responsePhrase: string,
    header: HttpHeader.header,
    messageBody: string
  }

  val fromStream :
        { inputLine: ('strm -> (string * 'strm) option),
          inputN: 'strm * int -> string * 'strm,
          inputAll: 'strm -> string * 'strm }
        -> 'strm -> (response * 'strm) option

  val toString : response -> string
end = struct
  type response = {
    status: string,
    responsePhrase: string,
    header: HttpHeader.header,
    messageBody: string
  }

  fun parseFirstLine line =
        let
          fun neg pred x = not (pred x)
          val line = Substring.full line
          val (version, line) = Substring.splitl (neg Char.isSpace) line
          val line = Substring.dropl Char.isSpace line
          val (status, line) = Substring.splitl (neg Char.isSpace) line
          val responsePhrase = Substring.dropl Char.isSpace line
        in
          SOME { status = Substring.string status,
                 responsePhrase = Substring.string responsePhrase }
        end

  fun fromStream {inputLine, inputN, inputAll} strm =
        case inputLine strm of
             NONE => NONE
           | SOME (line, strm') =>
               case parseFirstLine line of
                    NONE => NONE
                  | SOME {status, responsePhrase} =>
                      let
                        val (header, strm'') = HttpHeader.fromStream inputLine strm'
                      in
                        case HttpHeader.lookup (header, "Content-Length") of
                             NONE =>
                               let
                                 val (messageBody, strm''') = inputAll strm''
                                 val response = { status = status,
                                                  responsePhrase = responsePhrase,
                                                  header = header,
                                                  messageBody = messageBody }
                               in
                                 SOME (response, strm''')
                               end
                           | SOME length =>
                               case Int.fromString length of
                                    NONE => NONE
                                  | SOME length =>
                                      let
                                        val (messageBody, strm''') = inputN (strm'', length)
                                        val response = { status = status,
                                                         responsePhrase = responsePhrase,
                                                         header = header,
                                                         messageBody = messageBody }
                                      in
                                        SOME (response, strm''')
                                      end
                      end

  fun toString {status, responsePhrase, header, messageBody} =
        "HTTP/1.1 " ^ status ^ " " ^ responsePhrase ^ "\r\n" ^
        HttpHeader.toString header ^
        "\r\n" ^
        messageBody
end
