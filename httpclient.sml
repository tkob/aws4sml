structure HttpClient :> sig
  exception InvalidResponse of string

  val doRequest : { read : 'sock -> Word8Vector.vector,
                    writeAll : 'sock * Word8Vector.vector -> unit }
                  -> 'sock * HttpRequest.request
                  -> HttpResponse.response

end = struct
  exception InvalidResponse of string

  fun doRequest {read, writeAll} (sock, request) =
        let
          val () = writeAll (sock, Byte.stringToBytes (HttpRequest.toString request))
          val strm = Stream.fromFun (fn () => Byte.bytesToString (read sock))
          val fromStream =
            HttpResponse.fromStream { inputN=Stream.inputN,
                                      inputLine=Stream.inputLine,
                                      inputAll=Stream.inputAll }
        in
          case fromStream strm of
               NONE => raise InvalidResponse (#1 (Stream.inputAll strm))
             | SOME (response, strm') => response
        end
end
