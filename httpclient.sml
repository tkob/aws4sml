structure HttpClient :> sig

  val doRequest : { read : 'sock -> Word8Vector.vector,
                    writeAll : 'sock * Word8Vector.vector -> unit }
                  -> 'sock * HttpRequest.request -> unit

end = struct

  fun doRequest {read, writeAll} (sock, request) =
        let
          fun receive fragments =
                let
                  val fragment = read sock
                in
                  if Word8Vector.length fragment = 0 then
                    Word8Vector.concat (rev fragments)
                  else
                    receive (fragment::fragments)
                end
        in
          writeAll (sock, Byte.stringToBytes (HttpRequest.toString request));
          print (Byte.bytesToString (receive []))
        end
end
