functor Aws4ClientFun(val date : unit -> Date.date) = struct
  fun sort (l : string list) : string list  = ListMergeSort.sort (fn (x, y) => x > y) l
  fun contains ([], x) = false
    | contains (x'::xs, x) = if x = x' then true else contains (xs, x)
  fun uniq xs =
        let
          fun uniq' ([], acc) = rev acc
            | uniq' (x::xs, acc) =
                if contains (acc, x)
                then uniq' (xs, acc)
                else uniq' (xs, x::acc)
        in
          uniq' (xs, [])
        end

  fun createCanonicalRequest {
          method, path, query, header, messageBody} =
        let
          infix |>
          fun (x |> f) = f x
          val canonicalUri = URI.Path.toString (URI.Path.canonicalize path)
          fun parameterToString (name, value) =  name ^ "=" ^ value
          val canonicalQueryString =
            String.concatWith "&" (sort (map parameterToString (URI.Query.toList query)))
          fun lowerCase s = String.map Char.toLower s
          fun trimAll s =
                let
                  (* trim leading and trailing spaces first *)
                  val s = Substring.full s
                    |> Substring.dropl (Char.isSpace)
                    |> Substring.dropr (Char.isSpace)
                  (* then remove successive spaces *)
                  fun previousCharacterIsNotSpace (s, acc) =
                        case Substring.getc s of
                             NONE => implode (rev acc)
                           | SOME (c, s) =>
                               if Char.isSpace c then
                                 previousCharacterIsSpace (s, #" "::acc)
                               else
                                 previousCharacterIsNotSpace (s, c::acc)
                  and previousCharacterIsSpace (s, acc) =
                        case Substring.getc s of
                             NONE => raise Fail "should never reach here"
                           | SOME (c, s) =>
                               if Char.isSpace c then
                                 previousCharacterIsSpace (s, acc)
                               else
                                 previousCharacterIsNotSpace (s, c::acc)
                in
                  previousCharacterIsNotSpace (s, [])
                end
          val signedHeaderNames = uniq (sort (map (lowerCase o #1) (HttpHeader.toList header)))
          val canonicalHeaders = signedHeaderNames
            |> map (fn name => (name, HttpHeader.lookupAll (header, name)))
            |> map (fn (name, values) => (name, String.concatWith "," (map trimAll values)))
            |> map (fn (name, values) => name ^ ":" ^ values ^ "\n")
            |> concat
          val signedHeaders = String.concatWith ";" signedHeaderNames
          val hashedRequestPayload = Sha256.hashString messageBody
          val hexEncodedHashedRequestPayload =
            lowerCase (Sha256.toString hashedRequestPayload)
          val canonicalRequest =
            method ^ "\n" ^
            canonicalUri ^ "\n" ^
            canonicalQueryString ^ "\n" ^
            canonicalHeaders ^ "\n" ^
            signedHeaders ^ "\n" ^
            hexEncodedHashedRequestPayload
        in
          (canonicalRequest, signedHeaders)
        end

  fun createStringToSign (date, region, service, hashedCanonicalRequest) = 
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val algorithm = "AWS4-HMAC-SHA256"
          val requestDateTime = ExtDate.toIso8601Basic date
          val credentialScope =
            String.concatWith "/" [ExtDate.toYYYYmmdd date, region, service, "aws4_request"]
        in
          (String.concatWith "\n" [algorithm, requestDateTime, credentialScope, hashedCanonicalRequest],
          credentialScope)
        end

  fun calculateSignature (secret, date, region, service, stringToSign) =
        let
          val op ^ = ExtWord8Vector.^
          infix ^
          val bytes = Byte.stringToBytes
          val kSecret = bytes secret
          val date = ExtDate.toYYYYmmdd date
          val kDate    = HMAC.hmacSha256 (bytes "AWS4" ^ kSecret) (bytes date)
          val kRegion  = HMAC.hmacSha256 kDate                    (bytes region)
          val kService = HMAC.hmacSha256 kRegion                  (bytes service)
          val kSigning = HMAC.hmacSha256 kService                 (bytes "aws4_request")
        in
          HMAC.hmacSha256 kSigning (bytes stringToSign)
        end

  fun createAuthorizationHeader
          (host, region, service, accessKey, secret)
          date
          (request as {method, path, query, header, messageBody}) =
        let
          val (canonicalRequest, signedHeaders) = createCanonicalRequest request
          val hashedCanonicalRequest =
            String.map Char.toLower (Sha256.toString (Sha256.hashString canonicalRequest))
          val (stringToSign, credentialScope)  =
            createStringToSign (date, region, service, hashedCanonicalRequest)
          val signature_ = calculateSignature (secret, date, region, service, stringToSign)
          val authorizationHeader =
            ("Authorization",
            "AWS4-HMAC-SHA256" ^
            " Credential=" ^ accessKey ^ "/" ^ credentialScope ^
            ", SignedHeaders=" ^ signedHeaders ^
            ", Signature=" ^ ExtWord8Vector.base16lower signature_)
        in
          authorizationHeader
        end

  fun addSignatureToRequest
          (host, region, service, accessKey, secret)
          date
          (request as {method, path, query, header, messageBody}) =
        let
          val hostHeader = ("Host", host)
          val xAmzDateHeader = ("X-Amz-Date", ExtDate.toIso8601Basic date)
          val header = if HttpHeader.contains (header, "Host") then header else
            HttpHeader.fromList (HttpHeader.toList header @ [hostHeader])
          val header = if HttpHeader.contains (header, "X-Amz-Date") then header else
            HttpHeader.fromList (HttpHeader.toList header @ [xAmzDateHeader])
          val request = {method=method, path=path, query=query, header=header, messageBody=messageBody}

          val authorizationHeader =
            createAuthorizationHeader
              (host, region, service, accessKey, secret)
              date
              request
        in
          { method = method,
            path = path,
            query = query,
            header = HttpHeader.fromList (HttpHeader.toList header @ [authorizationHeader]),
            messageBody = messageBody }
        end

  fun doRequest 
          readWrite
          (host, region, service, accessKey, secret)
          (sock, date, request) =
        let
          val requestWithSignature =
            addSignatureToRequest (host, region, service, accessKey, secret) date request
          val _ = print (HttpRequest.toString requestWithSignature)
       in
          HttpClient.doRequest readWrite (sock, requestWithSignature)
        end

  infix >>=
  fun (SOME x) >>= k = k x
    | NONE     >>= k = NONE

  infix or
  fun (SOME x) or k = x
    | NONE     or k = k ()

  val defaultRw = {
    read = fn sock => Socket.recvVec (sock, 1024),
    writeAll = fn (sock, vec) =>
      ignore (Socket.sendVec (sock, Word8VectorSlice.full vec)),
    connect = fn (host, port) =>
      let
        val sock = INetSock.TCP.socket ()
        val addr =
          case NetHostDB.getByName host of
               NONE => raise OS.SysErr (host ^ " not found", NONE)
             | SOME entry =>
                 INetSock.toAddr (NetHostDB.addr entry, port)
      in
        Socket.connect (sock, addr);
        sock
      end,
    close = Socket.close }

  type credentials = {
    accessKey : string,
    secret : string,
    expiration : Date.date option
  }

  fun credentialsToString {accessKey, secret, expiration = NONE} =
         "{\"AccessKeyId\": \"" ^ accessKey ^ "\"" ^
         ", \"SecretAccessKey\": \"" ^ secret ^ "\"}"
    | credentialsToString {accessKey, secret, expiration = SOME expiration} =
         "{\"AccessKeyId\": \"" ^ accessKey ^ "\"" ^
         ", \"SecretAccessKey\": \"" ^ secret ^ "\"" ^
         ", \"Expiration\": \"" ^ ExtDate.toIso8601Basic expiration ^ "\"}"

  fun isExpired ({expiration = NONE, ...} : credentials) = false
    | isExpired {expiration = SOME _, ...} = true (* TODO *)

  fun getCredentialsFromEnvironmentVariable' getEnv =
        getEnv "AWS_ACCESS_KEY_ID"     >>= (fn accessKey =>
        getEnv "AWS_SECRET_ACCESS_KEY" >>= (fn secret =>
        SOME {accessKey = accessKey, secret = secret, expiration = NONE}))

  fun getCredentialsFromEnvironmentVariable () =
    getCredentialsFromEnvironmentVariable' OS.Process.getEnv

  fun getCredentialsFromProfile' getEnv =
        NONE (* TODO *)

  fun getCredentialsFromProfile () =
        getCredentialsFromProfile' OS.Process.getEnv

  fun lookupString (JSON.OBJECT attributes, name) =
        let
          fun lookup [] = NONE
            | lookup ((name', JSON.STRING value)::attributes) =
                if name = name' then SOME value
                else lookup attributes
            | lookup _ = NONE
        in
          lookup attributes
        end
    | lookupString _ = NONE

  fun getCredentialsFromContainer'
          (getEnv, rw as {read, writeAll, connect, close}) credentials =
        case credentials of
             SOME credentials =>
               if isExpired credentials
               then getCredentialsFromContainer' (getEnv, rw) NONE
               else SOME credentials
           | NONE =>
               case getEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" of
                    NONE => NONE
                  | SOME awsContainerCredentialsRelativeUri =>
                      let
                        val sock = connect ("169.254.170.2", 80)
                        val request = {
                          method = "GET",
                          path = URI.Path.fromString awsContainerCredentialsRelativeUri,
                          query = URI.Query.fromList [],
                          header = HttpHeader.fromList [],
                          messageBody = ""
                        }
                        val doRequest =
                          HttpClient.doRequest {read=read, writeAll=writeAll}
                        val response = doRequest (sock, request)
                        val credentialsJson =
                          JSONParser.parse (TextIO.openString (#messageBody response))
                        val parseDate =
                          ExtDate.W3cDtf.fromString ExtDate.W3cDtf.date
                      in
                        lookupString (credentialsJson, "AccessKeyId") >>= (fn accessKey =>
                        lookupString (credentialsJson, "SecretAccessKey") >>= (fn secret =>
                        lookupString (credentialsJson, "Expiration") >>= (fn expiration =>
                        parseDate expiration >>= (fn expiration =>
                        SOME { accessKey = accessKey,
                               secret = secret,
                               expiration = SOME expiration }))))
                        before (close sock)
                      end

  fun getCredentialsFromContainer credentials =
        getCredentialsFromContainer' (OS.Process.getEnv, defaultRw) credentials

  fun getCredentialsFromInstanceProfile'
          (getEnv, rw as {read, writeAll, connect, close}) credentials =
        NONE (* TODO *)

  fun getCredentialsFromInstanceProfile credentials =
        getCredentialsFromInstanceProfile' (OS.Process.getEnv, defaultRw) credentials

  exception CredentialsNotFound

  fun scanCredentials' (getEnv, rw) credentials =
        getCredentialsFromEnvironmentVariable' getEnv               or (fn () =>
        getCredentialsFromProfile' ()                               or (fn () =>
        getCredentialsFromContainer' (getEnv, rw) credentials       or (fn () =>
        getCredentialsFromInstanceProfile' (getEnv, rw) credentials or (fn () =>
        raise CredentialsNotFound))))

  fun scanCredentials credentials =
        scanCredentials' (OS.Process.getEnv, defaultRw) credentials

  fun getRegionFromEnvironmentVariable' getEnv = getEnv "AWS_REGION"

  fun getRegionFromEnvironmentVariable () =
        getRegionFromEnvironmentVariable' OS.Process.getEnv

  fun getRegionFromProfile' getEnv =
        NONE (* TODO *)

  fun getRegionFromProfile () = getRegionFromProfile' OS.Process.getEnv

  fun getRegionFromInstanceProfile' (getEnv, rw) =
        NONE (* TODO *)

  fun getRegionFromInstanceProfile () =
        getRegionFromInstanceProfile' (OS.Process.getEnv, defaultRw)

  exception RegionNotFound

  fun scanRegion' (getEnv, rw) =
        getRegionFromEnvironmentVariable' getEnv   or (fn () =>
        getRegionFromProfile' getEnv               or (fn () =>
        getRegionFromInstanceProfile' (getEnv, rw) or (fn () =>
        raise RegionNotFound)))

  fun scanRegion () = scanRegion' (OS.Process.getEnv, defaultRw)
end

structure Aws4Client = Aws4ClientFun(val date = fn () => Date.fromTimeUniv (Time.now ()))
