structure Aws4Client = struct

  fun sort (l : string list) : string list  = ListMergeSort.sort (fn (x, y) => x > y) l

  fun createCanonicalRequest {
          method, path, query, header, messageBody} =
        let
          val canonicalUri = URI.Path.toString path
          fun parameterToString (name, value) =  name ^ "=" ^ value
          val canonicalQueryString =
            String.concatWith "&" (sort (map parameterToString (URI.Query.toList query)))
          fun lowerCase s = String.map Char.toLower s
          (* TODO: remove successive spaces *)
          fun trimAll s =
                let
                  val s = Substring.full s
                  val s = Substring.dropl (Char.isSpace) s
                  val s = Substring.dropr (Char.isSpace) s
                in
                  Substring.string s
                end
          fun canonicalHeadersEntry (name, value) =
                lowerCase name ^ ":" ^ trimAll value ^ "\n"
          val canonicalHeaders = concat (sort (map canonicalHeadersEntry (HttpHeader.toList header)))
          val signedHeaders = String.concatWith ";" (sort (map (lowerCase o #1) (HttpHeader.toList header)))
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

  fun leftPad c width s =
        if String.size s >= width then s
        else leftPad c width (String.str c ^ s)

  fun monthToDigits Date.Jan = "01"
    | monthToDigits Date.Feb = "02"
    | monthToDigits Date.Mar = "03"
    | monthToDigits Date.Apr = "04"
    | monthToDigits Date.May = "05"
    | monthToDigits Date.Jun = "06"
    | monthToDigits Date.Jul = "07"
    | monthToDigits Date.Aug = "08"
    | monthToDigits Date.Sep = "09"
    | monthToDigits Date.Oct = "10"
    | monthToDigits Date.Nov = "11"
    | monthToDigits Date.Dec = "12"

  fun dateToIso8601Basic date =
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val year = leftPad #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = leftPad #"0" 2 (Int.toString (Date.day date))
          val hour = leftPad #"0" 2 (Int.toString (Date.hour date))
          val minute = leftPad #"0" 2 (Int.toString (Date.minute date))
          val second = leftPad #"0" 2 (Int.toString (Date.second date))
        in
          String.concat [year, month, day, "T", hour, minute, second, "Z"]
        end

  fun dateToYYYYmmdd date =
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val year = leftPad #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = leftPad #"0" 2 (Int.toString (Date.day date))
        in
          String.concat [year, month, day]
        end

  fun createStringToSign (date, region, service, hashedCanonicalRequest) = 
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val algorithm = "AWS4-HMAC-SHA256"
          val requestDateTime = dateToIso8601Basic date
          val credentialScope =
            String.concatWith "/" [dateToYYYYmmdd date, region, service, "aws4_request"]
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
          val date = dateToYYYYmmdd date
          val kDate    = HMAC.hmacSha256 (bytes "AWS4" ^ kSecret) (bytes date)
          val kRegion  = HMAC.hmacSha256 kDate                    (bytes region)
          val kService = HMAC.hmacSha256 kRegion                  (bytes service)
          val kSigning = HMAC.hmacSha256 kService                 (bytes "aws4_request")
        in
          HMAC.hmacSha256 kSigning (bytes stringToSign)
        end

  fun addSignatureToRequest
          (host, region, service, accessKey, secret)
          date
          (request as {method, path, query, header, messageBody}) =
        let
          val hostHeader = ("Host", host)
          val xAmzDateHeader = ("X-Amz-Date", dateToIso8601Basic date)
          val header = HttpHeader.fromList (hostHeader::xAmzDateHeader::HttpHeader.toList header)
          val request = {method=method, path=path, query=query, header=header, messageBody=messageBody}

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
          { method = method,
            path = path,
            query = query,
            header = HttpHeader.fromList (authorizationHeader::HttpHeader.toList header),
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
end
