structure AwsCredentials = struct
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

  fun toString {accessKey, secret, expiration = NONE} =
         "{\"AccessKeyId\": \"" ^ accessKey ^ "\"" ^
         ", \"SecretAccessKey\": \"" ^ secret ^ "\"}"
    | toString {accessKey, secret, expiration = SOME expiration} =
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
        case getEnv "HOME" of
             NONE => NONE
           | SOME home =>
               IniFile.fromFile (home ^ "/.aws/credentials") >>= (fn iniFile =>
               IniFile.lookup iniFile "default" "aws_access_key_id" >>= (fn accessKey =>
               IniFile.lookup iniFile "default" "aws_secret_access_key" >>= (fn secret=>
               SOME { accessKey = accessKey,
                      secret = secret,
                      expiration = NONE })))

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
                          path = valOf (URI.Path.fromString awsContainerCredentialsRelativeUri),
                          query = URI.Query.fromList [],
                          header = valOf (HttpHeader.fromList []),
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
        getCredentialsFromProfile' getEnv                           or (fn () =>
        getCredentialsFromContainer' (getEnv, rw) credentials       or (fn () =>
        getCredentialsFromInstanceProfile' (getEnv, rw) credentials or (fn () =>
        raise CredentialsNotFound))))

  fun scanCredentials credentials =
        scanCredentials' (OS.Process.getEnv, defaultRw) credentials

  fun getRegionFromEnvironmentVariable' getEnv = getEnv "AWS_REGION"

  fun getRegionFromEnvironmentVariable () =
        getRegionFromEnvironmentVariable' OS.Process.getEnv

  fun getRegionFromProfile' getEnv =
        case getEnv "HOME" of
             NONE => NONE
           | SOME home =>
               IniFile.fromFile (home ^ "/.aws/config") >>= (fn iniFile =>
               IniFile.lookup iniFile "default" "region")

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
