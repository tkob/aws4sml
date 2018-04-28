structure AwsSigV4Test = struct

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  val suiteDir = "aws-sig-v4-test-suite"

  fun test reqFilePath =
        let
          val insReq = TextIO.getInstream (TextIO.openIn reqFilePath)
          val request = valOf (HttpRequest.fromStream insReq)

          val authzFilePath =
            OS.Path.joinBaseExt {base = OS.Path.base reqFilePath, ext = SOME "authz"}
          val insAuthz = TextIO.openIn authzFilePath
          val expectedAuthorizationHeader = TextIO.inputAll insAuthz

          val host = "example.amazonaws.com"
          val region = "us-east-1"
          val service = "service"
          val accessKey = "AKIDEXAMPLE"
          val secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
          val date = Date.date {year=2015, month=Date.Aug, day=30, hour=12, minute=36, second=0, offset=SOME (Time.zeroTime)}
          val (_, actualAuthorizationHeader) =
            Aws4Client.createAuthorizationHeader
              (host, region, service, accessKey, secret)
              date
              request
        in
          Assert.assertEqualString
            expectedAuthorizationHeader actualAuthorizationHeader
        end

  local
    fun walk' dirPath acc =
          let
            val dir = OS.FileSys.openDir dirPath
            fun loop acc =
                  case OS.FileSys.readDir dir of
                       NONE => acc
                     | SOME entry =>
                         let
                           val entryPath = OS.Path.concat (dirPath, entry)
                         in
                           if OS.FileSys.isDir entryPath
                           then
                             let
                               val acc' = walk' entryPath acc
                             in
                               loop (entryPath::acc')
                             end
                           else
                             loop (entryPath::acc)
                         end
          in
            loop acc
            before (OS.FileSys.closeDir dir)
          end
  in
    fun walk dirPath = walk' dirPath []
  end

  val suite =
        let
          fun isReqFile path = case OS.Path.ext path of
                                    NONE => false
                                  | SOME ext => ext = "req"
          val reqFiles = List.filter isReqFile (walk suiteDir)
          fun labelTest reqFilePath =
                (reqFilePath, fn () => test reqFilePath)
        in
          Test.labelTests (map labelTest reqFiles)
        end

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = AwsSigV4Test.run ()
