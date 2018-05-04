structure Aws4ClientTest = struct

  val mockDate = Date.date { year=2017,
                             month=Date.May,
                             day=17,
                             hour=15,
                             minute=9,
                             second=54,
                             offset=SOME (Time.zeroTime) }
  structure MockAws4Client= Aws4ClientFun(val date = fn () => mockDate)
  open MockAws4Client

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  (* mock 'socket' which emits specified string *)
  fun stringRw s = {
    read = fn sock => Byte.stringToBytes (TextIO.input sock),
    writeAll = fn _ => (),
    connect = fn _ => TextIO.openString s,
    close = fn _ => () }

  fun assertEqualCredentials (expected : AwsCredentials.credentials) (actual : AwsCredentials.credentials) = (
        Assert.assertEqualString (#accessKey expected) (#accessKey actual);
        Assert.assertEqualString (#secret expected) (#secret actual);
        Assert.assertEqualOption Assert.AssertDate.assertEqualDate (#expiration expected) (#expiration actual))

  fun testGetCredentialsFromContainer () =
        let
          fun getEnv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" = SOME "/"
            | getEnv _ = NONE
          val rw = stringRw ("HTTP/1.1 200 OK\r\n\r\n" ^
            "{\n" ^
            "\"AccessKeyId\": \"ACCESS_KEY_ID\",\n" ^
            "\"Expiration\": \"2017-05-17T15:09:54Z\",\n" ^
            "\"RoleArn\": \"TASK_ROLE_ARN\",\n" ^
            "\"SecretAccessKey\": \"SECRET_ACCESS_KEY\",\n" ^
            "\"Token\": \"SECURITY_TOKEN_STRING\"\n" ^
            "}")
          val actual = AwsCredentials.getCredentialsFromContainer' (getEnv, rw) NONE
          val expected = SOME { accessKey="ACCESS_KEY_ID",
                                secret="SECRET_ACCESS_KEY",
                                expiration=SOME mockDate }
        in
          Assert.assertEqualOption assertEqualCredentials expected actual
        end

  val suite = Test.labelTests [
    ("test getCredentialsFromContainer", testGetCredentialsFromContainer)]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = Aws4ClientTest.run ()
