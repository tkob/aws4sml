structure UriTest = struct
  open URI

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun testPathFromAndToString s () =
        Assert.assertEqualString s (Path.toString (valOf (Path.fromString s)))

  fun testPathFromAndToString' s t () =
        Assert.assertEqualString t (Path.toString (valOf (Path.fromString s)))

  fun testPathFromInvalidString s () =
        Assert.assertNone (Path.fromString s)

  val suite = Test.labelTests [
    ("path from \"\"",     testPathFromAndToString ""),
    ("path from /",        testPathFromAndToString "/"),
    ("path from /abc",     testPathFromAndToString "/abc"),
    ("path from /abc/",    testPathFromAndToString "/abc/"),
    ("path from abc/def",  testPathFromAndToString "abc/def"),
    ("path from abc/def/", testPathFromAndToString "abc/def/"),
    ("path from /%20",     testPathFromAndToString "/%20"),
    ("path from /%ff",     testPathFromAndToString' "/%ff" "/%FF"),
    ("path from /%",       testPathFromInvalidString "/%"),
    ("path from /%gf",     testPathFromInvalidString "/%gf"),
    ("path from /%f",      testPathFromInvalidString "/%f"),
    ("path from /%fg",     testPathFromInvalidString "/%fg")
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = UriTest.run ()
