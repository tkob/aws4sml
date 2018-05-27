structure UriTest = struct
  open URI

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun testPathFromAndToString s () =
        Assert.assertEqualString s (Path.toString (valOf (Path.fromString s)))

  fun testPathFromAndToString' s t () =
        Assert.assertEqualString t (Path.toString (valOf (Path.fromString s)))

  fun testRemoveDotSegments s t () =
        Assert.assertEqualString
          t
          (Path.toString (Path.removeDotSegments (valOf (Path.fromString s))))

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
    ("path from fred@example.com", testPathFromAndToString' "fred@example.com" "fred@example.com"),
    ("path from /%",       testPathFromInvalidString "/%"),
    ("path from /%gf",     testPathFromInvalidString "/%gf"),
    ("path from /%f",      testPathFromInvalidString "/%f"),
    ("path from /%fg",     testPathFromInvalidString "/%fg"),
    ("remove-dot-segments 1",  testRemoveDotSegments "/a/b/c/./../../g" "/a/g"),
    ("remove-dot-segments 2",  testRemoveDotSegments "mid/content=5/../6" "mid/6"),
    ("remove-dot-segments 3",  testRemoveDotSegments "../"    ""),
    ("remove-dot-segments 4",  testRemoveDotSegments "./"     ""),
    ("remove-dot-segments 5",  testRemoveDotSegments "/./"    "/"),
    ("remove-dot-segments 6",  testRemoveDotSegments "/."     "/"),
    ("remove-dot-segments 7",  testRemoveDotSegments "/../"   "/"),
    ("remove-dot-segments 8",  testRemoveDotSegments "/.."    "/"),
    ("remove-dot-segments 9",  testRemoveDotSegments "a/../"  "/"),
    ("remove-dot-segments 10", testRemoveDotSegments "a/.."   "/"),
    ("remove-dot-segments 11", testRemoveDotSegments "/a/../" "/"),
    ("remove-dot-segments 12", testRemoveDotSegments "/a/.."  "/"),
    ("remove-dot-segments 13", testRemoveDotSegments "."      ""),
    ("remove-dot-segments 14", testRemoveDotSegments ".."     "")
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = UriTest.run ()
