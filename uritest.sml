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

  fun testQueryFromListToString l s () =
        Assert.assertEqualString
          s
          (Query.toString (Query.fromList l))

  fun testUriFromAndToString s () =
        Assert.assertEqualString s (URI.toString (valOf (URI.fromString s)))

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
    ("path from /c=GB", testPathFromAndToString' "/c=GB" "/c=GB"),
    ("path from comp.infosystems.www.servers.unix", testPathFromAndToString' "comp.infosystems.www.servers.unix" "comp.infosystems.www.servers.unix"),
    ("path from +1-816-555-1212", testPathFromAndToString' "+1-816-555-1212" "+1-816-555-1212"),
    ("path from oasis:names:specification:docbook:dtd:xml:4.1.2", testPathFromAndToString' "oasis:names:specification:docbook:dtd:xml:4.1.2" "oasis:names:specification:docbook:dtd:xml:4.1.2"),

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
    ("remove-dot-segments 14", testRemoveDotSegments ".."     ""),

    ("empty list to query", testQueryFromListToString [] ""),
    ("blank key-value to query", testQueryFromListToString [("", "")] "="),
    ("blank value to query", testQueryFromListToString [("a", "")] "a="),
    ("blank key to query", testQueryFromListToString [("", "b")] "=b"),
    ("key-value to query", testQueryFromListToString [("a", "b")] "a=b"),
    ("key-value to query: slash and question mark",
      testQueryFromListToString [("a", "http://foo@example.com?q")] "a=http://foo@example.com?q"),
    ("key-value to query: space", testQueryFromListToString [("a", " ")] "a=%20"),

    ("", testUriFromAndToString "http://example.com"),
    ("", testUriFromAndToString "http://example.com/"),
    ("", testUriFromAndToString "http://example.com/path"),
    ("", testUriFromAndToString "http://user-info@example.com:8080/"),
    ("", testUriFromAndToString "http://user:pass@example.com/"),
    ("", testUriFromAndToString "http://user%20name:pass@example.com/"),
    ("", testUriFromAndToString "http://example.com/?query=exists"),
    ("", testUriFromAndToString "http://example.com/#fragment"),
    ("", testUriFromAndToString "http://example.com/?query=exists#fragment"),
    ("", testUriFromAndToString "arn:aws:logs:region:account:log-group:log-group-name:log-stream:log-stream-name"),

    ("terminator", fn () => ())
  ]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = UriTest.run ()
