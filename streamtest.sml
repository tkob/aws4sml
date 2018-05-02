structure StreamTest = struct
  open Stream

  structure Assert = SMLUnit.Assert
  structure Test = SMLUnit.Test

  fun fromString (s, n) =
        let
          val strm = TextIO.openString s
        in
          fn () => TextIO.inputN (strm, n)
        end

  fun testInputAll () =
        let
          val source = "abcdef"
          val strm = fromFun (fromString (source, 2))
          val (actual, strm') = inputAll strm
          val (actual2, strm'') = inputAll strm'
          val (actual3, strm') = inputAll strm
          val (actual4, strm') = inputAll strm'
        in
          Assert.assertEqualString source actual;
          Assert.assertEqualString "" actual2;
          Assert.assertEqualString source actual3;
          Assert.assertEqualString "" actual4;
          ()
        end

  fun testInputN () =
        let
          val source = "abcdef"
          val strm = fromFun (fromString (source, 2))
          val (actual, strm') = inputN (strm, 6)
          val (actual2, _) = inputN (strm, 1)
          val (actual3, _) = inputN (strm, 2)
          val (actual4, strm'') = inputN (strm, 3)
          val (actual5, _) = inputAll strm'
          val (actual6, _) = inputN (strm'', 2)
          val (actual7, _) = inputN (strm'', 3)
        in
          Assert.assertEqualString source actual;
          Assert.assertEqualString "a" actual2;
          Assert.assertEqualString "ab" actual3;
          Assert.assertEqualString "abc" actual4;
          Assert.assertEqualString "" actual5;
          Assert.assertEqualString "de" actual6;
          Assert.assertEqualString "def" actual7;
          ()
        end

  val suite = Test.labelTests [
    ("test inputAll", testInputAll),
    ("test inputN", testInputN)]

  fun run () =
        SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suite
end

val () = StreamTest.run ()
