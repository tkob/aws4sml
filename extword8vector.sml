structure ExtWord8Vector :> sig

  include MONO_VECTOR
    where type vector = Word8Vector.vector
    where type elem = Word8.word

  val word8 : Word.word -> elem
  val xorBytes : vector * vector -> vector
  val ^ : vector * vector -> vector
  val rightPadBytes : elem -> int -> vector -> vector
  val bytes : int * elem -> vector
  val base16 : vector -> string
  val base16lower : vector -> string

end = struct

  open Word8Vector

  val word8 = Word8.fromLarge o Word.toLarge

  fun toList bytes = Word8Vector.foldr (op ::) [] bytes

  fun xorBytes (a, b) =
        let
          val len = Word8Vector.length a
          fun subXor i =
                Word8.xorb (Word8Vector.sub (a, i), Word8Vector.sub (b, i))
        in
          if len <> Word8Vector.length b then raise Fail ""
          else
            Word8Vector.tabulate (len, subXor)
        end

  infix ^
  fun (a ^ b) = Word8Vector.concat [a, b]

  fun rightPadBytes byte targetLen bytes =
        let
          val sourceLen = Word8Vector.length bytes
          fun take i =
                if i < sourceLen
                then Word8Vector.sub (bytes, i)
                else byte
        in
          Word8Vector.tabulate (targetLen, take)
        end

  fun bytes (len, init) = Word8Vector.tabulate (len, fn _ => init)

  fun base16 bytes = String.concat (List.map (StringCvt.padLeft #"0" 2 o Word8.toString) (toList bytes))
  fun base16lower bytes = String.map Char.toLower (base16 bytes)

end
