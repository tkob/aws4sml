structure HMAC :> sig 

  val hmac : (Word8Vector.vector -> Word8Vector.vector) * int
    -> Word8Vector.vector
    -> Word8Vector.vector
    -> Word8Vector.vector

  val sha256 : Word8Vector.vector -> Word8Vector.vector

  val hmacSha256 : Word8Vector.vector -> Word8Vector.vector -> Word8Vector.vector

end = struct

  fun hmac (hash, len) key message =
        let
          open ExtWord8Vector
          infix xorBytes
          val key = rightPadBytes (word8 0wx00) len key
          val ipad = bytes (len, (word8 0wx36))
          val opad = bytes (len, (word8 0wx5c))
        in
          hash ((key xorBytes opad) ^ hash ((key xorBytes ipad) ^ message))
        end

  fun sha256 (bytes : Word8Vector.vector) : Word8Vector.vector =
        let 
          infix |>
          fun (x |> f) = f x
          val word8 = Word8.fromLarge o Sha256.Word.toLarge
          val word = Sha256.Word.fromLarge o Word.toLarge
          fun hh w = word8 (Sha256.Word.>> (w, 0w24))
          fun hl w = word8 (Sha256.Word.andb (word 0wxff, Sha256.Word.>> (w, 0w16)))
          fun lh w = word8 (Sha256.Word.andb (word 0wxff, Sha256.Word.>> (w, 0w8)))
          fun ll w = word8 (Sha256.Word.andb (word 0wxff, w))
          fun word32ToBytes w = Word8Vector.fromList [hh w, hl w, lh w, ll w]
          fun vectorToList vector = Vector.foldr (op ::) [] vector
          val hashed = Sha256.hashString (Byte.bytesToString bytes)
            |> Sha256.toVector
            |> Vector.map word32ToBytes
            |> vectorToList
            |> Word8Vector.concat
        in
          hashed
        end

  fun hmacSha256 key data = hmac (sha256, 64) key data

end
