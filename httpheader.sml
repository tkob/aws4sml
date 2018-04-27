structure HttpHeader :> sig
  type header

  val fromList : (string * string) list -> header
  val toList : header -> (string * string) list

  val toString : header -> string
end = struct
  type header = (string * string) list

  fun fromList l = l
  fun toList h = h

  fun toString header =
        String.concatWith "\r\n" (map (fn (name, value) => name ^ ": " ^ value) header) ^ "\r\n"
end
