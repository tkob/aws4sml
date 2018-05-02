structure ExtDate = struct

  open Date

  fun leftPad c width s =
        if String.size s >= width then s
        else leftPad c width (String.str c ^ s)

  fun monthToDigits Date.Jan = "01"
    | monthToDigits Date.Feb = "02"
    | monthToDigits Date.Mar = "03"
    | monthToDigits Date.Apr = "04"
    | monthToDigits Date.May = "05"
    | monthToDigits Date.Jun = "06"
    | monthToDigits Date.Jul = "07"
    | monthToDigits Date.Aug = "08"
    | monthToDigits Date.Sep = "09"
    | monthToDigits Date.Oct = "10"
    | monthToDigits Date.Nov = "11"
    | monthToDigits Date.Dec = "12"

  fun toIso8601Basic date =
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val year = leftPad #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = leftPad #"0" 2 (Int.toString (Date.day date))
          val hour = leftPad #"0" 2 (Int.toString (Date.hour date))
          val minute = leftPad #"0" 2 (Int.toString (Date.minute date))
          val second = leftPad #"0" 2 (Int.toString (Date.second date))
        in
          String.concat [year, month, day, "T", hour, minute, second, "Z"]
        end

  fun toYYYYmmdd date =
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val year = leftPad #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = leftPad #"0" 2 (Int.toString (Date.day date))
        in
          String.concat [year, month, day]
        end
end
