structure ExtDate = struct

  open Date

  fun digitsToMonth "01" = SOME Date.Jan
    | digitsToMonth "02" = SOME Date.Feb
    | digitsToMonth "03" = SOME Date.Mar
    | digitsToMonth "04" = SOME Date.Apr
    | digitsToMonth "05" = SOME Date.May
    | digitsToMonth "06" = SOME Date.Jun
    | digitsToMonth "07" = SOME Date.Jul
    | digitsToMonth "08" = SOME Date.Aug
    | digitsToMonth "09" = SOME Date.Sep
    | digitsToMonth "10" = SOME Date.Oct
    | digitsToMonth "11" = SOME Date.Nov
    | digitsToMonth "12" = SOME Date.Dec
    | digitsToMonth _ = NONE

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
          val year = StringCvt.padLeft #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = StringCvt.padLeft #"0" 2 (Int.toString (Date.day date))
          val hour = StringCvt.padLeft #"0" 2 (Int.toString (Date.hour date))
          val minute = StringCvt.padLeft #"0" 2 (Int.toString (Date.minute date))
          val second = StringCvt.padLeft #"0" 2 (Int.toString (Date.second date))
        in
          String.concat [year, month, day, "T", hour, minute, second, "Z"]
        end

  fun toYYYYmmdd date =
        let
          val date = Date.fromTimeUniv (Date.toTime date)
          val year = StringCvt.padLeft #"0" 4 (Int.toString (Date.year date))
          val month = monthToDigits (Date.month date)
          val day = StringCvt.padLeft #"0" 2 (Int.toString (Date.day date))
        in
          String.concat [year, month, day]
        end

  structure Parser = struct
    infix >>=
    fun (SOME x) >>= k = k x
      | NONE     >>= k = NONE
    infix ||
    fun (a || b) input1 strm =
          case a input1 strm of
               SOME x => SOME x
             | NONE => b input1 strm
    infix --
    fun (a -- b) input1 strm =
          a input1 strm >>= (fn (e1, strm) =>
          b input1 strm >>= (fn (e2, strm) =>
          SOME ((e1, e2), strm)))
    fun flattenTriple ((e1, e2), e3) = SOME (e1, e2, e3)
    fun flatten4Tuple (((e1, e2), e3), e4) = SOME (e1, e2, e3, e4)
    fun flatten5Tuple ((((e1, e2), e3), e4), e5) = SOME (e1, e2, e3, e4, e5)
    fun flatten6Tuple (((((e1, e2), e3), e4), e5), e6) = SOME (e1, e2, e3, e4, e5, e6)
    fun flatten7Tuple ((((((e1, e2), e3), e4), e5), e6), e7) = SOME (e1, e2, e3, e4, e5, e6, e7)
    fun transform a f input1 strm =
          case a input1 strm of
               NONE => NONE
             | SOME (elem, strm') =>
                 case f elem of
                      NONE => NONE
                    | SOME elem' => SOME (elem', strm')
    infix >>
    fun (a >> f) input1 strm = transform a f input1 strm
    fun repeat class input1 strm =
          let
            fun loop strm acc =
                  case class input1 strm of
                       SOME (c, strm') => loop strm' (c::acc)
                     | NONE => SOME (rev acc, strm)
          in
            loop strm []
          end
    fun many0 class input1 strm = (
          (repeat class >> (fn acc => SOME acc)))
          input1 strm
    fun many1 class input1 strm = (
          (class -- repeat class)
          >> (fn (car, cdr) => SOME (car::cdr)))
          input1 strm
    fun opt class input1 strm =
          case class input1 strm of
               NONE => SOME (NONE, strm)
             | SOME (x, strm') => SOME (SOME x, strm')
    fun pred p input1 strm = (* consume an element that satisfies p *)
          case input1 strm of
               NONE => NONE
             | SOME (c', strm') =>
                 if p c' then SOME (c', strm') else NONE
    fun any input1 strm = pred (fn _ => true) input1 strm
    fun char c input1 strm = pred (fn c' => c' = c) input1 strm
    fun string s input1 strm =
          let
            val substring = Substring.full s
            fun loop (substring, strm) =
                  case Substring.getc substring of
                       NONE => SOME (s, strm)
                     | SOME (c, substring') =>
                         case input1 strm of
                              NONE => NONE
                            | SOME (c', strm') =>
                                if c' = c then loop (substring', strm') else NONE
          in
            loop (substring, strm)
          end
    fun space input1 strm = char #" " input1 strm
    fun digit input1 strm = pred Char.isDigit input1 strm
    fun alpha input1 strm = pred Char.isAlpha input1 strm
    fun int input1 strm = (
          (many1 digit)
          >> Option.filter (fn cs => cs = [#"0"] orelse hd cs <> #"0")
          >> (Int.fromString o implode))
          input1 strm
  end

  structure W3cDtf = struct
    type year = int
    type year_month = { year : int, month : Date.month }
    type year_month_day = { year : int, month : Date.month, day : int }
    type year_month_day_hour_minute_tzd = {
      year : int,
      month : Date.month,
      day : int,
      hour : int,
      minute : int,
      offset : Time.time }
    type date_fraction = Date.date * Time.time

    datatype w3c_date_time = Year of year
                           | YearMonth of year_month
                           | YearMonthDay of year_month_day
                           | YearMonthDayHourMinuteTzd of
                              year_month_day_hour_minute_tzd
                           | Date of Date.date
                           | DateFraction of date_fraction

    open Parser
    infix >= || -- >>

    (* YYYY (eg 1997) *)
    fun year input1 strm = (
          (many1 digit)
          >> (fn cs => SOME (implode cs))
          >> Int.fromString)
          input1 strm

    local
      fun month input1 strm = (
            (digit -- digit)
            >> (fn (dh, dl) => SOME (implode [dh, dl]))
            >> digitsToMonth)
            input1 strm
    in
      (* YYYY-MM (eg 1997-07) *)
      fun yearMonth input1 strm = (
            (year -- char #"-" -- month)
            >> flattenTriple
            >> (fn (year, _, month) => SOME {year=year, month=month}))
            input1 strm
    end

    local
      fun day input1 strm = (
            (digit -- digit)
            >> (fn (dh, dl) => SOME (implode [dh, dl]))
            >> Int.fromString
            >> (fn i => if i < 1 orelse i > 31 then NONE else SOME i))
            input1 strm
    in
      (* YYYY-MM-DD (eg 1997-07-16) *)
      fun yearMonthDay input1 strm = (
            (yearMonth -- char #"-" -- day)
            >> flattenTriple
            >> (fn ({year, month}, _, day) =>
                SOME ({ year = year,
                        month = month,
                        day = day })))
            input1 strm
    end

    local
      fun hour input1 strm = (
            (digit -- digit)
            >> (fn (dh, dl) => SOME (implode [dh, dl]))
            >> Int.fromString
            >> (fn i => if i < 0 orelse i > 23 then NONE else SOME i))
            input1 strm
      fun minute input1 strm = (
            (digit -- digit)
            >> (fn (dh, dl) => SOME (implode [dh, dl]))
            >> Int.fromString
            >> (fn i => if i < 0 orelse i > 59 then NONE else SOME i))
            input1 strm
      fun hourMinute input1 strm = (
            (hour -- char #":" -- minute)
            >> flattenTriple
            >> (fn (hour, _, minute) => SOME (hour, minute)))
            input1 strm
      val second = minute
      fun fraction input1 strm = (
            (char #"." -- many1 digit)
            >> (fn (dot, fraction) => SOME (implode (dot::fraction)))
            >> Time.fromString)
            input1 strm
      fun hmToSeconds (hour, minute) =
            Int.toLarge (hour * 60 * 60 + minute * 60)
      fun tzd input1 strm = (
               (string "Z" >> (fn _ => SOME Time.zeroTime))
            || ((char #"+" -- hourMinute)
               >> (fn (_, hourMinute) =>
                    SOME (Time.fromSeconds (hmToSeconds hourMinute))))
            || ((char #"-" -- hourMinute)
               >> (fn (_, hourMinute) =>
                    SOME (Time.fromSeconds (~(hmToSeconds hourMinute))))))
            input1 strm
    in
      (* YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00) *)
      fun yearMonthDayHourMinute input1 strm = (
            (yearMonthDay -- char #"T" -- hourMinute -- tzd)
            >> flatten4Tuple
            >> (fn ({year, month, day}, _, (hour, minute), tzd) =>
                SOME ({ year = year,
                        month = month,
                        day = day,
                        hour = hour,
                        minute = minute,
                        offset = tzd })))
            input1 strm

      (* YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00) *)
      fun date input1 strm = (
            (yearMonthDay -- char #"T" -- hourMinute -- char #":" -- second -- tzd)
            >> flatten6Tuple
            >> (fn ({year, month, day}, _, (hour, minute), _, second, tzd) =>
                SOME (Date.date { year = year,
                                  month = month,
                                  day = day,
                                  hour = hour,
                                  minute = minute,
                                  second = second,
                                  offset = SOME tzd })))
            input1 strm

      fun dateFraction input1 strm = (
            (yearMonthDay -- char #"T" -- hourMinute -- char #":" -- second -- fraction -- tzd)
            >> flatten7Tuple
            >> (fn ({year, month, day}, _, (hour, minute), _, second, fraction, tzd) =>
                SOME (Date.date { year = year,
                                  month = month,
                                  day = day,
                                  hour = hour,
                                  minute = minute,
                                  second = second,
                                  offset = SOME tzd }, fraction)))
            input1 strm
    end

    fun fromString parser s =
          case parser Substring.getc (Substring.full s) of
               NONE => NONE
             | SOME (d, s') =>
                 if Substring.isEmpty s' then SOME d
                 else NONE
  end
end
