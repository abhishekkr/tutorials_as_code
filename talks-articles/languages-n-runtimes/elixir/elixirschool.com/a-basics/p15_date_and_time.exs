## date and time

### time
#
#### * limited to working with UTC timezone
#
Time.utc_now() |> IO.inspect()  ## getting current time
Time.new(11, 22, 33) |> IO.inspect()
Time.new(11, 22, 33, 123456) |> IO.inspect()

#### * only contains time within a day; no DD/MM/YYYY is inferrable
#
tymx = ~T[21:31:24.012345] ## using sigil ~T[]
IO.inspect(tymx)
IO.inspect(tymx.hour)
IO.inspect(tymx.minute)
## IO.inspect(tymx.day)  ## error for not found


### date
#
Date.utc_today() |> IO.inspect()
Date.new(2011, 12, 23) |> IO.inspect()

#### * only contains date details; no time is inferrable
#
datx = ~D[2012-12-25] ## using sigil ~D[]
IO.inspect(datx)
IO.inspect(datx.year)
IO.inspect(datx.day)
## IO.inspect(datx.hour)  ## error for not found

{:ok, daty} = Date.new(2019, 10, 3)
Date.day_of_week(daty) |> IO.inspect()
Date.leap_year?(daty) |> IO.inspect()


### naivedatetime
#
#### * lack of TimeZone support
#
NaiveDateTime.utc_now() |> IO.inspect()
NaiveDateTime.new(2011, 12, 23, 0, 0, 10) |> IO.inspect()
#
ndx = ~N[2012-12-25 00:00:12] ## using sigil ~N[]
IO.inspect(ndx)
NaiveDateTime.add(ndx, 28) |> IO.inspect()


### datetime
#
#### * has both date and time with TimeZone support
#
#### * use only for conversion functions

DateTime.from_naive(ndx, "Etc/UTC") |> IO.inspect()

#### * Can also consider 'Timex' and 'Calendar'

##################################################
