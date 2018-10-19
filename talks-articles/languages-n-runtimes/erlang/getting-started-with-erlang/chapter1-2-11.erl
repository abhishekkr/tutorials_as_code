-module('chapter1-2-11').
-export([month_length/2]).

month_length(Year, Month) ->
  Leap = is_leap_year(Year), % if construct returned selected segment
  days_for(Month, Leap).

is_leap_year(Year) ->
  if
    trunc(Year / 400) * 400 == Year ->
      leap;
    trunc(Year / 100) * 100 == Year ->
      not_leap;
    trunc(Year / 4) * 4 == Year ->
      leap;
    true ->
      not_leap
  end.

days_for(Month, Leap) ->
  case Month of
    jan -> 31;
    feb when Leap == leap -> 29;
    feb -> 28;
    mar -> 31;
    apr -> 30;
    may -> 31;
    jun -> 30;
    jul -> 31;
    aug -> 31;
    sep -> 30;
    oct -> 31;
    nov -> 30;
    dec -> 31
  end.
