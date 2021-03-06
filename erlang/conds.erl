-module(conds).
-export([month_length/2]).

month_length(Year, Month) -> 
    Leap = if
        Year rem 400 == 0 ->
            leap;
        Year rem 100 == 0 ->
            not_leap;
        Year rem 4 == 0 -> 
            leap;
        true -> 
            not_leap
    end,
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.