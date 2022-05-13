-module(area_server).
-export([cycle/0]).

cycle() ->
    receive
        {rectangle, Width, Height} ->
            io:format("Area of rectangle = ~p~n", [Width * Height]),
            cycle();
        {circle, R} ->
            io:format("Area of circle = ~p~n", [3.14159 * R * R]),
            cycle();
        stop ->
            io:format("Server was manually stopped.");
        Other ->
            io:format("I don't know the area of ~p~n", [Other]),
            cycle()
    end.