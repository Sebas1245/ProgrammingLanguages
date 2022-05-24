-module(hello).
-export([test_hello/0, hello/0]).

test_hello() ->
    H = spawn(?MODULE, hello, []),
    test_hello(10, H).
test_hello(N, H) when N > 0 ->
    H ! {hi, self()},
    receive
        {reply, C} -> 
            io:format("Received ~w~n", [C]), 
        test_hello(N-1, H)
    end;
test_hello(_, _) ->
    io:format("My work is done").

hello() -> hello(0).
hello(Counter) ->
    receive
        {hi, P} -> 
            NewCounter = Counter + 1,
            P ! {reply, NewCounter},
            hello(NewCounter)
    end.