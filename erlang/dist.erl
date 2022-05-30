%% Solutions to Exercise 15 problems
%% Author: Dr. Santiago Conant

-module(dist).
-export([start/1, create_slave/2, send_msg/3, finish/1,
         master/1, slave/2, test/0]).

% transforms an atom name to a short machine name
getNode(Name) -> list_to_atom(
                    atom_to_list(Name)++"@Sebastians-MacBook-Air").

% Creates and starts the master process, as well as 
% registering it with the alias name (an atom).
start(Alias) ->
    register(Alias, spawn(?MODULE, master, [Alias])),
    io:format("master ~p created~n", [Alias]).
              
% asks a Master process to create a slave process on the 
% specified distributed node (if it exists). 
create_slave(Master, Node) ->
    {Master, getNode(Master)} ! {create_slave, getNode(Node)}.
    
% Asks a Master process to forward the Message to its 
% N-th slave process. The slave must display the 
% received message and terminate if it receives the 
% message die.
send_msg(Master, Message, Nslave) ->
    {Master, getNode(Master)} ! {msg, Message, Nslave}.
    
% Asks the Master process to stop. Before terminating, 
% it should eliminate all its slave processes by 
% sending them die messages .
finish(Master) ->
    {Master, getNode(Master)} ! stop.
    
% function to create the master process
master(Alias) -> master(Alias, 0, []).
master(Alias, Counter, SlavesPids) ->
    receive
        {create_slave, Node} -> 
            Alive = net_adm:ping(Node),
            if
                Alive == pang ->
                    io:format("node ~p is down~n",[Node]),
                    master(Alias, Counter, SlavesPids);
                true ->
                    NewCounter = Counter + 1,
                    Pid = spawn(Node, ?MODULE, slave, [Alias, NewCounter]),
                    case rpc:call(Node, erlang, is_process_alive, [Pid]) of
                        true -> 
                            io:format("~p slave ~p created in node ~p~n", [Alias, NewCounter, Node]),
                            master(Alias, NewCounter, SlavesPids++[Pid]);
                        false -> 
                            io:format("node ~p does not exist~n", [Node]),
                            master(Alias, Counter, SlavesPids)
                    end
            end;
        {msg, Message, Nslave} -> 
            if 
                (Nslave > 0) and (Nslave =< Counter) ->
                    Pid = lists:nth(Nslave,SlavesPids),
                    case rpc:call(node(Pid), erlang, is_process_alive,[Pid]) of
                        true ->
                            Pid ! {msg, Message};
                        false ->
                            io:format("~p slave ~p does not exist~n",[Alias, Nslave])
                    end;
                true -> 
                    io:format("~p slave ~p does not exist~n",[Alias, Nslave])
            end,
            master(Alias, Counter, SlavesPids);
        stop -> 
            kill_all(SlavesPids),
            io:format("master ~p has finished~n",[Alias])
    end.
    
% function to create slave processes
slave(Master, N) -> 
    receive
        {msg, die} -> 
            io:format(user, "~p slave ~p has died~n",
                      [Master, N]);
        {msg, Message} ->
            io:format(user, "~p slave ~p received msg: ~p~n",
                      [Master, N, Message]),
            slave(Master, N)
    end.
    
% HELPER FUNCTIONS

kill_all([SlavePid | Rest]) ->
    case rpc:call(node(SlavePid), erlang, is_process_alive,[SlavePid]) of
        true -> 
            SlavePid ! {msg, die},
            kill_all(Rest);
        false ->
            kill_all(Rest)
    end;
kill_all([]) -> bye.

% test cases
test() -> 
    start(master),
    create_slave(master, e1),
    create_slave(master, e2),
    send_msg(master, hello, 2),
    send_msg(master, die, 1),
    create_slave(master, e1),
    send_msg(master, hi, 1),
    create_slave(master, e3),
    send_msg(master, greetings, 3),
    finish(master).


