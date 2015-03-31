-module(server).
-export([start/2]).

start(Num, LPort) ->
    case gen_tcp:listen(LPort,[
                              % {ip, {127,0,0,1}},
                               list,
                               {active, false},
                               {packet,http}
                              ]) of
        {ok, ListenSock} ->
            io:format("ListenSock: ~p~n", [ListenSock]),
            start_servers(Num, ListenSock),
            {ok, Port} = inet:port(ListenSock),
            Port;
        {error, Reason} ->
            {error, Reason}
    end.

start_servers(0,_) ->
    ok;
start_servers(Num,LS) ->
    spawn(?MODULE,server,[LS]),
    start_servers(Num-1,LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,Data} ->
            Answer = process(Data), % Not implemented in this example
            gen_tcp:send(S,Answer),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.
