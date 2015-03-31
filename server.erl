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

start_servers(Num, ListenSock) ->
    start_servers(Num, ListenSock, [], 0).
start_servers(Num, ListenSock, ServerList, Active)
    when Active >= Num ->
        start_servers(Num, ListenSock, ServerList, countServers(ServerList));
start_servers(Num, ListenSock, ServerList, _) ->
    io:format("Bringing server up~n"),
    NewPid = spawn(fun() -> server(ListenSock) end),
    start_servers(Num, ListenSock, [NewPid|ServerList], countServers(ServerList)).

countServers(ServerList) ->
    lists:foldl(
        fun(ServerPid, Acc) ->
            case process_info(ServerPid) of
                undefined -> Acc;
                _ -> Acc + 1
            end
        end,
        0,
        ServerList
     ).

server(ListenSock) ->
    io:format("Starting Server: Pid ~p~n", [self()]),
    case gen_tcp:accept(ListenSock) of
        {ok, Socket} ->
            loop(Socket),
            server(ListenSock);
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

loop(Socket) ->
    inet:setopts(Socket,[{active,once}]),
    receive
        {http, Socket, Data} ->
            io:format("Socket ~p~n", [Socket]),
            io:format("Server ~p got message: ~p~n", [self(), Data]),
            %Headers = digest(Data),
            Answer = "Hello World",% process(Data), % Not implemented in this example
            gen_tcp:send(Socket, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
            gen_tcp:send(Socket, Answer),
            loop(Socket);
        Msg ->
            io:format("Server ~p got unknow message: ~p~n", [self(), Msg]),
            ok
    end.
