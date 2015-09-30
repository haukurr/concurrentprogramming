-module(server).
-export([loop/2, initial_state/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, users = []}.

%% ---------------------------------------------------------------------------

loop(St, {connect, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            {ok,St#server_st{users = [{Pid, Nick} | Users]}};
        _ ->
            {nick_in_use, St}
    end;

loop(St, {disconnect, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:member({Pid, Nick}, Users) of
        true -> {ok,St#server_st{users = lists:delete({Pid,Nick}, Users)}};
        _ -> {not_connected, St}
    end;

loop(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {Response, St}.
