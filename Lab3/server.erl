-module(server).
-export([loop/2, initial_state/1, channel/0]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, users = [], channels = []}.

%% ---------------------------------------------------------------------------

% Function for channel process
channel() ->
    receive
        {message,Message, Pid} ->
            Pid ! {stuff,"HELLO WORLD"}
    end,
    channel().

% User connects to server, must have unqiue nickname.
loop(St, {connect, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            {ok,St#server_st{users = [{Pid, Nick} | Users]}};
        _ ->
            {nick_in_use, St}
    end;

% User disconnects from server.
loop(St, {disconnect, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Pid, 1, Users) of
        false -> {not_connected, St};
        {_,Nick} -> {ok,St#server_st{users = lists:delete({Pid,Nick}, Users)}}
    end;

% User joins a channel. If there is no channel with the name one is created.
% Consider that #channelname, where "channelname" must not already be a registered atom.
loop(St, {join, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false ->
            register(list_to_atom(Channel),spawn(server,channel,[])),
            list_to_atom(Channel) ! {message,"HELLO",self()},
            receive
                _ -> io:fwrite("RECEIVED")
            end,
            { ok, St#server_st{channels = [ {Channel,[{Pid}] } | Channels] } };
        {_, Users}-> {ok, St#server_st{users = lists:keyreplace(Channel,1,Channels,{Channel,[Pid | Users]})}}
        % TODO: replace "users = ?" with "channels = ?".
    end;

% User leaves a channel.
loop(St, {leave, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false      -> { user_not_joined, St }; % Channel does not exist.
        {_, Users} -> 
            case lists:keyfind(Pid, 1, Users) of
                false -> { user_not_joined, St }; % User is not in channel.
                _     -> 
                    {ok, 
                        St#server_st{channels = 
                            lists:keyreplace( Channel, 1, Channels, {Channel, lists:keydelete(Pid, 1, Users)} ) }
                    }
            end
    end;
    
% The server recieves a message from a client.
loop(St, {msg_from_GUI, [_|Channel], Msg, Pid} ) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false -> {not_joined, St};
        _ ->
            %list_to_atom(Channel) ! {message, Msg, Pid},
            {ok,St}
    end;

% A client ask what name the server has stores for the client.
loop(St,{whoami, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Pid, 1, Users) of
        false -> {not_connected, St};
        {_,Nick} ->  {Nick,St}
    end;

% A client changes its Nickname on the server, the new nick must be not taken.
loop(St,{nick, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            case lists:keyfind(Pid, 1, Users) of
                false -> {not_connected, St};
                _ -> {ok, St#server_st{users = lists:keyreplace(Pid,1,St#server_st.users,{Pid,Nick})}}
            end;
        _ -> {nick_in_use, St}
    end;

loop(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {Response, St}.
