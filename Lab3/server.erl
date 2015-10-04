-module(server).
-export([loop/2, initial_state/1, channel/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, users = [], channels = []}.

%% ---------------------------------------------------------------------------

% Function for channel process
channel(Channel) ->
    receive
        {message,Message, Pid, St} ->
            {_, Users} = lists:keyfind(Channel,1, St#server_st.channels),
            {_, Username}  = lists:keyfind(Pid,1, St#server_st.users),
            lists:foreach(
              fun(CurrentPid) ->
                  genserver:requestAsync(CurrentPid, {incoming_msg, "#" ++ Channel, Username, Message})
              end,
            lists:delete(Pid,Users))
    end,
    channel(Channel).

% User connects to server, must have unqiue nickname.
loop(St, {connect, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            {ok,St#server_st{users = [{Pid, Nick} | Users]}};
        _ ->
            {user_already_connected, St}
    end;

% User disconnects from server.
loop(St, {disconnect, Pid}) ->
    Users = St#server_st.users,
    Channels = St#server_st.channels,
    case lists:keyfind(Pid, 1, Users) of
        false -> {user_not_connected, St};
        {_,Nick} ->
            case lists:any(fun(X) -> X end, lists:map(
                fun({_,ChannelUsers}) ->
                     lists:member(Pid,ChannelUsers)
                end,
            Channels)) of
                true ->
                    {leave_channels_first,St};
                false ->
                    {ok,St#server_st{users = lists:delete({Pid,Nick}, Users)}}
            end
    end;

% User joins a channel. If there is no channel with the name one is created.
% Consider that #channelname, where "channelname" must not already be a registered atom.
loop(St, {join, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false ->
            register(list_to_atom(Channel ++ "_channel"),spawn(server,channel,[Channel])),
            { ok, St#server_st{channels = [ {Channel,[Pid] } | Channels] } };
        {_, Users} ->
            case lists:member(Pid,Users) of
                false ->
                    {ok, St#server_st{channels = lists:keyreplace(Channel,1,Channels,{Channel,[Pid | Users]})}};
                true ->
                    {user_already_joined, St}
            end
    end;

% User leaves a channel.
loop(St, {leave, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false      -> { user_not_joined, St }; % Channel does not exist.
        {_, Users} -> 
            case lists:member(Pid, Users) of
                false -> { user_not_joined, St }; % User is not in channel.
                _     -> 
                    {ok, 
                        St#server_st{channels =
                            lists:keyreplace( Channel, 1, Channels, {Channel, lists:delete(Pid, Users)} ) }
                    }
            end
    end;

% The server recieves a message from a client.
loop(St, {msg_from_GUI, [_|Channel], Msg, Pid} ) ->
    Channels = St#server_st.channels,
    case lists:keyfind(Channel, 1, Channels) of
        false -> {user_not_joined, St};
        _ ->
            list_to_atom(Channel ++ "_channel") ! {message, Msg, Pid, St},
            {ok,St}
    end;

% A client ask what name the server has stores for the client.
loop(St,{whoami, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Pid, 1, Users) of
        false -> {user_not_connected, St};
        {_,Nick} ->  {Nick,St}
    end;

% A client changes its Nickname on the server, the new nick must be not taken.
loop(St,{nick, Nick, Pid}) ->
    Users = St#server_st.users,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            case lists:keyfind(Pid, 1, Users) of
                false -> {user_not_connected, St};
                _ -> {ok, St#server_st{users = lists:keyreplace(Pid,1,St#server_st.users,{Pid,Nick})}}
            end;
        _ -> {nick_taken, St}
    end;

loop(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {Response, St}.
