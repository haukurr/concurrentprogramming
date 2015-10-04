-module(server).
-export([loop/2, initial_state/1, channel/1]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, users = [], channels = []}.

%% ---------------------------------------------------------------------------

check_user(_,[]) -> false;
check_user(Pid,[Channel | Channels]) ->
    list_to_atom(Channel ++ "_channel") ! {is_here,Pid,self()},
    receive
        yes -> true;
        no -> check_user(Pid,Channels)
    end.

% Function for channel process
channel(St) ->
    receive
        {join, {UserPid,Nick}, Pid} -> 
            Users = St#channel_st.users,
            case lists:keyfind(UserPid,1,Users) of
                false ->
                    NewSt = St#channel_st{users = [{UserPid,Nick} | Users]},
                    Pid ! {ok,NewSt},
                    channel(NewSt);
                _ ->
                    Pid ! {user_already_joined,St}
            end;
        {leave, UserPid, Pid} ->
            Users = St#channel_st.users,
            case lists:keyfind(UserPid,1,Users) of
                false ->
                    Pid ! {user_not_joined,St};
                _ ->
                    NewSt = St#channel_st{users = lists:keydelete(UserPid,1,Users)},
                    Pid ! {ok,NewSt},
                    channel(NewSt)
            end;
        {nick, UserPid, Nick} ->
            Users = St#channel_st.users,
            case lists:keyfind(UserPid, 1, Users) of
                false ->
                    channel(St);
                _ ->
                    NewSt = St#channel_st{users = lists:keyreplace(UserPid,1,Users,{UserPid,Nick})},
                    channel(NewSt)
            end;
        {is_here, UserPid, Pid} ->
            Users = St#channel_st.users,
            case lists:keyfind(UserPid,1,Users) of
                false ->
                    Pid ! no;
                _ ->
                    Pid ! yes
            end;
        {message,Message, Pid} ->
            Channel = St#channel_st.name,
            Users = St#channel_st.users,
            {_, Nick}  = lists:keyfind(Pid,1, Users),
            lists:foreach(
              fun({CurrentPid,_}) ->
                  genserver:requestAsync(CurrentPid, {incoming_msg, "#" ++ Channel, Nick, Message})
              end,
            lists:delete({Pid,Nick},Users))
    end,
    channel(St).

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
            case check_user(Pid,Channels) of
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
    {_, Nick}  = lists:keyfind(Pid,1, St#server_st.users),
    case lists:member(Channel, Channels) of
        false ->
            register(list_to_atom(Channel ++ "_channel"),spawn(server,channel,
                        [#channel_st{users=[{Pid,Nick}],name=Channel}])),
            { ok, St#server_st{channels = [ Channel | Channels] } };
        true ->
            list_to_atom(Channel ++ "_channel") ! {join,{Pid,Nick},self()},
            receive
                {ok,_} -> {ok,St};
                {user_already_joined,_} -> {user_already_joined, St}
            end
    end;

% User leaves a channel.
loop(St, {leave, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:member(Channel, Channels) of
        false      -> { user_not_joined, St }; % Channel does not exist.
        true ->
            list_to_atom(Channel ++ "_channel") ! {leave,Pid,self()},
            receive
                {ok,_} -> {ok,St};
                {user_not_joined,_} -> {user_not_joined, St}
            end
    end;

% The server recieves a message from a client.
loop(St, {msg_from_GUI, [_|Channel], Msg, Pid} ) ->
    Channels = St#server_st.channels,
    case lists:member(Channel, Channels) of
        false -> {user_not_joined, St};
        _ ->
            list_to_atom(Channel ++ "_channel") ! {message, Msg, Pid},
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
    Channels = St#server_st.channels,
    case lists:keyfind(Nick, 2, Users) of
        false ->
            case lists:keyfind(Pid, 1, Users) of
                false -> {user_not_connected, St};
                _ -> 
                    lists:foreach(
                        fun(Channel) ->
                            list_to_atom(Channel ++ "_channel") ! {nick, Pid, Nick}
                        end,
                    Channels),
                    {ok, St#server_st{users = lists:keyreplace(Pid,1,St#server_st.users,{Pid,Nick})}}
            end;
        _ -> {nick_taken, St}
    end;

loop(St, Request) ->
    io:fwrite("Server received: ~p~n", [Request]),
    Response = "hi!",
    io:fwrite("Server is sending: ~p~n", [Response]),
    {Response, St}.
