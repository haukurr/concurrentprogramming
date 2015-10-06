-module(server).
-export([loop/2, initial_state/1, channel/2]).
-include_lib("./defs.hrl").

% Produce initial state
initial_state(ServerName) ->
    #server_st{name = ServerName, users = [], channels = []}.

%% ---------------------------------------------------------------------------

% Function for channel process

%Join channel
channel(St, {join, UserPid, Nick}) ->
    Users = St#channel_st.users,
    case lists:keyfind(UserPid,1,Users) of
        false ->
            {ok,St#channel_st{users = [{UserPid,Nick} | Users]}};
        _ -> {user_already_joined,St}
    end;

%Leave channel
channel(St, {leave, UserPid}) ->
    Users = St#channel_st.users,
    case lists:keyfind(UserPid,1,Users) of
        false -> {user_not_joined,St};
        _ -> {ok,St#channel_st{users = lists:keydelete(UserPid,1,Users)}}
    end;

%Change nickname on channel
channel(St, {nick, UserPid, Nick}) ->
    Users = St#channel_st.users,
    case lists:keyfind(UserPid, 1, Users) of
        false -> {ok,St};
        _ -> {ok,St#channel_st{users = lists:keyreplace(UserPid,1,Users,{UserPid,Nick})}}
    end;

%Check if user is on channel by pid
channel(St, {is_here, UserPid}) ->
    Users = St#channel_st.users,
    case lists:keyfind(UserPid,1,Users) of
        false -> {no,St};
        _ -> {yes,St}
    end;

%Send a message to all users on a channel
channel(St, {message, Message, Pid}) ->
    Channel = St#channel_st.name,
    Users = St#channel_st.users,
    case lists:keyfind(Pid,1,Users) of
        false -> {user_not_joined, St};
        {_, Nick}  ->
            lists:foreach(
                fun({CurrentPid,_}) ->
                    spawn(fun() ->
                        genserver:requestAsync(CurrentPid, {incoming_msg, "#" ++ Channel, Nick, Message})
                    end)
                end, lists:delete({Pid,Nick},Users)), %Skip the owner of the msg
            {ok,St}
    end.

%Wrapper for sending requests to channels
channelRequest(Channel,Data) ->
    genserver:request(list_to_atom(Channel ++ "_channel"),Data).

%Wrapper for sending requests to channels asynchronously
channelRequestAsync(Channel,Data) ->
    genserver:requestAsync(list_to_atom(Channel ++ "_channel"),Data).

%Checks if user is in any of the given channels
user_in_a_channel(_,[]) -> false;
user_in_a_channel(Pid,[Channel | Channels]) ->
    case channelRequest(Channel,{is_here,Pid}) of
        yes -> true;
        no -> user_in_a_channel(Pid,Channels)
    end.

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
            case user_in_a_channel(Pid,Channels) of
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
            genserver:start(list_to_atom(Channel ++ "_channel"), 
                #channel_st{users=[{Pid,Nick}],name=Channel},
                fun server:channel/2),
            { whereis(list_to_atom(Channel ++ "_channel")), St#server_st{channels = [ Channel | Channels] } };
        true ->
            case channelRequest(Channel,{join,Pid,Nick}) of
                user_already_joined -> {user_already_joined,St};
                _ -> {whereis(list_to_atom(Channel ++ "_channel")), St}
            end
    end;

% User leaves a channel.
loop(St, {leave, [_|Channel], Pid}) ->
    Channels = St#server_st.channels,
    case lists:member(Channel, Channels) of
        false      -> { user_not_joined, St }; % Channel does not exist.
        true ->
            case channelRequest(Channel,{leave,Pid}) of
                user_not_joined -> {user_not_joined, St};
                _ -> {ok,St}
            end
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
                    lists:foreach(fun(Channel) -> channelRequestAsync(Channel,{nick,Pid,Nick}) end, Channels),
                    {ok, St#server_st{users = lists:keyreplace(Pid,1,St#server_st.users,{Pid,Nick})}}
            end;
        _ -> {nick_taken, St}
    end.

