-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, server = undefined, channels = []}.

% Requests to server. Checks that the client is connected to the server
% before executing the callback.
request(St,Data,Callback) ->
    Server = St#client_st.server,
    Error = { {error, user_not_connected, "You are not connected to a server!"},
               St#client_st{server = undefined, channels = []}},
    case Server of
        undefined -> Error;
        _ ->
            ServerAtom = list_to_atom(Server),
            Response = genserver:request(ServerAtom, Data),
            case Response of
                user_not_connected -> Error;
                _ ->             Callback(Response)
            end
    end.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
    case St#client_st.server of
        undefined ->
            ServerAtom = list_to_atom(Server),
            case whereis(ServerAtom) of
                undefined -> {{error,server_not_reached, "Unknown host"}, St};
                _ ->
                    Response = genserver:request(ServerAtom, {connect,St#client_st.nick, self()}),
                    case Response of
                        ok ->          { ok, St#client_st{server = Server} };
                        user_already_connected -> { {error, user_already_connected, "Nick already in use"}, St}
                    end
            end;
        _ -> {{error, user_already_connected, "Already connected to a server"},St}
    end;

%% Disconnect from server
loop(St, disconnect) ->
    request(St, {disconnect, self() },
        fun(Response) ->
            case Response of
                ok -> { ok, St#client_st{server = undefined} };
                leave_channels_first -> {{ error, leave_channels_first, "You have to leave all channels"},St}
            end
        end
    );

% Join channel
loop(St, {join, Channel}) ->
    request(St, {join, Channel, self()},
        fun(Response) ->
            Channels = St#client_st.channels,
            case Response of
                user_already_joined ->  {{error, user_already_joined, "Already joined channel!"},St};
                Pid -> { ok, St#client_st{channels = [{Channel,Pid} | Channels]} }
            end
        end
    );

%% Leave channel
loop(St, {leave, Channel}) ->
    request(St, {leave, Channel, self()},
        fun(Response) ->
            Channels = St#client_st.channels,
            UpdatedChannelSt = St#client_st{channels = lists:keydelete(Channel,1,Channels)},
            case Response of
                ok ->              { ok, UpdatedChannelSt};
                user_not_joined ->      {{error, user_not_joined, "You are not on this channel!"}, UpdatedChannelSt}
            end
        end
    );

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    Channels = St#client_st.channels,
    UpdatedChannelSt = St#client_st{channels = lists:delete(Channel, Channels)},
    Error = {{error, user_not_joined, "You are not on this channel!"}, UpdatedChannelSt},
    case lists:keyfind(Channel,1,Channels) of
        false -> Error;
        {_,Pid} ->
            Response = genserver:request(Pid, {message,Msg,self()}),
            case Response of
                ok -> { ok, St};
                user_not_joined -> Error
            end
    end;

%% Get current nick, tries to find it from the server if connected to one
% otherwise it will show the locally stored nick. 
loop(St, whoami) ->
    case St#client_st.server of
        undefined -> { St#client_st.nick,St };
        _ ->
            request(St, {whoami, self()},
                fun(Response) ->
                    case Response of
                        Nick -> { Nick, St }
                    end
                end
            )
    end;

%% Change nick locally and on the server.
loop(St, {nick, Nick}) ->
    NewSt = { ok, St#client_st{nick=Nick} },
    case St#client_st.server of
        undefined -> NewSt;
        _ ->
            request(St, {nick, Nick, self()},
                fun(Response) ->
                    case Response of
                        nick_taken -> { {error, nick_taken, "Nick already in use"}, St};
                        ok ->          NewSt
                    end
                end
            )
    end;

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
