-module(client).
-export([loop/2, initial_state/2]).
-include_lib("./defs.hrl").

%% Produce initial state
initial_state(Nick, GUIName) ->
    #client_st { gui = GUIName, nick = Nick, server = undefined}.

%% ---------------------------------------------------------------------------

%% loop handles each kind of request from GUI

%% Connect to server
loop(St, {connect, Server}) ->
    case St#client_st.server of
        undefined ->
            ServerAtom = list_to_atom(Server),
            case whereis(ServerAtom) of
                undefined -> {{error,unknown_host, "Unknown host"}, St};
                _ ->
                    Response = genserver:request(ServerAtom, {connect,St#client_st.nick, self()}),
                    case Response of
                        ok ->          { ok, St#client_st{server = Server} };
                        nick_in_use -> { {error, nick_in_use, "Nick already in use"}, St}
                    end
            end;
        _ -> {{error, already_connected, "Already connected to a server"},St}
    end;

%% Disconnect from server
loop(St, disconnect) ->
    Server = St#client_st.server,
    Error = { {error, not_connected, "You are not connected to a server!"}, St},
    case Server of
        undefined -> Error;
        _ ->
            ServerAtom = list_to_atom(Server),
            case whereis(ServerAtom) of
                undefined -> {{error,unknown_host, "Unknown host"}, St};
                _ ->
                    Response = genserver:request(ServerAtom, {disconnect,St#client_st.nick, self()}),
                    case Response of
                        ok ->            { ok, St#client_st{server = undefined} };
                        not_connected -> Error
                    end
            end
    end;

% Join channel
loop(St, {join, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Leave channel
loop(St, {leave, Channel}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

% Sending messages
loop(St, {msg_from_GUI, Channel, Msg}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Get current nick
loop(St, whoami) ->
    % {"nick", St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Change nick
loop(St, {nick, Nick}) ->
    % {ok, St} ;
    {{error, not_implemented, "Not implemented"}, St} ;

%% Incoming message
loop(St = #client_st { gui = GUIName }, {incoming_msg, Channel, Name, Msg}) ->
    gen_server:call(list_to_atom(GUIName), {msg_to_GUI, Channel, Name++"> "++Msg}),
    {ok, St}.
