% This record defines the structure of the client process.
% Add whatever other fields you need.
% It contains the following fields:
%   gui: the name (or Pid) of the GUI process.
%   nick: the string name of the client.
%   server: the string name of the server.
%   channels: a list of tuples with the {name, pid} of the channel.
-record(client_st, {gui, nick, server, machine,channels}).

% This record defines the structure of the server process.
% Add whatever other fields you need.
% It contains the following fields:
%   name: the string name of the server.
%   users: a list of tuples with the {pid, name} of the users.
%   channels: a list of strings with the registered channel names.
-record(server_st, {name, users, channels}).

% This record defines the structure of the channel process.
% It contains the following fields:
%   name: the string name of the channel.
%   users: a list of tuples with the {pid, name} of the users.
-record(channel_st, {name,users}).
