![chat bubbles](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/chat.png)
## [](#introduction-to-cchat)Introduction to CCHAT

[Check how to install Erlang/OTP in your computer!](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/comp_inf.html)

Building a "real" concurrent system is not easy. In this lab, we aims put you through some of that experience. You will need to make design decisions, evaluate the trade-off between performance and simplicity, face debugging, and much more. While challenging, it is plenty of fun!

### [](#the-topic)The topic

Nowadays, messaging systems are very popular (Whatsapp, Snapchat, Facebook chat, etc.). They provide great connectivity to users as well as different features like stickers, videos clips, etc. In this lab, you will build a simple (but still quite real) text-based messaging system called **CCHAT** (the first _C_ is from _C_halmers). **CCHAT** is very much inspired by IRC, an old but still valid standard designed for group discussions. For simplicity, and reasons of time, your implementation of CCHAT is not going to use IRC's protocol nor low-level TCP/IP communication. Instead, it will leverage Erlang's processes and message passing features.

### [](#a-first-look)A first look

Below, you can find a short video of how **CCHAT** will look and work.

https://www.youtube.com/embed/dLpjP9uxLTc?rel=0

Here, some details about the demo.

*   Command `cchat:server()` starts a chat server called `shire`. For the rest of the lab, users connect to that server.

*   Command `cchat:client()` opens a new client window. The GUI is basic, but the good news is that you do not have to implement it yourselves.

*   The user interface has a `System` tab. This tab has the purpose of showing the activities carried out by different domains as well as _reporting errors_. For instance, when connecting to the `shire`, you saw the following on the system tab:
```
* Trying to connect to shire...
+ Connected!
```
*   The chat server cannot have two users connected using the same name. By default, the GUI utilizes the nickname `user01`. The command `/nick <username>` changes the nickname to `<username>`. The format for `username` is the same as for an [Erlang's atom](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/erlang.html#data_types), i.e., it will start with a lower case letter and followed by letters, numbers, or underscore (`_`). **For this lab, you only need to handle changing the client's nickname when disconnected from the server**.

*   In order to chat, `bilbo` joined to the discussion group (or channel) `#hobbits` with the command `/join #hobbits`. By convention, all the channels' names start with `#`.

*   Then, `frodo` joined to the same channel. After a small, chat `frodo` left the channel using the command `/leave`. Then, he disconnected from the server by issuing the command `/disconnect`. Similarly, `bilbo` executed the same commands and finally closed the application.

## [](#architecture)Architecture

**CCHAT** will use a [client-server architecture](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/clientserver.html). It consists on a client part, which runs the GUI and a client process, and a server which runs the chat service (server process) and host the discussions groups. The graphic below illustrates the situation.

![architecture](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/architecture.png)

In the graphic above, the client process (blue circle with double lines) has the goal to be a _bridge_ between the GUI and the server process (the other blue circle with double lines).

*   **Location of the client and server processes**

    The picture suggests that the client and server processes might be located in different machines. Nevertheless, and for simplicity, _we will consider that all the processes are located on the same local machine._ If you get the implementation of **CCHAT** right, you will be able to easily adapted to run in a [distributed environment.](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/clientserver.html#distributed_environments) This is one of the great aspects of Erlang!

*   **The GUI and the client process**

    The GUI will interact with the client process through a specific protocol (described below). You do not need to implement the GUI, we will give it to you. Instead, **you need to provide a implementation of the client process**. Your implementation should know how to interact with the GUI, i.e., it should follow the GUI protocol (to be explained later on).

*   **The server process**

    The server handles the requests coming from the clients. The protocol being used between a client and the server is up to you. **You have complete freedom to implement the server process as you want.** Take into account that the chat server might be composed by several processes, not only the one that you see in the picture above. **You have to think of the parts of your code in which the fact of having more processes would increase the concurrency of your application.** Remember to keep your code simple.

## [](#the-gui-and-the-client-process)The GUI and the client process

### [](#the-protocol)The protocol

The protocol between the **GUI** and the client process is partly fixed. The reason for that is two fold. Firstly, by following the protocol, you will be able to use the **GUI** without knowing its internal implementation details. Secondly, and more importantly, we will test your code assuming that your client process follow the protocol. **If you do not follow it, your code will not pass the tests (see the test section below) and your submission will be immediately rejected.**

The protocol scheme is as follows.

![Protocol](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/protocol.png)

The **GUI** sends a message `Message` requesting some operations. Then, the **Client** either replies with the atom `ok` or `{error, Atom, Text}`. Atom `ok` indicates that the operation succeeded. Tuple `{error, Atom, Text}` denotes that something went wrong while processing the request. These errors are not fatal and the GUI can recover from them. Variable `Text` contains the text to be reported in the `System` tab of the **GUI**. You are free to choose the value of `Text`. However, you should strictly use the values for `Atom` as described by the protocol.

*   **Connecting to the server**

    When the user writes the command `/connect Server`, the GUI sends the message `{connect, Server}` to the client process, where variable `Server` denotes the name of the chat server chosen by the user.

    Atom `user_already_connected` is used when the user tried to connect but it is already connected to the server. Atom `server_not_reached` is returned when the server process cannot be reached for any reason.

*   **Disconnecting from the server**

    When the user writes the command `/disconnect`, the GUI sends the message `disconnect` to the client process. It is possible to disconnect from a server **only if** the user has left all the chat rooms (i.e. channels) first.

    Atom `user_not_connected` is returned when a user tries to disconnect from a server where he/she is not connected to. Atom `leave_channels_first` is returned when a user tries to disconnect from a server without leaving the chat rooms first. Lastly, atom `server_not_reached` is used when the server process cannot be reached for any reason.

*   **Joining a chat room**

    To join a chat room, users write the command `/join Chatroom`, where `Chatroom` is a string starting with "#", e.g., "#hobbits". The **GUI** sends the message `{join, Chatroom}`.

    Internally, if the chat room does not exists in the server side, the server process will create it. We assume that, once created, chat rooms are never destroyed, i.e., they will always exist as long as the server runs. Bear in mind that only users who have joined a chat room can write messages on it.

    Atom `user_already_joined` is return when a user tries to join to the same channel again.

*   **Writing messages in a chat room**

    When the user is in a chat room and writes an string (not started with `/`), the **GUI** sends the message `{msg_from_GUI, Chatroom, String}` where variable `Chatroom` contains the name of the channel as an string (e.g. "#hobbits") and variable `String` stores the typed string (e.g. "hello fellow hobbits").

    Atom `user_not_joined` is returned when a user tries to write a message in a channel that he/she has not joined.

*   **Leaving a chat room**

    When the user types `/leave` in a chat room, the **GUI** sends the message `{leave, Chatroom}`, where variable `Chatroom` contains the name of the chat room.

    Atom `user_not_joined` is returned when a user tries to leave a channel that he/she has not joined.

*   **Asking for the nickname**

    When the user writes the command `/whoami`, the GUI sends the message `whoami` to the client process. The client should respond with the nick as a string (instead of the atom `ok`). There are no errors to report in this case.

*   **Changing the nickname**

    When the user writes the command `/nick Name`, the GUI sends the message `{nick, Name}` to the client process. Variable `Name` contains the new chosen nickname for the user.

    **To make things easier, you only need to support changing nick when the user is disconnected.** Return atom `user_already_connected` if the user attempts to change nick when connected.

    **Optional**: If you wish to implement changing the nick when connected, you should return atom `nick_taken` when trying to change to nick that is already taken.

Until this point, the protocol describes communications initiated by the **GUI**. There is only one occasion when the client process starts communication with the **GUI**: when something is written to a channel, the client needs to tell the GUI to display the new text. The client process sends the message `{msg_to_GUI, Chatroom, Msg}` when it wishes to print out the line `Msg` in the chat room `Chatroom`.

![Client to GUI](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/clientgui.png)

For that, we use the following line of code (which you do not need to change):

```erlang
gen_server:call(list_to_atom(GUIName), {msg_to_GUI,  Chatroom,  Name++"> "++Msg})
```
Where `Name` is the nick of the author and `Msg` is their message. Observe that `gen_server` is not the same as `genserver` (the module that has been shown in class). Module `gen_server` is the [OTP implementation of a generic server](http://www.erlang.org/doc/man/gen_server.html).

Variable `GUIName` contains the name for the GUI process, which is generated in the `gui.erl` module and passed onto your client in `client:initial_state/2`.

## [](#errors)Errors

![Fatal errors in the protocol](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/protocolerror.png)

*   **Fatal errors**

    The client process has the chance to respond to the **GUI** with the tuple `{'EXIT', Reason}`. This tuple indicates to the **GUI** that something went very wrong on the client side, e.g. when the server process clashes in the middle of processing a request. The **GUI** will display the content of variable `Reason` and exit. You might see this behavior during development but it is clear that it should not appear in your submission when we will test it.

*   **Other errors**

    When developing your solution, you might find yourself wondering "what if this fails?", "what should process A or B do then?", "should I modify my code to catch this error?", etc. If you want to take this road, you will go down the rabbit hole! Try to focus on making your solution work as described by the protocol above. For instance, you might decide to **only** report those errors described above, and making your solution crash if some other error occurs.

## [](#code-skeleton)Code skeleton

[Get the skeleton code here!](https://github.com/Chalmers-TDA382/tda382-lab3-code-skeleton/)

In this lab, you are required to build the client and server processes based on the structure in the skeleton code. The main reason for that is to make it easier to have your lab up and running. You could use any Erlang code that we saw at the lectures if it helps you.

We will give you the following files. <span class="text-danger">Do not edit the ones highlighted in red.</span>

| Component | Files | Description |
| ------------- | ------------- | ------------- |
| GUI | gui.erl lexgrm.erl grm.yrl lex.xrl | These files contain the implementation of the **GUI**. Do not modify them. |
| Testing | test_client.erl dummy_gui.erl | Files used for testing. Do not modify them. |
| Record definitions |  defs.hrl  | This file contain record definitions. The state of the client, server, or any other entity that you wish to add should represent its state as a record. We provide you with some incomplete definitions already. For instance, `-record(client_st, {gui}).` defines the record `client_st` (client state) where field `gui` stores the name of the **GUI**. |
| Client process | client.erl | The exported function `client:loop/2` handles each different kind of request, returning a tuple of the response and the updated state. You also need to implement `initial_state(Nick, GUIName)` which generates the initial state for the client. |
| Server process | server.erl | The exported function `server:loop/2` handling messages from the GUI process in a loop, in a similar way to the client. It is up to you to decide the protocol between the client and the server! You also need to implement `initial_state(ServerName)` which generates the initial state for the server. |
| CCHAT | cchat.erl | This is the top level module. It is used to launch the server and several clients with their respective GUIs. Do not modify this file. |
| Generic Server | genserver.erl | This file contains functions for spawning and running Erlang processes as servers. and implementing synchronous message passing. It is used internally, so do not modify this file... but you might want to use its functions yourselves! |

To download skeleton, click on the button that says "Download ZIP", or you can clone the entire repository using `git clone`.

### [](#compiling-for-the-first-time)Compiling for the first time

After you download it (and extract the files) you should compile everything with the command:

<pre class="prettyprint lang-bash prettyprinted"> <span class="pln">$ make all</span> </pre>

You can then open the Erlang shell (`erl`) and start a client with `cchat:start().`. You should be able to start the server and open up chat windows, but Bilbo and Frodo will not able to communicate because most of the functions are not implemented. It is your task to make sure that they can!

#### [](#alternatives-to-make-)Alternatives to `make`

If you do not have `make` command, you should run the following commands **in the Erlang shell**:

```erlang
cd("the directory where the skeleton code is"). c(lexgrm). lexgrm:start(). cover:compile_directory().  
```

## [](#tips)Tips

*   The server process will be registered (using Erlang's `register` BIF) to an atom of the same name. So when the server name is shire, you can send messages directly to the atom `shire` without knowing the server's process ID. You might also want to check the function `list_to_atom`.

*   From the Erlang shell, use the function `cover:compile_directory/0` to re-compile all Erlang files in the current directory.

## [](#test-cases)Test cases

All unit tests are contained in the file `test_client.erl`. Tests are carried out using EUnit. We have created entries in the `Makefile` to make life easier for you (see below for alternatives to using `make`).

There are positive and negative tests which check that your solution follows the requirements and protocol as specified above. To run these tests, execute the following:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ make</span> <span class="pun">-</span><span class="pln">s run_tests</span></pre>

If your submission fails any of the tests, it is a good indication that your submission will be rejected. However even if you pass all of the tests, that is not a guarantee that your submission will be accepted.

#### [](#robustness-tests)Robustness tests

Included in the unit tests are **robustness tests**, which test your use of concurrency. The `robust_channel` test checks if your chat server can continue to operate when some of your requests are caused to hang. It works as follows (actual values may change):

1.  A server is started and 4 channels are created.
2.  Each channel is joined by 3 users. No user is in more than one channel, making the total number of users 4 × 3 = 12.
3.  Every user sends 2 messages on their respective channel. The test framework will automatically make some of these requests hang.
4.  We count how many messages get through successfully. If you use concurrency correctly, **only** the blocked messages should get stuck, and all others should be delivered successfully.

The `robust_server` test checks how independent your channels are from your server. It does this by killing the server process during an ongoing chat. If this causes the entire CCHAT system to die then the test will fail, which is a sure sign that your solution is not very concurrent.

To run _just_ the robustness tests, use:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ make</span> <span class="pun">-</span><span class="pln">s run_robustness_tests</span></pre>

### [](#alternatives-to-make--1)Alternatives to `make`

If your system doesn't have the `make` command, you can run the test suites like so:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ erl</span> <span class="pun">-</span><span class="kwd">eval</span> <span class="pln"></span> <span class="str">"cover:compile_directory(), eunit:test(test_client), halt()"</span></pre>

### [](#turning-off-colour-codes)Turning off colour codes

If you don't have a colour-enabled terminal, you will see a lot of ugly colour codes in your test output. You can disable these by commenting out the `colour` function in `test_client.erl` and replacing it with:

```erlang
colour(Num,S) -> S.
```

## [](#submission)Submission

You should submit your code based on the skeleton code and whatever other files are needed for your solution to work. In addition, you should comment your code so that graders can easily review your submission.
