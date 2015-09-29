![chat bubbles](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/chat.png)

## [](#introduction-to-dchat)Introduction to DCHAT

This lab is basically an adaptation of CCHAT (Lab 3) for a distributed environment. When it comes to make your code distributed, Erlang is pretty good at it. Here, you will put that to the test!

## [](#arquitecture)Arquitecture

**DCHAT** will use the same arquitecture as **CCHAT**, i.e., a [client-server architecture](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/clientserver.html). However, now you client process (and the GUI) should be able to communicate with your server located in another Erlang node (maybe in a different computer).

*   The GUI and the client process

    The GUI will interact with the client process as before except that the CCHAT protocol is extended with one additional case (see below).

*   The server process

    The server handles the requests coming from the clients as before, but it should be able to reply to a client located in another Erlang node (perhaps in another computer).

## [](#the-gui-and-the-client-process)The GUI and the client process

### [](#the-protocol)The protocol

As before, the protocol between the **GUI** and the client process is partly fixed and follows the same scheme as in **CCHAT**. Below, we briefly describe the extra case.

![Protocol](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/assets/img/protocol.png)

*   **Connecting to a remote server**

    When the user writes the command `/connect {Server,Machine}`, the GUI sends the message `{connect, {Server, Machine} }` to the client process, where variable `Server` denotes the name of the chat server chosen by the user, `Machine` the computer to connect to (it has the shape `'name@IP'`, e.g., `'chalmers_irc@127.0.0.1'`).

    Variable `Atom` gets the follows values. Atom `user_already_connected` is used when the user tried to connect but it is already connected to the server. Atom `server_not_reached` is returned when the server process cannot be reached for any reason.

*   **Connecting to a local server**

    **As in CCHAT,** you can still use the command `/connect Server` which sends `{connect, Server}` to the client. In this case you should assume that your are connecting to a server on the same node as the client & GUI.

## [](#code-skeleton)Code skeleton

You should use the same one given to **CCHAT**. [You can get the code skeleton here](https://github.com/Chalmers-TDA382/tda382-lab3-code-skeleton/)

## [](#requirements)Requirements

*   All the requirements from **CCHAT** still apply for this lab.

## [](#test-cases)Test cases

Now, your software **should pass the same test cases as before** together with some extra ones designed to check if you adapted your code to work in a distributed environment. The test cases are given in the same testing framework (eunit) as we did previously.

The distributed tests are contained in the file `test_remote.erl` (there is currently only one test case). To run these tests, make sure to get the latest version of the code skeleton and execute the following:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ make</span> <span class="pun">-</span><span class="pln">s run_distributed_tests</span></pre>

You should see the following output:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ make</span> <span class="pun">-</span><span class="pln">s run_distributed_tests</span> <span class="typ">Erlang</span> <span class="pln">R16B03</span> <span class="pun">(</span><span class="pln">erts</span><span class="pun">-</span><span class="lit">5.10</span><span class="pun">.</span><span class="lit">4</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">[</span><span class="pln">source</span><span class="pun">]</span> <span class="pln"></span> <span class="pun">[</span><span class="pln">smp</span><span class="pun">:</span><span class="lit">4</span><span class="pun">:</span><span class="lit">4</span><span class="pun">]</span> <span class="pln"></span> <span class="pun">[</span><span class="pln">async</span><span class="pun">-</span><span class="pln">threads</span><span class="pun">:</span><span class="lit">10</span><span class="pun">]</span> <span class="pln"></span> <span class="pun">[</span><span class="pln">kernel</span><span class="pun">-</span><span class="pln">poll</span><span class="pun">:</span><span class="pln">false</span><span class="pun">]</span> <span class="pln"></span> <span class="typ">Eshell</span> <span class="pln">V5</span><span class="pun">.</span><span class="lit">10.4</span> <span class="pln"></span> <span class="pun">(</span><span class="pln">abort with</span> <span class="pun">^</span><span class="pln">G</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">(</span><span class="pln">testsuite@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span><span class="pun">)</span><span class="lit">1</span><span class="pun">></span> <span class="pln"></span> <span class="com"># Test: one_client</span> <span class="pln">start server node</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">server startup</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">start client node client_50150</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client startup client_50150</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client_50150@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">connects to server as user_72305</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client_50150@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">joins</span> <span class="com">#channel_44359: Ok</span> <span class="pln">start client node client_91566</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client startup client_91566</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client_91566@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">connects to server as user_31133</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client_91566@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">joins</span> <span class="com">#channel_44359: Ok</span> <span class="pln">client_50150@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">sends message on</span> <span class="com">#channel_44359: Ok</span> <span class="pln">channel matches</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">message matches</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln">client_91566@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">leaves</span> <span class="com">#channel_44359: Ok</span> <span class="pln">client_50150@127</span><span class="pun">.</span><span class="lit">0.0</span><span class="pun">.</span><span class="lit">1</span> <span class="pln">sends message on</span> <span class="com">#channel_44359: Ok</span> <span class="pln">no more messages</span><span class="pun">:</span> <span class="pln"></span> <span class="typ">Ok</span> <span class="pln"></span> <span class="typ">Test</span> <span class="pln">passed</span><span class="pun">.</span></pre>

If you have added extra modules, make sure that they are also compiled (e.g. by adding them to the makefile).

### [](#alternatives-to-make-)Alternatives to `make`

If your system doesn't have the `make` command, you can run the test suite like so:

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ killall beam</span><span class="pun">.</span><span class="pln">smp
$ erl</span> <span class="pun">-</span><span class="pln">name</span> <span class="str">"testsuite@127.0.0.1"</span> <span class="pln"></span> <span class="pun">-</span><span class="kwd">eval</span> <span class="pln"></span> <span class="str">"cover:compile_directory(), eunit:test(test_remote), halt()"</span></pre>

## [](#submission)Submission

You should submit your code based on the skeleton code and whatever other files are needed for your solution to work. In addition, you should comment your code so that graders can easily review your submission.
