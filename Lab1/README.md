## [](#deadlines)Deadlines

[Information about how to run the lab remotely or in Windows can be found here.](./comp_inf.html)

## [](#lab-1)Lab 1

In this assignment you will write a program to control two trains. The purpose is to write the program so that the trains move relatively independently of each other, but without colliding. Instead of a real railway, we have a simulator, where one can control the speeds of trains and change rail switches. The simulator gives a signal when a train passes sensors suitably placed on the tracks. It is part of the assignment to decide where sensors should be placed to give sufficient information for your program. The map without sensors looks as follows.

![](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/map.gif)

### [](#choice-of-programming-language)Choice of programming language

In this assignment you must use Java.

### [](#important)Important

In order to make sure you are using the right version of Java and train simulator, do not forget to run

```
setup_course tda381
```

on Chalmers GNU/Linux machines.

* * *

### [](#assignment)Assignment

Your assignment is to write a program that controls the trains. The trains should run simultaneously between the two stations (_note that each station has two tracks_). It is important that the trains are able to move at the same time, and be as independent from each other as possible, without crashing. At the stations the trains should stop and then wait 1 to 2 seconds. The trains should run forever between the stations.

In the beginning there are no sensors on the tracks. You must find where it is suitable to place sensors so that the control program gets the necessary information.

The program that you deliver should work as follows. Each train should be controlled by an independent _train program_, which runs in its own Java thread. Train programs cannot communicate with outside world by anything else than acquiring and releasing semaphores. In particular, no shared variables should be read or changed by the train programs. Also, both trains must use the same 'train program', and it is not allowed to make the behaviour of a train depend on its train id. For example, the track that a train chooses should not depend on its train id.

Below is a list of more detailed requirements.

### [](#requirements)Requirements

*   **Command line parameters**. Your program must take three optional parameters from the command line that represent the speed for each train, and the speed of the simulation. For instance, the line

    ```
      $ 2 "tsim --speed 20 Lab1.map" "java Lab1 10 5 20"
    ```

    will launch the simulation at speed `20` with the top train at speed `10` and the bottom train at speed `5`. If the first or second parameter is missing, then you may pick any speed in the range `[1,maxspeed]`, and if the third parameter is missing, you must default to `100` as this is the default for the simulator as well.

*   **Accounting for speed at stations**. The trains must wait 1-2 seconds at each station after coming to a full stop. Use the following formula (time in ms): `time_at_station + 2 * simulation_speed_parameter * |train_speed|`

*   **Maximum train speed**. You need to find out the maximum train speed `maxspeed` that is usable with your particular solution. This must be at least `15`.

*   **No minimal speed**. There should be no assumption about minimal train speed. All speeds in the range `[1,maxspeed]` will be tried.

*   **No excessive semaphores**. Solutions that use `10` or more semaphores will be rejected.

*   **No Polling**. Your solutions must not use _busy-waiting-loops_ or their close relative, <mark><a href="">polling</a></mark>.

*   **Dynamic behaviour**. You must not statically encode how the trains choose track at the switches (for example: train 1 always takes the upper path in the middle, while train 2 always takes the lower; same for stations). You must let the trains have one track as default (for example: the track with shortest distance). You must select the other track if the default track is already occupied by the other train. Note that you should not try to find a solution which works for any track layout or any number of trains, as this will be too complicated for all of us! It is enough to solve the problem for the given track layout.

*   **No Map Modifications**. You may not alter the map in any way, other than by adding sensors.

*   **No randomization**. You may not use randomization in any way that affects the simulation, as this makes testing more difficult.

*   **Two trains – two threads**. When the system is running there should be only two processes (threads): one for each train. This is in addition to the main thread and the thread that handles communication with the simulator. This latter thread is created in the interface code we provide you with.

*   **Two trains – one implementation**. The solution must use the same implementation for both trains. The only thing separating the processes of the different trains is the train id, starting position, and speed. The fact that the two trains start in different positions means that it is permitted (and necessary) to have different initial behaviour for the two processes, but after that the behaviour of the processes _must not be dependent_ on which train it is acting for.

*   **Use binary semaphores**. You must use the provided binary semaphore for mutual exclusion. You will have the opportunity to use other synchronisation mechanisms in the other assignments.

*   **Trains mind their own business**. Trains should operate independently. A train should not make plans/decisions for the other train. Further, a train should not make plans/decisions for itself based on the information (speed, position, direction) of the other one.

*   **Good Train Flow**. The parallel section in the middle of the map must allow a fast train to overtake a slow train going in the same direction.

*   **Documentation**. In addition to working code you need to provide a convincing argument why your solution is correct. You must document your solution **with an ASCII text file**. Give a high-level description of your program and then go into detail about all pieces of code that are essential for correctness. The documentation should include everything needed for easy understanding of the solution. In particular we demand that your documentation contains discussion on:

    *   Placement of the sensors
    *   Choice of critical sections
    *   Maximum train speed and the reason for it
    *   How you tested your solution
*   **Code quality**. Reasonable code quality is expected; do not complain if your assignment is rejected because we think your program is ugly (magic numbers, poor use of types, cut-and-paste coding style) – even if it works.

*   **Submission**. When you submit your solution, you must provide the following files:

    *   `Lab1.java` (your program to control the trains)
    *   `Lab1.map` (which indicates the placement of your sensors)
    *   `documentation.txt` (ASCII file containing your documentation)

* * *

## [](#train-simulator)Train simulator

The simulator is called `tsim` and with this program you can even modify the train map. You can run `tsim` as follows:

*   `tsim`

    *   Make a new, empty map, of default size.
*   `tsim filename`

    *   Read in an old map from file `filename`.
*   `tsim --speed 50 filename`

    *   Read in an old map from file `filename`, and also set the simulation speed to `50`. This value is the number of milliseconds that `tsim` waits between updates of its state (i.e moving the trains). `100` ms is the default, so `50` makes the simulation go at double speed.
*   `tsim --priority 15 --speed 20 filename`

    *   For extreme simulation speeds you may need to also set the priority of the simulator process, since otherwise it might move the trains without your controller program getting time to receive sensor events. The priority value is the [nice value](http://en.wikipedia.org/wiki/Nice_(Unix)).
*   `tsim --help`

    *   See all options of the simulator.

### [](#-tsim-as-a-map-editor)`tsim` as a map editor

You need to get your own copy of the map in order to be able to add the sensors. The map you will be working on is that shown above.

The map file is found in the [downloads section](./lab1.html#downloads). Download it to your working directory (we strongly recommend that you create a subdirectory `lab1` for your work with this assignment).

Then you can modify the map by writing `tsim Lab1.map`. The figure below shows how to use `tsim`. You should _only_ select the sensor button in the tools window (the small T-like symbol) and then use the mouse to place sensors on the track.

![](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/tsim.gif)

### [](#-tsim-as-a-simulator)`tsim` as a simulator

You may start `tsim` and interact with it using commands on standard input. Try for example the commands

```
SetSpeed 1 20
SetSwitch 17 7 RightSwitch
```

and see what happens. (The second command is necessary for the train to survive the first switch which initially is set in the wrong direction). The `tsim` language interfaces handle the communication with tsim and let you change the speed of the trains and the state of the switches in a more convenient way.

Here are some things to note about the simulator:

*   When you give a new speed for a train, it takes a while for the train to increase or decrease from its old to its new speed. This means that the train's braking distance puts limits on how fast they could drive.

*   The trains can not turn around, but can be given a negative speed to travel backwards.

*   You are not able to give a speed which implies that a train changes direction. To change direction the train must first be stopped (by giving it speed 0). There is no signal from the simulator to tell you when the train has stopped, so you will need to wait a suitable time before changing direction. This is approximately `2 * simulation_speed_parameter * |train_speed|`.

*   You must be careful when you change the switches. If a train comes to a switch which is in the wrong position, then the train will derail. See the figure below.

![](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/switch.gif)

### [](#how-your-program-communicates-with-the-simulator)How your program communicates with the simulator

The simulator and your program run as separate OS processes. They use pipes to communicate. We will have two-way communication, so we will need two pipes. See the figure below.

![](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/communication.gif)

You can join together two programs in the above way with the special command `2`. This command starts the two commands which are given as arguments, and joins the output (stdout) of one to the input (stdin) of the other.

```
$ 2 "tsim Lab1.map" "java Lab1 10 5"
```

where `Lab1` is the class containing your solution.

### [](#note-on-debugging)Note on debugging

Since both the standard I/O channels are in use, if you want to write something to the screen (for example, to help with debugging), you must use `System.err.println(String s)`.

* * *

## [](#program-simulator-interaction)Program/Simulator interaction

All commands between the your program and the simulator are sent line-by-line as text strings, as you saw above. To aid you in your assignment we provide you with a Java library (`TSim`) that implements the protocol `tsim` is using. You must use this Java library. The package provides you with an interface to control the behaviour of the trains and switches. The package is available in the [downloads section](./lab1.html#downloads) below.

The Java `TSim` contains a collection of classes to be used when communicating with the simulator from Java. You can view the JavaDoc documentation of the all interface classes [here](http://www.cse.chalmers.se/~cajohn/stuff/TSim/doc/).

### [](#usage)Usage

You cannot create instances of `TSimInterface` using its constructor, since it is private. Instead you use the static method `getInstance()` that creates an instance of the class if that has not already been done (singleton pattern).

<pre class="prettyprint lang-java prettyprinted"><span class="typ">TSimInterface</span> <span class="pln">tsi</span> <span class="pun">=</span> <span class="pln"></span> <span class="typ">TSimInterface</span><span class="pun">.</span><span class="pln">getInstance</span><span class="pun">()</span> <span class="pln"></span> <span class="pun">;</span></pre>

### [](#interesting-classes)Interesting classes

*   `TSimInterface` is the class used to control the trains. It contains methods to control the speed of the train and to switch the positions of the switches. All these methods throw a `CommandException` if some tsim-related error occurs. In addition there is a method to turn on debugging, which displays the communication between your program and `tsim`. Read the Javadoc documentation of this class carefully!

*   SensorEvent represents the event of a train passing over a sensor. Every time a train passes over a sensor first an `ACTIVE` event is created followed by an `INACTIVE` event when the train leaves the sensor. This class contains the status of the sensor, the position of the sensor and the identity of the train causing the event.

*   `CommandException` represents a failure of one of the methods of `TSimInterface`. Information on the error is stored in the exception and can be acquired by the `getMessage` method. You should _never_ catch an exception leaving the exception handler empty! For your own and our best _always_ print the error message contained in the exception object. Otherwise, your program will be much harder to debug.

    <pre class="prettyprint lang-java prettyprinted"><span class="kwd">try</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln"></span> <span class="pun">...</span> <span class="pln"></span> <span class="pun">}</span> <span class="pln"></span> <span class="kwd">catch</span> <span class="pln"></span> <span class="pun">(</span><span class="typ">CommandException</span> <span class="pln">e</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln"></span> <span class="typ">System</span><span class="pun">.</span><span class="pln">err</span><span class="pun">.</span><span class="pln">println</span><span class="pun">(</span><span class="pln">e</span><span class="pun">.</span><span class="pln">getMessage</span><span class="pun">());</span> <span class="pln"></span> <span class="com">/* If a command failed then something bad has
         happened and we probably don't want to
         continue executing */</span> <span class="pln"></span> <span class="typ">System</span><span class="pun">.</span><span class="pln">exit</span><span class="pun">(</span><span class="lit">1</span><span class="pun">);</span> <span class="pln"></span> <span class="pun">}</span></pre>

## [](#downloads)Downloads

To start you need to download these two files:

*   A [small example](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/downloads/Lab1.java) that starts one of the trains to get you started

*   [The map](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/downloads/Lab1.map) to place your sensors on

If you wish to work on this assignment on your own computer, then you can download the files needed using the links below. Note that we do not have any experience trying to make the simulator work on windows, it will probably be quite difficult and we do not provide any help for that, but it is known to work on Mac and Linux. However, should you succeed, we would be happy to hear about it. In this case, you will also need these files:

*   The Java [`TSim` source](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/downloads/Tsim-java.tar.gz), containing the `TSim` interface

*   The simulator [source code](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/downloads/tsim-0.84.tar.gz). To install it, run

    ```
    $ ./configure
    $ make
    $ make install
    ```

    The default installation path is `/usr/local`. If you want to install to some other location instead run `./configure --prefix=/your/location`. For more information, see the INSTALL file.

*   The source of the [2 command](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/2.c). Compile it by writing

    ```
    $ gcc -o 2 2.c
    ```

To unzip and untar the packages use for example (or read the man page of `tar` and `gzip`):

<pre class="prettyprint lang-bash prettyprinted"><span class="pln">$ gzip</span> <span class="pun">-</span><span class="pln">dc filename_goes_here</span><span class="pun">.</span><span class="pln">tar</span><span class="pun">.</span><span class="pln">gz</span> <span class="pun">|</span> <span class="pln">tar xvf</span> <span class="pun">-</span></pre>

* * *

## [](#tips-and-tricks)Tips and Tricks

*   Which parts of track are critical resources?

*   There are two common ways of writing the train programs. One is that the train remembers in which place on the track it is and performs its actions based on that (stateful solution). The other is that the train decides what to do based on the coordinates of the sensor it steps on (stateless solution). In the stateless solution, you might still have to remember some things, for example which semaphors you are holding.

*   Do not try to solve the general problem. Create a solution that works for this particular exercise. For example, it is OK to assume that there will be only 2 trains on the map. If you are interested in solving the general problem, write a short description of how to solve it in the documentation file.

*   If you have to lock two critical sections at the same time, this probably indicates that your train flow will be poor. Perhaps you need a little bit more sensors?

*   It is not necessary for trains to stop exactly at the end of the track at a station (before changing direction).

*   Do not overoptimise modularity. The assignment is simple enough to have the solution in one file.

*   For debugging, it is helpful to print out statements about the state of the trains and semaphores every time it is changed.

*   For testing, run multiple instances of the simulator (at a high simulation speed), with different train speeds for a long time. e.g Test your trains with the first going very fast and the second going very slow, and vice versa. Don't set the simulation speed too high (`--speed` switch should be about 15 or higher), since the trains might not get commands from your program on time. Use the `--priority` switch when running the simulation fast, as outlined earlier.

*   Note that `get_sensor(TId)` **blocks** until the given train (`TId`) next passes a sensor.

*   In previous courses, the best solutions had usually less than 300 lines of code. If your solution is much bigger than that then you are probably making it too complicated. Try making it simpler. It is very important that your code is understandable.

*   There is no need to have more than a single try-catch in the entire program ([more information](http://download.oracle.com/javase/tutorial/essential/exceptions/declaring.html)).

## [](#note-)Note!

It was stated above but may be stated again: Reasonable code quality is expected. You are programmers – programming should be an art to you. The quality of your code will be vital for its reuse and maintenance.

## [](#common-reasons-for-rejection)Common reasons for rejection

### [](#polling-busy-waiting)Polling/busy waiting

What is polling and what is not is a hard question. We here give some examples in pseudo code. Loops that reduce to something similar to the situation below (where the dots do not include any _blocking wait_) must be considered as polling.

<pre class="prettyprint lang-java prettyprinted"><span class="kwd">while</span> <span class="pln"></span> <span class="pun">(</span><span class="pln">e</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln"></span> <span class="pun">#</span> <span class="pln">POLLING</span><span class="pun">!</span> <span class="pln"></span> <span class="pun">...</span> <span class="pln">sleep</span><span class="pun">(</span><span class="pln">t</span><span class="pun">);</span> <span class="pln"></span> <span class="pun">...</span> <span class="pln"></span> <span class="pun">}</span></pre>

Using a blocking operation within a loop is not considered as polling.

<pre class="prettyprint lang-java prettyprinted"><span class="kwd">while</span> <span class="pln"></span> <span class="pun">(</span><span class="pln">e</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln"></span> <span class="pun">#</span> <span class="pln">NO POLLING</span><span class="pun">!</span> <span class="pln"></span> <span class="pun">...</span> <span class="pln">wait</span><span class="pun">(</span><span class="pln">o</span><span class="pun">);</span> <span class="pln"></span> <span class="pun">...</span> <span class="pln"></span> <span class="pun">}</span></pre>

if _it is not the case_ that something is regularly waking the process from its wait. Thus, the following example must be considered as an instance of polling:

<pre class="prettyprint lang-java prettyprinted"><span class="pln">obj o</span><span class="pun">;</span> <span class="pln">process a</span> <span class="pun">{</span> <span class="pln"></span> <span class="kwd">while</span> <span class="pln"></span> <span class="pun">(</span><span class="kwd">true</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln">sleep</span><span class="pun">(</span><span class="pln">t</span><span class="pun">);</span> <span class="pln">notify</span><span class="pun">(</span><span class="pln">o</span><span class="pun">);</span> <span class="pln"></span> <span class="pun">}</span> <span class="pln"></span> <span class="pun">}</span> <span class="pln">process b</span> <span class="pun">{</span> <span class="pln"></span> <span class="pun">#</span> <span class="pln">POLLING</span><span class="pun">!</span> <span class="pln"></span> <span class="kwd">while</span> <span class="pln"></span> <span class="pun">(</span><span class="pln">e</span><span class="pun">)</span> <span class="pln"></span> <span class="pun">{</span> <span class="pln">wait</span><span class="pun">(</span><span class="pln">o</span><span class="pun">);</span> <span class="pln"></span> <span class="pun">}</span> <span class="pln"></span> <span class="pun">}</span></pre>

* * *

<footer>Concurrent Programming 2015 - Chalmers University of Technology & Gothenburg University</footer>

</div>
