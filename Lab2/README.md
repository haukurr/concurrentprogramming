## [](#deadlines)Deadlines

## [](#lab-2)Lab 2

In this assignment, you will implement a variation of the program to control the two trains that uses monitors rather than semaphores.

![](http://www.cse.chalmers.se/edu/year/2013/course/TDA382_Concurrent_Programming_HT2013/assignments/1/map.gif)

### [](#choice-of-programming-language)Choice of programming language

In this assignment you must use Java.

* * *

### [](#assignment)Assignment

Your assignment is to adapt the program that controls the trains from the [Lab 1](http://www.cse.chalmers.se/edu/year/2015/course/TDA383/lab1.html) to use monitors instead of semaphores.

### [](#requirements)Requirements

*   **All the requirements from the first assignment are also demanded here except** that now we use monitors instead of semaphores.

*   You should use monitors that have **explicit locks and condition variables**. That is, you must use the `java.util.concurrent.locks` package from Java 5, _not_ `synchronized` methods, to implement your monitor.

*   You should not use the `tryLock()` method of the `Lock` interface. If you need such functionality, you have to implement it yourself in your monitor.

*   Waiting in your monitors should be realised using condition variables. Locks should be held locked only for short moments.

* * *

## [](#tips-and-tricks)Tips and Tricks

To give you some hints, you should think that each track is represented by a monitor. The monitor will have, _at least_ , the following interface:

<pre class="prettyprint lang-java prettyprinted"><span class="kwd">public</span> <span class="pln"></span> <span class="kwd">void</span> <span class="pln">enter</span><span class="pun">()</span> <span class="pln"></span> <span class="kwd">public</span> <span class="pln"></span> <span class="kwd">void</span> <span class="pln">leave</span><span class="pun">()</span> <span class="pln"></span> </pre>

where `enter()` is called by a train when entering a single track and `leave()` when a train leaves a single or double track.

## [](#submission)Submission

In your submission, you should clearly explain the following items:

*   What is the purpose of each method in the monitor?

*   How many conditional variables, if any, are needed in your solution? Justify your answer.

* * *

<footer>Concurrent Programming 2015 - Chalmers University of Technology & Gothenburg University</footer>

</div>
