# Mistral Tutorial

This document is a tutorial for the Mistral language. The language
[specification][spec] is also available.

In one sentence: Mistral is an aspect-oriented, strongly typed, functional,
immutable, recursive, high level language designed specifically for the domain
of massively concurrent, distributed, computation in environments that may lack
robust networks and hosts. 

In two sentences: Mistral aspires to allow programmers to program and coordinate
the actions of millions of hosts in a distributed network, using natural
expressions for reasoning about organization, scheduling, bounds on behavior,
and task assignment to hosts. At the same time, Mistral makes it easy to assure
properties of programs, and to catch errors in programming as early as
possible.

Many concurrent languages already exist. Some of these languages might best be
characterized as supporting a SIMD (Single Instruction, Multiple Data)
computational model. Map-reduce approaches fall into this category. Others fall
into the MIMD (Multiple Instruction, Multiple Data) computational model. These
languages can be further divided into those optimized for local computation on a
single host, such as C, C++, Java, and others that use typical thread packages;
and those optimized for distributed computation on many hosts, such as Erlang
and Scala. Mistral seeks to extend the MIMD, disributed computation model to
support computation in environments where networks may be unreliable, hosts may
fail unexpectedly, users may wish to distributed computation to specific
networks, geographies, or host resource profiles, communication may need to be
explicitly routed and encoded, and run-time platforms may have a broad range of
limitations.

# 'Hello World' in Mistral

Hello World is, admittedly, the starting point for examining almost every
computer programming language.  It gives us an opportunity to look at the core
parts of Mistral without getting bogged down learning new primitives and control
constructs that are necessary to implement complex operations.

All Mistral code lives in one module or another, so the first line is always a
module declaration:

```
module HelloWorld where
```

In typical programming languages you would now define the __actions__ to be
executed in the 'hello world' program:

```
helloWorldFunction : Actions ()
helloWorldFunction = log "Hello World"
```

However, this isn't a complete Mistral program.  In Mistral there are five
aspects for programmers to think about. While these aspects make very simple
programs verbose, they make complex programs much less verbose and easier for
programmers to reason about. The remaining three aspects are:

* A __topology__ that describes the __nodes__ engaged in computational
  __tasks__, and their interconnection __links__. A node is a virtual or
  physical host running the Mistral run-time. A link is a communication
  channel between two nodes, used for sending information between tasks that
  run on those nodes.

* A __task set__ that enumerates the entry points for each computational task,
  grouping of tasks into meaningful sets, and constraints on executing tasks,
  such as inter-task scheduling dependencies, and resources required for tasks
  to run.

* A __schedule__ that assigns task sets to portions of the defined topology.

<!-- * __Actions:__ define the functions to be executed. -->

To continue our Hello World example, we define a topology consisting of one
node and no links (because we don't need to communicate among tasks for this
job), a task set with one task, and a schedule that connects this single task
set to our topology.

First, let's define the topology, containing only one node, optionally named
`x`:

```
helloTopology : Topology
helloTopology = topology {
    x = node ()
}
```

Next, we define our task set. Using the keyword `task`, we construct a single
task called helloTask and specify that the task's entry point is
`helloWorldFunction`.

```
helloTasks : TaskSet
helloTasks = taskSet {
    task helloWorldFunction
}
```

Next is our schedule. The schedule uses the reserved word `main` to indicate it
is the top-level entry point for the program. For any Mistral application there
must be one `main` schedule that maps a set of tasks to a portion of our
topology.  Schedules can incorporate other schedules by reference.  The same is
true for topologies and task sets.  Thus, these values can be built by
composing smaller parts from libraries or other modules.

```
main : Schedule
main = schedule {
    helloTasks using helloTopology
}
```

Task allocation to nodes may be done at compile time if there are no
runtime-dependent constraints. Otherwise, task allocation to nodes is done by
the runtime. In our HelloWorld example, task allocation is performed at compile
time because we place no constraints on the schedule tasks. Running the program
results in starting a single node, execution of the helloWorldFunction code,
then shutdown.

# Familiar Concepts

This section covers concepts that are not unique to Mistral, including types,
control structures, and literals.  Concepts that are unique to Mistral along
with a more through handling and example set can be found in the next section,
_New in Mistral_.

## Comments

Anything after `-- ` (hypen, hypen, space) is a comment.  Multi-line comments
are not supported.

## Basic Syntax

Variables have names. These names are alphanumeric strings (including the prime
and underscore characters). Each variable name starts with a lower case letter
or underscore.  Type names use the same character set, but start with upper
case letters.

Language layout can either be indentation based, much like Python, or use
brackets and semi-colon seperators as in shell scripts.  For example, a series
of actions can be written as

```
op = actions
    act1
    act2
    act3
```

or

```
op' = actions {
    act1 ;
    act2 ;
    act3
}
```

Note that semi-colon is a seperator, not a terminator like in C or Java, so the
final action in a block lacks a semi-colon.  However, as this is easy to forget,
the parser will allow a trailing semi-colon in layout blocks:

```
op'' = actions {
    act1 ;
    act2 ;
    act3 ;
}
```

### Leaving out `actions`

As the `actions` block shows up frequently in Mistral code, the `actions`
keyword can be omitted when using explicit layout

```
op''' = {
    act1 ;
    act2 ;
    act3
}
```

## Types

Any declaration can be given an explicit type, which is good form on top level
declarations.  Explicit type signatures use a colon, for example:

```
value : Int
```

Functions are first class values, which means functions can be passed as
parameters to other functions.  The types of functions contain the types of
their arguments and results, delimited by arrows, as found in Haskell, Ocaml,
and other typed functional languages.  Ill-typed expressions will be detected at
compile time.

```
squareRoot : Int -> Int
```

Current types include Topology, Taskset, Schedule, Atom, Actions, tuples, and
lists.

### Lists

Lists of values can be constructed for any given type.  Square brackets start
and end lists while commas seperate the elements.  For example, a list of atoms
with explicit type signature is syntactically written:

```
exAtomList : [Atom]
exAtomList = [#Atom1, #Atom2, #Atom3]
```

Lists can be of variable length, with a zero-element list represented by `[]`.
All elements of a list must have the same type.

### Tuples

Tuples, such as pairs `(first,second)` or triples `(f,s,t)` and so on, are a
built-in type in Mistral.  A pair of a string and a number with explicit type
signature would be:

```
exTuple : (String,Int)
exTuple = ("some string", 42)
```

A function accepting such a pair and duplicating the number could be written:

```
dupSecond : (String, Int) -> (String, Int, Int)
dupSecond (s,n) = (s,n,n)
```

### Polymorphism (Function Overloading by Type)

Mistral support fully polymorphic functions by way of type variables.  Functions
can be overloaded in the types of arguments, but not in the number of
arguments. For example, the above `dupSecond` function could be written to work
on any pair instead of strictly on `(String,Number)`:

```
dupSecond' : (a, b) -> (a, b, b)
dupSecond' (s,n) = (s,n,n)
```

Notice type variables are represented as a lower-case string while concrete
types (`Int`, `TaskSet`, etc) all start with upper-case letters.

### User-defined Types

Mistral supports user-defined data types.  As a simple example, here is the type
of an optional value:

```
data OptionalInt = Value Int | NoInt
```

Values of type `OptionalInt` can be created using the two constructor functions
introduced by the right-hand-side of the data declaration: `Value` and `NoInt`.

```
Value : Int -> OptionalInt
NoInt : OptionalInt
```

The `OptionalInt` type can be generalized, by parameterizing it on what is
optional:

```
data Optional opt = Value opt | Nothing
```

Now, when a number is used with the `Value` constructor, it will fix the type
variable `opt` in `Optional opt` type:

```
Value 10 : Optional Int
```

Parameterizing datatypes this way can allow for better reuse of common patterns.


### Atoms

Inspired by Erlang's atom type, Mistral supports values of type `Atom` that can
be serialized and deserialized.  Syntactically, atoms are any alpha-numeric
string preceded by a hash, for example `#ThisString`.

More significantly, atoms are used to _Tag_ certain values, allowing the
programmer to refer to named sets of values.  Conceptually, a tag is the
social media equivalent of a hashtag for your programming language.  Do you
know there exists some processes hashtagged `#Database` and want to reach one?
Great, just use the `#Database` atom when sending to the database. Tags can be
thought of as a global name space; we present the idea more completely in the
next section.

### Literals

There are four types of literals in Mistral.  Numeric literals, string literals,
colon literals, and dot literals.  Numeric literals can be of any numeric type,
which Mistral will define in the future.  String literals represent packed
string, such as `"Hello World"`. Colon literals, written `a:bcd:e:f:ghi`, are
generally used for IPv6 addresses and times (`1:2:3` means one hour two minutes
three seconds while `1:2:3:4` and `1:2` are invalid times). Dot literals are
common for domain names such as `www.example.com` (note this is not a quoted
string) and for IPv4 addresses such as `8.8.8.8`.

Strictly speaking, there are also colon-dot literals which are also used for
time. Colon-dot literals allow specification of times with sub-second precision
such as three hours and four hundred micro-seconds (4 ten-thousandths of a
second): `3:0:0.0004`.

### Control Structures

Control flow can be affected by pattern matches at the top level, if-then-else
statements, `transitions` blocks, and (from some perspectives) tail recursion.
The if-then-else statement takes a Boolean expression and will execute one of
two expressions, for example:

```
exIf = if val
        then expr1
        else expr2
```

Will execute `expr1` when `val` evaluate to `True` and `expr2` otherwise.

Pattern matching with a `case` expression allows for discrimination of values.
For example:

```
divideSafe : Int -> Int -> Some Int
divideSafe a b = case b of
                   0 -> None
                   _ _> Some (div n d)
```

Finally, blocks of `transitions` can be used to block execution until one of
many events occur, usually a time out or receipt of a network message.  We
delay details till the next section, but a simple example of receiving a
message or timing out after one second is:

```
recvOrFail : (Int -> State) -> State -> State
recvOrFail cont failOper =
    transitions
        ? msg         -> goto (cont msg)
        timeout 0:0:1 -> goto failOper
```

We mention tail recursion here because it is a critical method to
provide looping and similar constructs.  Mistral does not offer any
traditional imperative style loops, so use of tail recursion is
critical (and tail recursion is its own reward anyway).  For more
details of tail recursion, please read the section on functions,
procedures, and subroutines above.


# New in Mistral

### Topology

Topologies specify sets of `node`s, `link`s, and their relationships. A node is
a virtual or physical network-connected host that runs the Mistral run-time.  A
link is a network connection between two nodes. Topologies are constructed using
the `topology` keyword. Topologies are composable using Comprehensions,
described below.

#### Nodes, and Links

Nodes, and links may be declared statically or dynamically, and only within
topologies.  For example, the following topology declares two nodes with a
single connecting link. If no path of links connects two nodes, then tasks
running on them cannot communicate.

```
nodeExample : Topology
nodeExample = topology {
    node1 = node () ;
    node1 <-> node2 -- a communication link between node1 and node2
}
```

#### Comprehensions in Topologies

Suppose we want to describe a topology that describes a star network with one
central node, `switch`, which is connected to each of a list of nodes,
`computers`, at the points of the star. We use parameterization and list
comprehensions to describe the star concisely this way:

```
star : Node -> [Node] -> Topology
star switch computers = topology
    [switch <-> computer | computer <- computers]
```

Now we can show how to compose topologies to create a "double star" with the
switches connected to a single router.  To use a sub-topology just call it with
function-application syntax and preceded by the keyword `using`:

```
starOfStars : Node -> [(Node,[Node])] -> Topology
starOfStars router stars = topology
    [using (star switch computers) | (switch, computers) <- stars]
```

### Tasks and TaskSets

A task is the specification for an entry point. Each task runs on a single node
in a topology. Communication in Mistral occurs between tasks.

Constructed with the `taskSet` keyword, a task set is a collection of tasks
along with optional constraints on their placement.

Within a `TaskSet` each use of the `task` keyword results in a new
`TaskHandle`. Task handles allow other tasks within the Mistral program to send
messages to the named task.  Task handles can be sent in messages to other
tasks as an introduction.

Initially, task handles come from a TaskSet or by messages from other tasks.
As an example of the former, consider:

```
namedTaskExample : TaskSet
namedTaskExample = taskSet {
    peer1  = task (torrent peer2);
    peer2  = task (torrent peer1)
}

-- 'torrent' represents a distributed peer-to-peer task
torrent : TaskHandle -> Actions ()
torrent handle = ...
```

Here, the task handles are referenced by naming them in to the expressions
specified in the task set. Notice the order of declaration does not matter within a task set.

As with toplogies, it may be useful to define large numbers of similar tasks. If a
large number of cooperating tasks are desired we can use a comprehension:

```
myDistributedDB : Int -> TaskSet
myDistributedDB nrDB = taskSet {
    [task distributedDBFunction | _ <- [1..nrDB]]
}
```

We can use `Tag`s when specifying the execution node for a task destinations:

```
myTwoDBTasks' : TaskSet
myTwoDBTasks' = taskSet {
    db1 = task (dbOper db2)
        on #Node1 ;
    db2 = task (dbOper db1)
        on #Node2
}
```

### Schedules

Constructed with the `schedule` keyword, a `Schedule` is a collection of task
sets and topologies on which the tasks should be deployed and run.  The only way to use a
schedule is within another schedule or by declaring the schedule to be 'main'
and thus the entry point to the Mistral program.

### States

States specify the progression of tasks. A task begins in the state specified in
its `task` declaration. Each state specifies a set of transitions. Each
transition specifies a trigger that causes the transition to fire, a set of
actions to execute before the transition completes, and a next state to
transition into when those actions are complete.  Supported triggers include
receipt of a specific message, evaluation of a trigger expression to "TRUE", or
passage of time. Triggers in a transition are prioritized in order, from top to
bottom. A default trigger may be specified in terms of a "timeout", so that if
no other trigger conditions occur within the timeout period, the timeout
transition fires.

#### Moving Between States

The actions of a transition must always terminate in a state change.  This can
be achieved in one of two ways: `goto` or `done`.

`goto` specifies a new state to transition into, waiting for new messages to
move the state machine along.

`done` signals that the task has finished, and that no more messages are
expected.

```
state1 : State
state1 = transitions
           ? msg -> goto (state2 msg)

state2     : String -> State
state2 msg = transitions
               ? #done -> { log msg; done }
```

## Communication

Communication between tasks occur using the `send` primitive to send data and
the receive operation (syntactically using `transitions` and `?`).

The send operation performs a blocking I/O operation which does not return
until the message is delivered to a corresponding receive transition by its
partner.  The send operation performs all necessary marshaling and
serialization to transfer the data.  Currently the send operation has no
definition for action in the case of failure. This will be addressed in an
future revision of the language.

For example, two processes computing Fibonacci can be implemented as such:

```
-- one process uses 'fibStart' as its entry point
fibStart : TaskHandle -> Actions ()
fibStart partner = actions {
    send partner (1,1) ;
    log (string 1) ;
    goto (fibOperation partner)
  }

-- The entry point for the second process is 'fibOperation'
fibOperation : TaskHandle -> Actions ()
fibOperation partner =
    transitions
        ? (old,new) -> actions {
            log (string new) ;
            send partner (new, add old new) ;
            goto (fibOperation partner)
          }
```

In the above, one task starts the shared computation by sending the first two
fibinacci numbers, `1, 1`.  The other process, running `fibOperation`, responds
with `1, 2` (The `1` being a state and the `2` being a new fibinacci value).  The
computation loops and these tasks will carry on forever.

This requires that each task will only be receiving one input - a number from
its peer.

### Transitions

A transitions block is syntactically similar to `switch` or `case` blocks of
other languages, but there are important semantic differences including pattern
matching, and a semantics of blocking on effectful actions.

As an example, consider this code that receives data from any source and then
forwards both the sender information and the message on to another entity. The
code loops infinitely due to an unconditional tail call.

```
forwardTo handle =
    transitions
        src ? msg -> actions
               send handle (src, msg)
               forwardTo handle
```

In this code there is a single transition that is blocked until data is
received from any source (recall the `?` means "receive").  Prior to the
transition block the variable `src` was free, so upon receiving a message `src`
is bound to the sender's TaskHandle. Similarly, the previously-free variable
`msg` is bound to the received message.

Omitting anything left of `?` is legal and results in receiving a message
without getting the sender's task handle. 

Alternatively, imagine we want to either forward data or route it to a
destination specified by the source.  For that we will use some pattern
matching:

```
forwardOrRoute : TaskHandle -> Actions ()
forwardOrRoute dflt =
    transitions
        src ? (#routeMe, dstHandle, msg) -> actions
                send dstHandle (src,msg)
                forwardOrRoute dflt
        src ? msg -> actions
               send dflt (src, msg)
               forwardOrRoute dflt
```

Now there are two transitions. The first will attempt to decode a received
message for an atom, `#routeMe`, and a value of type `TaskHandle` named
`dstHandle`.  Upon reception, the message and source information is forwarded
on to the requested destination.  All other messages are forwarded to a default
task.

Note that order is important! If the two transitions were swapped then the
`#routeMe` case would never be used because `msg` is a free variable and can
match any pattern.  Instead of routing data we would be forwarding all
messages, even those with the `#routeMe` atom, to our default task (`dflt`).
It is the programmers responsibility to order transitions from most specific to
most general matches.


## Tags: Using Atoms as a Global Namespace

Most values declared in a topology, task set, or schedule can be "tagged" with
one or more atom.  Once tagged, each value can be refered to using any of those
atoms.  Tags are set using the `@` symbol (recall that the `#` symbol is used
for atoms).

Consider the three tasks and tagging:

```
taggedTasks : TaskSet
taggedTasks = taskSet {
    taskA = task functionA @ (#OperTask, #TaskA) ;
    task (functionB taskA) @ (#OperTask, #TaskB) ;
    task functionC
}
```

Two tasks bear two tags (the syntax of tuples is re-used to express unordered
sets of tags).  One of those two tags is shared - referring to that tag will
refer to _an arbitrary one of_ the two tasks.

To send to task `A`, a process either must have been passed the `taskA` value
(of type `TaskHandle`), or use the tag:

```
functionB : TaskHandle -> Actions ()
functionB taskA = send taskA "This is a message for task A"

functionA : Actions ()
functionA = send #TaskB "This is a message for task B"
```

Finally, task `C` can send to either task `A` or task `B` (not both) using the tag:

```
functionC : Actions ()
functionC = send #OperTask "A message for A or B"
```

The above can be read "send the message to one of the values bearing tag
`#OperTask`".  It is also possible to specify a set of tags, `(#Tag1,#Tag2)`,
to indicate one of the values bearing all the specified tags.

There is currently no syntax for referring to all values bearing a tag or
values bearing some complex combination of tags.

  [spec]: Mistral.md "The Mistral Language Specification"
