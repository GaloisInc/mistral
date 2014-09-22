%Mistral: A Language for Programming Internet-Scale Applications

Introduction
------------

Programming at Internet-scale is a new challenge.  While many approaches exist
to sharing data at Internet-scale and network speed, we lack methods and
infrastructure to share computation at levels of concurrency, scale, and speed
to take advantage of this global computing resource.

One challenge in programming at Internet scale is the development of languages
in which to do this programming. For example, concurrency and control of it is
one aspect where current languages fall short. A typical highly concurrent
language such as Erlang can handle at most a few thousand concurrent processes
in a computation, and requires substantial assumptions about reliable
interconnection of all hosts involved in such computation. In contrast,
languages for programming at Internet-scale should scale to handle millions of
processes, yet be tolerant of highly dynamic network environments where both
hosts and communication paths may come and go frequently during the lifetime of
an application.

In this white paper we describe such a language.  First, we lay out basic
principles we feel are important for programming.  Second, we define the
structure of the language. Third, we define the entity classes (and resulting
types) recognized by our language, and define the syntax for declaring and
reasoning about them. We accompany these definitions with examples of syntax in
our language.


Mistral Design Principles
-------------------------

Mistral aspires to support programs that are:

- **Natural to write** The language should be designed around concepts familiar
  to domain experts rather than programming experts.

- **Scalable to millions of participating hosts and network links.** We seek to
  enable programming that leverages large portions of Internet resources in the
  execution of highly concurrent programs. We seek to make it easy for
  programmers to leverage network resources both known and unknown to them.

- **Fast** Such programs must operate at "network speed", able to cooperate and
  act without bottlenecks such as single points of control or human-in-the-loop
  time limitations.

- **Certain** The language must provide ways to analyze and limit the potential
  for undesirable effects caused by programs that are either poorly written or
  exhibit behaviors beyond their programmer's expectations.

- **Long-lived and upgradeable** We seek to enable Internet-scale distributed
  applications that may run for long periods. We recognize that such programs
  may need to be modernized while they run.

- **Effective** The language should support abstractions that allow the user's
  intent to be clear, while allowing tools in the compilation chain to optimize
  performance.


Mistral Language Structure
--------------------------

Mistral achieves separation of concerns by aspect-oriented programming. Mistral
distinguishes three aspects: _topology_, _tasking_, and _scheduling_.  Topology
specifies computational resources available, tasking specifies what computation
should be done, and scheduling attempts to distribute work to be done over
resources available.  We believe that these three aspects allow the programmer
to separate distinct concerns in the domain, focusing on each in turn to more
easily develop programs at Internet scale.

### Modules

A program in Mistral is composed of one or more modules.

#### Module Declaration

A module is defined with a `module` declaration at the top of a file:

```
module Foo where

m = pure ()
```

All symbols in the file will be visible externally; there is currently no
mechanism for specifying an export list.

#### Module Imports

Symbols from another module can be imported to the current module by way of an
`import` declaration.  For example, if the module `Foo` above has already been
compiled, and the module `Test` is currently being defined, the symbol `m` can
be used in `Test` by simply importing the `Foo` module:

```
module Test where

import Foo

test = m
```

When linked, the resulting program will contain declarations for both the `m`
and `test` symbols.  Note that as with exports, there is currently no way to
filter the list of symbols imported by an `import` declaration.

### Tags

Tags are a feature of Mistral by which a program author can give a semantic name
to a value that becomes globally available.  The introduction of tags is
currently only allowed on `node` and `task` declarations, but the environments
that they manipulate are referenced in many different places.

The general syntax for tags is as follows:

------ ---------------------
*tag*  ::= **`#`**identifier
*tags* ::= *tag* { *tag* * }
------ ---------------------

### Topology

A Mistral application runs on a virtual topology laid over the physical network.
A topology value specifies nodes and the links that connect them. A topology
value is introduced this way:

```
mainTopo : Topology
mainTopo = topology
  <node and link declarations>
```

#### Nodes

Nodes represent instances of the Mistral run-time. Nodes are declared in the
virtual topology of a Mistral application. Each node in the virtual topology
maps to a host on which its run-time instance is deployed. Node declarations
express this mapping. Host specification in a node declaration may be
accomplished by specifying a host's network address, or by specifying a set of
geographic or functional characteristics that the host must possess, and
allowing the compiler suite or the run-time to bind nodes to appropriate hosts.
Nodes in the overlay network may be tagged to allow easy reference to individual
nodes or collections of them from any scope within a program. At least one node
must be declared in each topology.

The example below shows syntax for declaring a single node, with name `console`,
and with an associated tag `#console`. Note that node names only exist in the
context of a topology declaration, while the task can be referenced through the
tag `#console` externally.  The `()` after the `node` keyword is a syntactic
placeholder for future expansion, where rich node characteristics will be
specified.

```
console = node () @ #console
```

The general syntax for the `node` declaration is as follows:

------------- ------------------------------------------------------------
*node*        ::= [ *name* **=** ] **`node`** *descriptors* { **`@`** *tags* }
*descriptors* ::= **`()`**
------------- ------------------------------------------------------------

#### Links

Links are bidirectional paths for communication between nodes in the virtual
overlay network of an application. Each link in a topology connects two nodes.
In the underlying network, a link may be composed of several path segments that
connect the hosts underlying the connected nodes.

*link* ::= *name* **`<->`** *name* { **`@`** *tags* }

```
node1 <-> node2 @ #myLink
```

### Tasking

A task is a process that runs on a node. Mistral specifies tasks using a finite
state machine model.  Each state specifies a set of legal transitions to new
states, the triggers that cause those transitions, and the set of actions to
take immediately after a trigger is recognized and before the new state is
entered.  Available actions include sending a message, or performing a
computation.  Actions are expressed as a sequence of statements ending in either
a `goto` transition, or entering the `done` state.  For example, when a
transition is triggered by receipt of a message or a timeout, the sequence of
statements associated with the transition will be performed, and the state
machine will move to the indicated new state. A task (and its associated threads
of execution) ends when it moves to the distinguished state "done".


#### Task Sets

A task declaration minimally includes a task name, and a starting state.  One
can also include: node constraints to be used when scheduling the task, or tags
to use when referencing the task later.  Tasks are grouped into task sets by the
`taskSet` syntax, and typically form a coherent program function (i.e.  task set
declarations will usually provide self-contained functionality).

```
mainTasks : TaskSet
mainTasks = taskSet
  <task declarations>
```

---------- -----------------------------
*task-set* ::= **`taskSet`** { *task* }
*task*     ::= [ *name* **`=`** ] **`task`** *state* { **`on`** *constraints* } { **`@`** *tags* }
---------- -----------------------------

The first example below declares a task named `consoleTask`, with initial state
`consoleStart`, that will run on the node tagged `#console` and have an
associated tag `#consoleTask`. Here, `console` is the identifier of a node,
`consoleStart` is the name of a state in a state machine declaration, and
`#consoleTask` is a tag to be associated with the task.The second example omits
the node assignment, a condition we refer to as "underspecified". In
underspecified tasking, the compiler will attempt to make choice about which
node a task will run on.

```
tasks : TaskSet
tasks = taskSet
  console = task consoleStart on #console @ #consoleTask
  work    = task workLoop                 @ #workTask
```

Tasks run until they enter the distinguished state "done", or until the node
they run on is de-allocated, at which time they cease as a thread of execution,
and their associated tags are no longer valid.


#### Task specifications

Tasks in a task set start when the application begins execution.  Dynamically
created tasks are started after the call to `addSchedule` finishes. Scheduling
ordering constraints between tasks must at present be implemented by explicit
coordination. In the future, we plan to allow constrained concurrency by
inter-task dependencies specified in the task set declaration. This will
simplify the programmer's task, while allowing the compilers to choose how to
best resolve dependencies.

As `taskSet` declarations are just values, they can be parameterized over other
values in the language, including: values to pass to the initial state of a
task, computation to be performed during a transition, a value to send to
another task.


## State Transitions

------------------ --------------
*transition*       ::= **`transitions`** *stateTransitions*
*stateTransitions* ::= *stateTransition* { *stateTransition* }
*stateTransition*  ::= *trigger* **`->`** *expression*
*trigger*          ::= *source* **`?`** *pattern*
                   | **`?`** *pattern*
                   | **`match`** *source* **`?`** *pattern*
                   | **`timeout`** *time*
                   | **`at`** *time*
------------------ --------------

The sample code below declares one state value: `loop`. Upon entry to this
state, the task thread of execution waits for one of two triggers: the receipt
of a message in the form of the constructor Go whose only argument is a list;
or the receipt of the single atom `#stop`. If the former trigger is received,
the specified actions block is executed, and the state machine transitions to
the same state `loop` using the `goto` primitive.  If the latter is received, a
single action -- creation of a logging message -- is performed, and the state
machine transitions to the state *done*, where the task ceases to exist.

```
loop : State
loop = transitions

  ? Go list ->
    { send #data (Message "got list")
    ; map (\x -> send #data (Append x)) list
    ; goto loop
    }

  ? #stop ->
    { log "stopped"
    ; done
    }
```

### Sending Messages

Tasks may send messages to other tasks.  The statement

```
send #task (#go, list)
```

sends a 2-tuple with elements `#go` (an atom) and `list` (a list of items). The
message is sent to the task tagged `#task`.

### Receiving Messages

Messages may be received in a `transitions` block, triggering either a sequence
to occur before moving to a new state, or the message to be discarded, returning
to the original state.

```
transitions
  ? (#go,list) -> actions { ... ; goto nextState }
```

This expression shows a state transition triggered by receipt of the above
message, where the action taken is empty, and the next state is `nextState`.

### Expressions


```
c = plus a b
```

comparisons(equal, gtr):

```
c = eq a b
```

conditionals:

```
if eq a b then {} else {}
```

and function application:

```
(\x -> plus x 1) 10
let f x = plus x 1 in f 10
```

### Inter-task communication actions

Mistral uses message passing to communicate between tasks. The message passing
primitives are `send` and `?` (receive). Default message passing is
synchronous. That is, a message is not sent by a sender until it is known that
the receiver is ready to receive it.


### Scheduling

Task sets are run on a topology by specifying schedules. A schedule maps one or
more task sets onto one or more topologies. Schedules allow programmers to limit
where tasks in a task set run to specific portions of the active overlay
network.

-----------          -----------
*schedule*           ::= **`schedule`** { *scheduleStatements* }
*scheduleStatements* ::= *scheduleStatement* { *scheduleStatement* }
*scheduleStatement*  ::= *name* **`using`** *topology*
-----------          -----------

```
main : Schedule
main = schedule
  mainTasks using mainTopo
```

#### Dynamic Scheduling Changes

Mistral supports dynamically adding topologies to the overlay network at
run-time. The following example declares a new topology and adds it to the
overlay network at runtime when the `test` action is called.

```
newTopo : Topology
newTopo = topology
  foo = node foo @ #foo
  bar = node bar @ #bar
  foo      <-> bar @ #fooBarLink
  #console <-> foo @ #fooConsoleLink

emptyTasks = taskSet

test : Action ()
test = addSchedule (schedule emptyTasks using newTopo)
```

Note that new topologies typically must be linked to existing topologies. To do
this, simply include in the topology declaration one or more links where one
endpoint tag is the tag of a node in the pre-existing overlay network to which
linking should be done. In the above example, the link from node `foo` to node
`#console` shows such a connection between the new and pre-existing topologies.
