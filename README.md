# statechum
Statechum project, originally on sourceforge

Statechum is the tool that can be used for the inference of models and their comparison as well as for test generation. 
The inference process can use both passive and active. 
For the passive one, Blue Fringe can be used; active learner uses QSM.

A relatively brief manual can be found in resources/introduction/index.html
# description
Statechum is a framework that implements a number of regular grammar inference algorithms. Regular grammars can be represented as finite state machines. Once the grammar / state machine has been generated, StateChum can visualise it, and provides a selection of state-machine analysis and testing algorithms.

The original purpose of the framework was to investigate the application of grammar inference to the problem of reverse-engineering state machines from execution traces. It also contains code to generate test sets using the W method and methods for random trace generation.

It is possible to introduce domain-specific constraints using LTL or by directly encoding them in automata. This makes inference significantly more effective because the learner does not need to make "guesses" in many cases.

Random FSM generator implemented in this tool was built to generate finite-state machines with structural properties similar to machines published in Software Engineering literature. It has been used to generate samples for the Stamina grammar inference competition.

Graph comparison effectively computes a "diff" between arbitrary directed graphs by using language measure to match states. Finite-state machines can also be compared using precision/recall or BCR scores. Graphs can be visualised using the R tool and dynamically updated in the course of analysis.

Other work involves integration with Erlang in order to perform automated inference of Erlang software. This is made possible by the Typer engine which generates types of Erlang functions hence making it possible to automate trace generation for the inference process.
