# fpg
Spineless G machine lab

This project is about trying to implement C++ code for a spineless G machine by following the paper 

The Spineless G Machine
-    G L Burns
-    S L Peyton Jones
-    J D Robson

The spineless G machine precedes the spineless, tagless, G machine in the evolution of GHC. The present machine has 'tags'
in that the nodes that would have been on the spine are tagged as to what kind they are, while the spineless one uses the
address of the code that handles the node as the replacement of the tag.

As of today the code generated is incomplete because certain expressions are reaching the C Scheme of the compiler when 
there is no definition of how those expressions should be handled by the C Scheme.

The short-term goal is to have the G code interpreted.

Other goals:
- Detag the nodes
- Unbox native data
- That thing with cons/nil/hd/tl is weird.

The G0 project included here for now is a non-spineless G machine based on 

Implementing Functional Languages: a tutorial
- Simon L Peyton Jones
--Department of Computing Science, University of Glasgow
- David R Lester
--Department of Computer Science, University of Manchester

This tutorial has multiple interpreters for classic functional program architectures, including TIM, G machine, a template instantiation machine, and a parallel G machine.

The G code emitted by the compiler in G0 creates Application nodes
on the node stack and those get applied to each other by an unwind instruction until you have a number node or a code node. The
code node will then be executed. At the moment G0 doesn't quite work.

The g1 project included here for now is a classic G machine based closely on
 
Efficient Compilation of Lazy Evaluation
- Thomas Johnsson

This is one of the original G machine papers

The compiler and interpreter in G1 are working pretty well at the moment, except that let forms are not right yet.
