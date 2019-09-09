# fpg
Spineless G machine lab

This project is about trying to implement C++ code for a spineless G machine by following the paper 
The Spineless G Machine
G L Burns
S L Peyton Jones
J D Robson

The spineless G machine precedes the spineless, tagless, G machine in the evolution of GHC. The present machine has 'tags'
in that the nodes that would have been on the spine are tagged as to what kind they are, while the spineless one uses the
address of the code that handles the node as the replacement of the tag.

As of today the code generated is incomplete because certain expressions are reaching the C Scheme of the compiler when 
there is no definition of how those expressions should be handled by the C Scheme.

The short-term goal is to have the G code interpreted.
Other goals:
Detag the nodes
Unbox native data
That thing with cons/nil/hd/tl is weird.

