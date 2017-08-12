Dynamite structuralizer
=======================

Takes a list of basic blocks and their types, converts it to
a structural program.

Usage
-----

::

    $ cat test
    a C x b c
    b U d
    c U d
    d R

::

    $ ./dynamite.py test
    a
    if (x) {
      b
    } else {
      c
    }
    d
    return

The following options are supported:

- ``-c``: split critical edges
- ``-d``: print the computed dominator tree
- ``-D``: enable debug info from dominator tree construction phase
- ``-h``: print result of the half-structuralization phase
- ``-H``: enable debug info from the half-structuralization phase
- ``-F``: enable debug info from the final phase
- ``-g <FILE>``: write the computed dominator tree in graphviz format to
  a given file

Input file syntax
-----------------

A ``#`` character starts a comment until the end of the line.

Each non-empty line describes one basic block.  Basic blocks are identified
by a name, which must not contain spaces nor the ``->`` substring.  The first
line describes the entry block.  The following kinds of basic blocks exist:

- ``<name> U <exit>``: A block with a single exit (ending with an unconditional
  jump or a fallthru in assembly). ``<exit>`` is the next block to be executed.
- ``<name> E``: A block with no exit (ends with a processor halt instruction,
  an exit syscall, call to a noreturn function, etc).
- ``<name> R``: A block that ends with a return instruction.
- ``<name> C <cond> <exitp> <exitn>``: A block with a two-way conditional exit.
  ``<cond>`` is the condition checked (any string that does not contain spaces),
  ``<exitp>`` is the next basic block to be executed iff the condition is true,
  and ``<exitn>`` is the one to be executed otherwise.
- ``<name> S <expr> <value0> <exit0> [<value1> <exit1> ...] [<exit-default>]``:
  A block ending with a table-driven indirect jump instruction (ie. a switch).
  ``<expr>`` is the expression to be looked up in the table (any string without
  spaces), ``<valueX>`` are values to be matched (again, any string without spaces),
  and ``<exitX>`` are corresponding basic blocks.  ``<exit-default>``, if present,
  is the basic block to be executed if none of the values matched (if not present,
  we assume we have no idea what will happen for values not present in the table).

In addition, appending ``+`` to the block type for any of the above (eg.
``<name> U+ <exit>``) will mark this block as "heavy" -- its "body" will not be
folded into compound conditions, possibly forcing ``goto`` to be used instead.
Lightwise, appending ``-`` to the block type will mark this block as "light"
-- its body will be considered to be completely uninteresting and folded away.
