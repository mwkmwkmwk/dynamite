Graphviz:

- visualize exits
- apply toposort to joins
- select nice headport for blue edges
- visualize loops as clusters

Ideas for later:

- improve joins structuralization:

  - try to ensure the final join actually has an edge to ``after``
  - try to ensure sequential blocks actually have edges between them
  - try to sort switch cases by value
  - for switches, try to maximize number of edges to the break block,
    or skip it entirely if ``after`` has lots of in-edges
  - make the join order deterministic
  - print unnatural loops specially somehow (at least note that they exist...)
  - emit more complex structures when useful? ::

      A:
      a();
      if (0) {
        B:
        b();
      }
      c();

- for joinless ifs, move the heavier branch out::

    if (x) {
      a();
      b();
      c();
      return;
    } else {
      d();
      return;
    }

  into::

    if (!x) {
      d();
      return;
    }
    a();
    b();
    c();
    return;

- loop improvements:

  - detect ``do while`` / ``for`` through single loop head jump
  - try detecting glued loop heads?
  - allow manually specifying loop breaks
  - improve loop break selection heuristics:

    - make it deterministic
    - like above, try to maximize break weight, all else being equal
    - if settling for a break at an exit, make sure it's actually used more
      than once
    - perhaps lower the priority of break-at-exit drastically, or remove
      it altogether?  Just about the only useful case is connecting this
      loop's break to parent's break/continue...
    - consider long chains of blocks ending with a noreturn call as well
      (need to partially reverse half-structuralization for these?)

- if a return block contains no interesting instructions and has incoming
  gotos, just change them to a "return"
- convert if trees to switches [requires a better input format]
- maybe convert switches to ifs/subswitches when common exits are involved, eg.

  ::

    switch (x) {
      case 0:
        a();
        goto C;
      case 1:
        b();
      C:
        c();
        break;
      case 2:
        d();
        goto F;
      case 3:
        e();
      F:
        f();
        break;
    }

  into::

    if (x == 0 || x == 1) {
      if (x == 0) {
        a();
      } else {
        b();
      }
      c();
    } else if (x == 2 || x == 3) {
      if (x == 2) {
        d();
      } else if (x == 3) {
        e();
      }
      f();
    }

  or find another way to deal with common switch paths.
