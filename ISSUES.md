### Known Issues

Some of the items described below are out of date. For up-to-date issues, see
the GitHub repository's Issue tracker.

#### Larger Issues
- The frontend does not support signatures, functors, record types, and a slew of
corner cases in the language.

- PVal and PTuples cannot be used together. The "fast clone" translation breaks
invariants relied on by the work-stealing scheduler with regards to the valid
intermediate states of the work queues.

- Exception handling is not implemented.

- The inatomic/from-atomic/to-atomic naming convention used in inline BOM is still
a bugfest and should really be replaced by a static annotation that is checked
by the compiler.

- The basis library is a hodgepodge mess. The few structures that exist are
typically dramatically different from the SML basis library due to the subset of
the language implemented, which both makes existing code from another system
hard to reuse and sometimes the interface cannot even be written.

#### Smaller Issues

- The effect analysis defined in bom-opt/remove-atomics.sml should be changed from
being name-based to instead either have a trackable annotation or other better
marker for user-level code that uses mutable state. Additionally, while we
remove ATOMIC operations around PURE functions, we do not handle reducing them
in the case where the code between the parallel spawn and another lock is PURE.

- We cannot handle allocations larger than a single heap page size (minus some
slop). These allocations result in an exception, which is tough to debug because
there is no exception handling.

- The work-stealing scheduler cannot handle more than a stack of 32k tasks, and
crashes quietly when that is exceeded.

- Memoization and mutable state exist only as hand-performed translations to call
basis library functions.

#### Incomplete projects

- The safe-for-space closure conversion was not completed. While its code may be
used for inspiration, we were not able to get a full write-up on its status
before the student graduated.

- In CFG, we now have code that performs rudimentary loop identification and can
also generate a DOT file for visualization of basic blocks. Loop unrolling was
not implemented.

- A branch was created for the BOM implementation of flattening, but it is still
in the design phase.
