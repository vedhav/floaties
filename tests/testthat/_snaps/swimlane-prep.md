# events can use a different subject column

    Code
      prep_swimlane(sw_lanes(), pid, stop, events = ev, event_time = at, event = what)
    Condition
      Error in `prep_swimlane()`:
      ! Can't select columns that don't exist.
      x Column `pid` doesn't exist.

# events for unknown subjects or missing times are dropped

    Code
      spec <- prep_swimlane(sw_lanes(), pid, stop, events = ev, event_time = at,
      event = what)
    Condition
      Warning:
      Dropped 1 event with a missing at.
      i Affected subjects: "p1".
      Warning:
      Dropped 1 event for subjects not in `data`.
      i Unknown subjects: "nobody".

# prep_swimlane() validates its input

    Code
      prep_swimlane(d, pid)
    Condition
      Error in `prep_swimlane()`:
      ! `end` is required.
    Code
      prep_swimlane(d, pid, cohort)
    Condition
      Error in `prep_swimlane()`:
      ! `end`: Must be of type 'numeric', not 'character'
    Code
      prep_swimlane(d, pid, stop, start = cohort)
    Condition
      Error in `prep_swimlane()`:
      ! `start`: Must be of type 'numeric', not 'character'
    Code
      prep_swimlane(d, pid, stop, ongoing = begin)
    Condition
      Error in `prep_swimlane()`:
      ! `ongoing`: Must inherit from class 'logical'/'character'/'factor', but has class 'numeric'
    Code
      prep_swimlane(d, pid, begin, start = stop)
    Condition
      Error in `prep_swimlane()`:
      ! `end` must not be before `start`.
      x Lanes end before they start for "p3", "p1", "p4", and "p2".
    Code
      prep_swimlane(d, pid, stop, events = ev, event = what)
    Condition
      Error in `prep_swimlane()`:
      ! `event_time` is required.
    Code
      prep_swimlane(d, pid, stop, events = ev, event_time = what, event = what)
    Condition
      Error in `prep_swimlane()`:
      ! `event_time`: Must be of type 'numeric', not 'factor'
    Code
      prep_swimlane(d, pid, stop, events = "ev", event_time = at, event = what)
    Condition
      Error in `prep_swimlane()`:
      ! `events`: Must be of type 'data.frame' (or 'NULL'), not 'character'
    Code
      prep_swimlane(d, pid, stop, events = ev, event_time = at, event = what,
        event_shapes = c("circle", "hexagon"))
    Condition
      Error in `prep_swimlane()`:
      ! `event_shapes`: Must be a subset of {'circle','square','triangle','diamond','cross','star','arrow'}, but has additional elements {'hexagon'}
    Code
      prep_swimlane(d, pid, stop, events = ev, event_time = at, event = what,
        event_colors = c(respond = "red"))
    Condition
      Error in `prep_swimlane()`:
      ! `event_colors` must have a color for every level.
      x No color for "progress".

