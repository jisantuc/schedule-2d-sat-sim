`schedule-sim`
=====

[![codecov](https://codecov.io/gh/jisantuc/schedule-2d-sat-sim/graph/badge.svg?token=RGEx8tVyCY)](https://codecov.io/gh/jisantuc/schedule-2d-sat-sim)

This project holds simulation code for scheduling imagery collection on imaginary 2d satellites with targets located
some number of radians rotation along the unit circle.

The simulation should cover the stylized facts of what's complicated about the scheduling problem I work on on a daily
basis at Umbra, specifically:

* We have several satellites, and not every task can be completed by every satellite. Some have one satellite, some
  can be on any satellite, some can be on specific satellites).
* The cost of each task (how long it takes) isn't fixed; it depends on the angle between the satellite and the task.
* We don't know the whole universe of tasks we have to schedule ahead of time. We evaluate the schedule, _commit_,
  and then add more tasks if someone asks for them.

These three facts take the problem out of textbook optimizing job shop problems and into something more complicated. Or
at least I think they do, but I am not a computer scientist, so who knows, maybe someone who's ever seen day 25 of
Advent of Code could tell me otherwise.

For now, I'm ignoring other real life operational concerns, e.g.:

* the simulated satellite has infinite storage and it never has to send the data to the ground;
* the sun doesn't exist, and the simulated satellite has infinite battery;
* we can talk to the satellite at any time if we want to tell it about a new schedule; and
* we have perfect knowledge about the future of where the satellite will be, since it's a point in the middle of a
  circle instead of zooming through space at a high speed dealing with atmospheric drag and potential collisions with
  other satellites.

The 2d satellite initially points at the unit vector (1, 0). Tasks arrive at random times with random valid intervals
sometime in the future.
