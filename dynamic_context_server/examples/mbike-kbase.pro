:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('mbike-specs.pro'),
  consult('mbike-rules.pro').
