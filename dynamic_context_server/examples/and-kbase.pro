:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('and-specs.pro'),
  consult('and-rules.pro').
