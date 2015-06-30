:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('ml-specs.pro'),
  consult('ml-rules.pro').
