:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('ttt-specs.pro'),
  consult('ttt-rules.pro').
