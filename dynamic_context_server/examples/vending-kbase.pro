:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('vending-specs.pro'),
  consult('vending-rules.pro').
