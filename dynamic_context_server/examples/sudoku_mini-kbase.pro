:-initialization(startup).

startup :-
  consult('engine.pro'),
  consult('sudoku_mini-specs.pro'),
  consult('sudoku_mini-rules.pro').
