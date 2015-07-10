:-initialization(startup).

startup :-
  consult('engine.pro'),
  http_query(c53060, 5600, 'carms_extern.command?FILE_LOAD=JEEP.MM', _),
  http_query(c53060, 5600, 'carms_extern.command?RUN_TIME=10.0', _),
  consult('jeep-specs.pro'),
  consult('jeep-rules.pro').
