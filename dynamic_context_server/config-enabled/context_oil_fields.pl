
% ?- Format = 'select * where {?U skos:broader category:Oil_fields_by_country ; rdfs:label ?T }',
%    sparql_client:sparql_query(Format, row(I1,I2), [host('dbpedia.org'), path('/sparql/')] ).

%    Format = 'select * where {dbpedia:Orinoco_Belt dcterms:subject ?A . }',
%    sparql_client:sparql_query(Format, row(I1), [host('dbpedia.org'), path('/sparql/')] ).

%    Format = 'select * where {dbpedia:Orinoco_Belt dbpprop:discovery ?A . }'

%    Format = 'select * where {dbpedia:Orinoco_Belt dbpprop:estOilBbl ?A . }'
%
