:- module(context_soil, []).

/** <module> Soil classification
    * Uses ontological classification scheme
*/

:- context:register(context_soil:soil_table).

:- use_module(context).

/*
% :- multifile(rdf_/3).

rdf_local(ent:soil, ent:category, ent:coarse_grained_soil).
rdf_local(ent:soil, ent:category, ent:fine_grained_soil).
rdf_local(ent:soil, ent:category, ent:highly_organic_soil).

rdf_local(ent:coarse_grained_soil, ent:division, ent:gravel).
rdf_local(ent:coarse_grained_soil, ent:division, ent:sand).
rdf_local(ent:coarse_grained_soil, ent:description, 'coarse grained').
rdf_local(ent:coarse_grained_soil, ent:has, ent:gradation).
rdf_local(ent:coarse_grained_soil, ent:has, ent:contents).

rdf_local(ent:fine_grained_soil, ent:division, ent:moist_silt_clay).
rdf_local(ent:fine_grained_soil, ent:division, ent:dry_silt_clay).
rdf_local(ent:fine_grained_soil, ent:description, 'fine grained').
rdf_local(ent:fine_grained_soil, ent:has, ent:plasticity).

rdf_local(ent:highly_organic_soil, ent:description, 'highly organic').
rdf_local(ent:highly_organic_soil, ent:has, ent:contents).
rdf_local(ent:highly_organic_soil, ent:division, ent:organic).


rdf_local(ent:gravel, ent:symbol, 'G').
rdf_local(ent:gravel, ent:name, gravel).

rdf_local(ent:sand, ent:symbol, 'S').
rdf_local(ent:sand, ent:name, sand).

rdf_local(ent:moist_silt_clay, ent:class, ent:clay).
rdf_local(ent:moist_silt_clay, ent:class, ent:silt).

rdf_local(ent:dry_silt_clay, ent:class, ent:clay).
rdf_local(ent:dry_silt_clay, ent:class, ent:silt).

rdf_local(ent:silt, ent:class, ent:contents).
rdf_local(ent:silt, ent:symbol, 'M').
rdf_local(ent:silt, ent:name, silt).
rdf_local(ent:silt, ent:description, 'SILT-like character').
rdf_local(ent:silt, ent:has, ent:low_plasticity).

rdf_local(ent:clay, ent:class, ent:contents).
rdf_local(ent:clay, ent:symbol, 'C').
rdf_local(ent:clay, ent:name, clay).
rdf_local(ent:clay, ent:description, 'clay-like character').
rdf_local(ent:clay, ent:has, ent:high_plasticity).

rdf_local(ent:organic, ent:symbol, 'O').
rdf_local(ent:organic, ent:name, organic).

rdf_local(ent:low_gradation, ent:class, ent:gradation).
rdf_local(ent:low_gradation, ent:symbol, 'P').
rdf_local(ent:low_gradation, ent:description, 'poorly graded (uniform particle sizes)').

rdf_local(ent:high_gradation, ent:class, ent:gradation).
rdf_local(ent:high_gradation, ent:symbol, 'W').
rdf_local(ent:high_gradation, ent:description, 'well-graded (diversified particle sizes)').

rdf_local(ent:moist_silt_clay, ent:symbol, 'H').
rdf_local(ent:high_plasticity, ent:class, ent:plasticity).
rdf_local(ent:high_plasticity, ent:symbol, 'H').
rdf_local(ent:high_plasticity, ent:description, 'high plasticity').

rdf_local(ent:dry_silt_clay, ent:symbol, 'L').
rdf_local(ent:low_plasticity, ent:class, ent:plasticity).
rdf_local(ent:low_plasticity, ent:symbol, 'L').
rdf_local(ent:low_plasticity, ent:description, 'low plasticity').
*/

soil_combination([C, Desc, TG, [Desc, ' containing ', Graininess]]) :-
   C = 'coarse grained',
   % Prefix
   rdf(ent:soil, ent:category, Cat),
   rdfS(Cat, ent:description, C),
   rdf(Cat, ent:division, Div),

   rdfS(Div, ent:symbol, Texture),
   rdfS(Div, ent:name, Desc),

   % Suffix
   (
     rdf(Character, ent:class, ent:gradation);
     rdf(Character, ent:class, ent:contents)
   ),
   rdfS(Character, ent:symbol, Grain),
   rdfS(Character, ent:description, Graininess),

   % Code
   atom_concat(Texture, Grain, TG).

soil_combination([C, Desc, TG, [Desc, ' ', Plasticity, ' mainly ', Graininess]]) :-
   C = 'highly organic',
   % Prefix
   rdf(ent:soil, ent:category, Cat),
   rdfS(Cat, ent:description, C),
   rdf(Cat, ent:division, Div),

   rdfS(Div, ent:symbol, Texture),
   rdfS(Div, ent:name, Desc),

   % Suffix
   rdf(Contents, ent:class, ent:contents),
   rdfS(Contents, ent:description, Graininess),

   rdf(Character, ent:class, ent:plasticity),
   rdfS(Character, ent:symbol, Grain),
   rdfS(Character, ent:description, Plasticity),
   rdf(Contents, ent:has, Plast),
   rdfS(Plast, ent:description, Plasticity),

   % Code
   atom_concat(Texture, Grain, TG).


soil_combination([C, Desc, TG, [Desc, ' with ', Plasticity]]) :-
   C = 'fine grained',
   % Prefix
   rdf(ent:soil, ent:category, Cat),
   rdfS(Cat, ent:description, C),

   rdf(Cat, ent:division, Div0),
   rdfS(Div0, ent:symbol, Grain),
   rdf(Div0, ent:class, Div),

   rdfS(Div, ent:symbol, Texture),
   rdfS(Div, ent:name, Desc),

   % Suffix
   rdf(Character, ent:class, ent:plasticity),
   rdfS(Character, ent:symbol, Grain),
   rdfS(Character, ent:description, Plasticity),

   % Code
   atom_concat(Texture, Grain, TG).


soil_table(_Request) :-
    findall(Row,soil_combination(Row), Rows),
    reply_html_page([title('Unified Soil Classification Table'),
		     \(con_text:style)],
                   [
		    h1('Soil Classifiers'),
		    p('USCS two-character code'),
		    \(con_text:table_multiple_entries(
				   [[b(category), b(division), b(groupSymbol), b(groupName)]],
				    Rows
						     )
		     )
                   ]).

% This creates a decorated list

soil_range(Element, li([i(Item), ' : '  | Description])) :-
    atom_to_term(Element, E, [Item=E]),
    atom(Item),
    not(compound(E)),
    soil_combination([_C, _Div, Item, Description]).

soil_range(Element, li([b([i(Item1),' : ' | Description1]), &(rarr), br([]),
                        b([i(Item2),' : ' | Description2])]
                      ) ) :-
    atom_to_term(Element, A-B, [Item1=A,Item2=B]),
    soil_combination([_C1, _Div1, Item1, Description1]),
    soil_combination([_C2, _Div2, Item2, Description2]).



process_soil_types([], Input, Output) :- reverse(Input, Output).
process_soil_types([F|R], Input, Output) :-
    soil_range(F, Value),
    process_soil_types(R, [Value|Input], Output).


parse_request(Soils, List) :-
    atom_to_term(Soils, Items, []),
    process_soil_types(Items, [], List), !.
parse_request(_Soils, '').


% This creates an undecorated list

soil_collect(Element, Item) :-
    atom_to_term(Element, E, [Item=E]),
    atom(Item),
    not(compound(E)),
    soil_combination([_C, _Div, Item, _Description]).

soil_collect(Element, [Item1, Item2] ) :-
    atom_to_term(Element, A-B, [Item1=A,Item2=B]),
    soil_combination([_C1, _Div1, Item1, _Description1]),
    soil_combination([_C2, _Div2, Item2, _Description2]).



collect_soil_types([], Input, Output) :- reverse(Input, Output).
collect_soil_types([F|R], Input, Output) :-
    soil_collect(F, Value),
    collect_soil_types(R, [Value|Input], Output).


collect(Soils, List) :-
    atom_to_term(Soils, Items, []),
    collect_soil_types(Items, [], List), !.
collect(_Soils, '').


search(Name, List) :-
    rdfS(URI, ent:soil_classification, Types),
    rdfS(URI, ent:name, Name),
    collect(Types, L),
    flatten(L, LL),
    sort(LL, List)
    .

/*
store_local :-
    context:make_name('soil_classification', Ent, E),
    print(user_error, Ent),
    !,
    rdf_local(A,B,C),
    context:storeRDF_to_graph(A, B,C, E),
    fail.
*/

/*
USDA Soil Classification Description
Superstition-Rositas Sand
Carrizo Extremely gravelly loamy coarse sand
Riverbend Extremely cobbly sand loam
Cristobol-Gunsight Silty, clayey gravel with sand to sandy day to poorly-graded gravel
with silt
Gunsight-Chuckawalla Extremely gravelly sandy loam to extremely gravelly loamy fine
sand to very gravelly silt
Carsitas-Chuckawalla Extremely gravelly sand to extremely gravelly loamy fine sand to
very gravelly silt loam
Lithic Torriorthents Extremely gravelly sandy loam

*/

