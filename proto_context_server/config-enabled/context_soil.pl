:- module(context_soil, []).

/** <module> Soil classification
    * Uses ontological classification scheme
*/

:- context:register(context_soil:soil_table).


:- multifile(rdf_/3).

rdf_(ent:soil, ent:category, ent:coarse_grained_soil).
rdf_(ent:soil, ent:category, ent:fine_grained_soil).
rdf_(ent:soil, ent:category, ent:highly_organic_soil).

rdf_(ent:coarse_grained_soil, ent:division, ent:gravel).
rdf_(ent:coarse_grained_soil, ent:division, ent:sand).
rdf_(ent:coarse_grained_soil, ent:description, 'coarse grained').
rdf_(ent:coarse_grained_soil, ent:has, ent:gradation).
rdf_(ent:coarse_grained_soil, ent:has, ent:contents).

rdf_(ent:fine_grained_soil, ent:division, ent:moist_silt_clay).
rdf_(ent:fine_grained_soil, ent:division, ent:dry_silt_clay).
rdf_(ent:fine_grained_soil, ent:description, 'fine grained').
rdf_(ent:fine_grained_soil, ent:has, ent:plasticity).

rdf_(ent:highly_organic_soil, ent:description, 'highly organic').
rdf_(ent:highly_organic_soil, ent:has, ent:contents).
rdf_(ent:highly_organic_soil, ent:division, ent:organic).


rdf_(ent:gravel, ent:symbol, 'G').
rdf_(ent:gravel, ent:name, gravel).

rdf_(ent:sand, ent:symbol, 'S').
rdf_(ent:sand, ent:name, sand).

rdf_(ent:moist_silt_clay, ent:class, ent:clay).
rdf_(ent:moist_silt_clay, ent:class, ent:silt).

rdf_(ent:dry_silt_clay, ent:class, ent:clay).
rdf_(ent:dry_silt_clay, ent:class, ent:silt).

rdf_(ent:silt, ent:class, ent:contents).
rdf_(ent:silt, ent:symbol, 'M').
rdf_(ent:silt, ent:name, silt).
rdf_(ent:silt, ent:description, 'silt-like character').
rdf_(ent:silt, ent:has, ent:low_plasticity).

rdf_(ent:clay, ent:class, ent:contents).
rdf_(ent:clay, ent:symbol, 'C').
rdf_(ent:clay, ent:name, clay).
rdf_(ent:clay, ent:description, 'clay-like character').
rdf_(ent:clay, ent:has, ent:high_plasticity).

rdf_(ent:organic, ent:symbol, 'O').
rdf_(ent:organic, ent:name, organic).

rdf_(ent:low_gradation, ent:class, ent:gradation).
rdf_(ent:low_gradation, ent:symbol, 'P').
rdf_(ent:low_gradation, ent:description, 'poorly graded (uniform particle sizes)').

rdf_(ent:high_gradation, ent:class, ent:gradation).
rdf_(ent:high_gradation, ent:symbol, 'W').
rdf_(ent:high_gradation, ent:description, 'well-graded (diversified particle sizes)').

rdf_(ent:moist_silt_clay, ent:symbol, 'H').
rdf_(ent:high_plasticity, ent:class, ent:plasticity).
rdf_(ent:high_plasticity, ent:symbol, 'H').
rdf_(ent:high_plasticity, ent:description, 'high plasticity').

rdf_(ent:dry_silt_clay, ent:symbol, 'L').
rdf_(ent:low_plasticity, ent:class, ent:plasticity).
rdf_(ent:low_plasticity, ent:symbol, 'L').
rdf_(ent:low_plasticity, ent:description, 'low plasticity').

soil_combination([C, Div, TG, [Desc, ' containing ', Graininess]]) :-
   C = 'coarse grained',
   % Prefix
   rdf_(ent:soil, ent:category, Cat),
   rdf_(Cat, ent:description, C),
   rdf_(Cat, ent:division, Div),

   rdf_(Div, ent:symbol, Texture),
   rdf_(Div, ent:name, Desc),

   % Suffix
   (
     rdf_(Character, ent:class, ent:gradation);
     rdf_(Character, ent:class, ent:contents)
   ),
   rdf_(Character, ent:symbol, Grain),
   rdf_(Character, ent:description, Graininess),

   % Code
   atom_concat(Texture, Grain, TG).

soil_combination([C, Div, TG, [Desc, ' ', Plasticity, ' mainly ', Graininess]]) :-
   C = 'highly organic',
   % Prefix
   rdf_(ent:soil, ent:category, Cat),
   rdf_(Cat, ent:description, C),
   rdf_(Cat, ent:division, Div),

   rdf_(Div, ent:symbol, Texture),
   rdf_(Div, ent:name, Desc),

   % Suffix
   rdf_(Contents, ent:class, ent:contents),
   rdf_(Contents, ent:description, Graininess),

   rdf_(Character, ent:class, ent:plasticity),
   rdf_(Character, ent:symbol, Grain),
   rdf_(Character, ent:description, Plasticity),
   rdf_(Contents, ent:has, Plast),
   rdf_(Plast, ent:description, Plasticity),

   % Code
   atom_concat(Texture, Grain, TG).


soil_combination([C, Div, TG, [Desc, ' with ', Plasticity]]) :-
   C = 'fine grained',
   % Prefix
   rdf_(ent:soil, ent:category, Cat),
   rdf_(Cat, ent:description, C),

   rdf_(Cat, ent:division, Div0),
   rdf_(Div0, ent:symbol, Grain),
   rdf_(Div0, ent:class, Div),

   rdf_(Div, ent:symbol, Texture),
   rdf_(Div, ent:name, Desc),

   % Suffix
   rdf_(Character, ent:class, ent:plasticity),
   rdf_(Character, ent:symbol, Grain),
   rdf_(Character, ent:description, Plasticity),

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

