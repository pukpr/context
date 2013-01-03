:- module(context_friction, [
			     ]).

/** <module> Specification for friction for various materials
    * Coefficient of friction table
    *
*/

:- context:register(context_friction:coefficient_friction_table).

%  standard(a(href('http://www.atlantaeng.com/aes_calculator_vehicle_speed_combined.html'),web)).
%
%
%%   standard(-Names)
%
%    Returns a list of (ISO) standards for friction coefficients
standard(Names) :-
    findall(p(Name), (rdf(UID, ent:standard, ent:friction),
                   rdfS(UID, ent:name, Name)), Names).

%%   coeff(-Friction_Info)
%
%     Return a tuple of cofficients of friction
coeff([Value, Desc, DSF, DFF, WSF, WFF, Constraint, Note]) :-
    rdfS(UID, ent:material, Value),
    rdfS(UID, ent:description, Desc),
    rdfS(UID, ent:dry_slow_friction, DSF),
    rdfS(UID, ent:dry_fast_friction, DFF),
    rdfS(UID, ent:wet_slow_friction, WSF),
    rdfS(UID, ent:wet_fast_friction, WFF),
    rdfS(UID, ent:constraint, Constraint),
    rdfS(UID, ent:note, Note).


%%   coefficient_friction_table(+Request)
%
%    Render HTML Table of coefficients of friction
coefficient_friction_table(_Request) :-
    findall(Row,coeff(Row), Rows),
    standard(Name),
    reply_html_page([title('Coefficient Friction Table'),
		     \(con_text:style)],
                   [
		    h1('Coefficient of Friction Categories'),
		    p(['From ' | Name]),
		    \(con_text:table_multiple_entries(
				   [[b('surface material'), b(description),
                                     b('dry slow friction'), b('dry fast friction'),
                                     b('wet slow friction'), b('wet fast friction'),
                                     b('constraint'), b('note')]],
				    Rows
						     )
		     )
                   ]).
