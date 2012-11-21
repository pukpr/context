:- module(context_ref_search, []).

/** <module> Reference search
    * Through Zotero database
    * Tagged with SWEET terms
*/

:- use_module(applications(browse)).
% :- use_module(components(graphviz)).

:- context:register(context_ref_search:search_sweet).
:- context:register(context_ref_search:cite).
:- context:register(context_ref_search:list_requirements).
:- context:register(context_ref_search:graph).

spec('humanEnvirStandards:EnvironmentalStandard').
foundation('humanResearch:Publication').

author_order('_1', 'lead author') :- !.
author_order('_2', 'second author') :- !.
author_order(_, 'supporting author').

%% article_link(+ArticleURI, +Title, -Href) is det.
%
%  Generates an Href link from a Titled article Resource
%
article_link(Article, Title, a(href(Link),Title) ) :-
   rdf(Article, dc:identifier, Ref),
   rdf(Ref, rdf:type, dcterms:'URI'),
   rdf(Ref, rdf:value, literal(Link)),
   !.
article_link(Article, Title, Link) :-
   context:uri_index_link(Article, Title, Link).

% Search for authors
find_title(Name, tr([td(Full_Name), td(Order),td(b(Doc)), td(b(Link))])) :-
   rdf_reachable(Ref, rdfs:subClassOf, bib:'Reference'),
%   rdf(Part, rdfs:subClassOf, bib:'Reference'),
%   ( rdf(Ref, rdfs:subClassOf, Part);
%     Ref=Part ),
   rdf(Article, rdf:type, Ref),
   rdf(Article, bib:authors, Authors),
   rdf(Authors, RefNum, NamedAuthor),
   rdf(NamedAuthor, foaf:surname, literal(exact(Name), Ref_Name)),
   (
   rdf(NamedAuthor, foaf:givenname, literal(First_Name)) ->
   true;
   First_Name = '@'
   ),
   atom_concat(First_Name, Ref_Name, Full_Name),
   rdf(Article, dc:title, literal(Title)),
   article_link(Article, Title, Link),
   rdf_split_url(_,R,RefNum),
   rdf_split_url(_,Doc,Ref),
   author_order(R, Order).

% Search for subjects
find_title(Name, tr([td(Name),td(Author_List), % [Author, ' : ', i(Order)]
			      td(Doc), td(b(Link))])) :-
   spec(Spec),
   foundation(Foundation),
   rdf(Part, dc:subject, literal(Name)),
   (    rdf(Part, dc:subject, literal(Foundation)) ->
	Doc = [img(src('/html/images/priority.gif')), 'model']
   ;
        rdf(Part, dc:subject, literal(Spec)) ->
	Doc = [img(src('/html/images/spec.gif')),'spec']
   ;
        Doc = 'ref'
   ),
   rdf(Part, dc:title, literal(Title)),
   article_link(Part, Title, Link),
   rdf(Part, bib:authors, Authors),
   findall('~w '-Author,
	   (
	    rdf(Authors, _RefNum, NamedAuthor),
	    % rdf_split_url(_,R,RefNum),
	    % author_order(R, Order),
	    rdf(NamedAuthor, foaf:surname, literal(Author))
	   ),
	   Author_List).

cite(Request) :-
   http_parameters(Request, [name(Name, [])]),
   findall(Title, find_title(Name,Title), List),
   reply_html_page(% cliopatria(default),
                   [title('Citations'),\(con_text:style_cliopatria)],
                   [ h1(Name),
                     table([class('block'),border(1)],
                           [tr([th('Name'), th('Who'), th('Action'), th('Title')])
                           |List]) ]).


%
%  SWEET specific
%
search_results(_,[]) -->
   {}.
search_results(M,[F|R]) -->
   {
    % this splits up "phen"+"AtmoPressure", i.e. dategory+concept
    % so phenAtmoPressure:Barometric is CategoryConcept:Term
   rdf_split_url(Prefix, Term, F),
   rdf_current_ns(NS, Prefix),
   context_sweet_utils:sweet_category(Category, Description),
   atom_concat(Category,Concept,NS)
   },
   odd_even_row(M, N, [td(Term), td(Concept), td(Description)]),
   search_results(N, R).


search_sweet(Request) :-
   http_parameters(Request, [name(Name, [])]),
   context:create_global_term(Name, Term),
   findall(Title, find_title(Name,Title), Titles),
   rdf_reachable(Term, rdf:type, owl:'Class'),
   findall(Subject, rdf_reachable(Subject, rdfs:subClassOf, Term), Subjects),
   reply_html_page(%cliopatria(default),
                   [title('Citations & List of Terms'),
                   \(con_text:style_cliopatria)],
           [ h1([Name, ' : ', a([href(['graph?name=',Name]),
                                 target(render)],
                                'Semantic Graph'
                               )
                ]),
             table([class('block'),border(1)],
                   [tr([th('Name'), th('Who'), th('Action'), th('Title')])
                                                | Titles]),
             h2('apropos terms'),
             table([class('block'),border(1)],
                   [ tr([th('Term'), th('Concept'), th('Category')]) |
                     \search_results(1,Subjects) ] )
             % h2('context graph'),
	     % \(context_graph(Term, []))
           ]).

graph(Request) :-
   http_parameters(Request, [name(Name, [])]),
   context:create_global_term(Name, Term),
   reply_html_page(
       [title('Semantic Graph'),
        \(con_text:style_cliopatria)
       ],
       [
           h2('context graph'),
           \(context_graph(Term, []))
       ]
                  ).


%
% Requirements
%

% Model path
find_context_requirement(Term, tr([td(Comment),
                                   td(Context),
                                   td(a(href(Link),doc)),
                                   % td(a(href(Workflow),path)),
				   td(a([href(Workflow),
                                        target('_parent')],
                                        path)),
                                   td([Begin, b(i(u(Middle))), Last]),
				   td(SweetName),
                                   td(ReqLink),
                                   td(Attribute),
                                   td(Category),
                                   td(System),
                                   td(Level)
                                  ])) :-
   rdf(Term, rdfs:comment, literal(Comment)),
%   (
%   rdf(Term, dcterms:'URI', Link);
%   ),
   rdf(Term, req:references, Model),
   rdf(Term, dc:subject, literal(SWEET)),
   context:replace_chars(':', ' ', SWEET, SweetName),
   rdf(Model, ent:workflow_path, literal(Workflow)),
   % concat('/', Workflow, RootWorkflow),
   rdf(Req, ent:req_text, literal(Narrative)),
   context:find_and_break_out_terms(Comment, Narrative, Begin, Middle, Last),
   rdf(Req, ent:req_attribute, literal(Attribute)),
   rdf(Req, ent:uri, literal(Link)),
   rdf(Req, ent:req_category, literal(Category)),
   rdf(Req, ent:req_sys_description, literal(System)),
   rdf(Req, ent:req_system_level, literal(Level)),
   (   rdf(Req, ent:context, literal(Context)) ->
       true
   ;
       Context='()'
   ),
   context:uri_index_link(Req, ReqLink).


% Doc path

/*
find_context_requirement(Term, tr([td(Comment), td(a(href(Link),Short)),
                                                td(Part)])) :-
   rdf(Term, rdfs:comment, literal(Comment)),
   % rdf(Term, dcterms:'URI', Link),
   rdf(RefTitle, rdfs:comment, literal(Comment)),
   rdf(RefTitle, rdfs:label, literal(Short)),
   rdf(RefTitle, dcterms:hasPart, literal(Part)),
   rdf(Link, dc:title, literal(Short)).
*/

find_context_requirement(Term, tr([td(Comment), td(Doc), td(Link),
				   td(a([href(Workflow),
                                        target('_parent')],
                                        path)),
				   td(Part),
				   td(SweetName),
				   td(ReqLink)])) :-
   spec(Spec),
   % Look for phrase
   rdf(Term, rdfs:comment, literal(Comment)),
   rdf(Term, dc:subject, literal(SWEET)),
   % Connect to Zotero
   rdf(RefTitle, dc:subject, literal(SWEET)),
   rdf(RefTitle, dc:subject, literal(Spec)),
   % rdf(RefTitle, dc:title, literal(Title)),
   article_link(RefTitle, spec, Link),
   % Look for reference points to spec
   rdf(Req, rdf:type, req:'ReferencePoint'),
   rdf(Req, rdfs:label, literal(Doc)),
   rdf(Req, rdfs:comment, literal(Comment)),
   rdf(Req, dc:subject, literal(Spec)),
   rdf(Req, dc:subject, literal(SWEET)),
   context:replace_chars(':', ' ', SWEET, SweetName),
   rdf(Term, req:references, Model),
   rdf(Model,ent:workflow_path, literal(Workflow)),
   % concat('/', Workflow, RootWorkflow),
   context:uri_index_link(Req, ReqLink),
   rdf(Req, dcterms:hasPart, literal(Part)).


list_requirements(Request) :-
   http_parameters(Request, [name(Term, [])]),
   print(user_error, [Term]),
   rdf_split_url(Prefix, Topic, Term),
   rdf(Term, rdf:'type', req:'Requirement'),
   findall(Req, find_context_requirement(Term,Req), Reqs),
   reply_html_page(% cliopatria(default),
                   [title('Requirements Workflow'),
                    % ,\(con_text:style_cliopatria)
                    \(con_text:style)
                   ],
                   [
                     \(con_text:multi_columns(
                                    [h2(Topic),
                                     i('from ontology graph labelled : '),
                                     tt(Prefix)
                                     ])),
                     table([class('block'),border(1)], [tr([th('Req Phrase'),
                                                            th('Context'),
							    th('Ref'),
							    th('Workflow'),
                                                            th('Requirement'),
							    th('SWEET'),
                                                            th('Graph'),
                                                            th('Attribute'),
                                                            th('Category'),
                                                            th('System'),
                                                            th('Level')]) | Reqs])
                   ]
                   ).









