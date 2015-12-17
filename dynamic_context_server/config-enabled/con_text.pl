:- module(con_text, [
                     obj_result/2,
                     acronym//1
                    ]).

/** <module> HTML utilities
    * Form generation
    * Frames
*/

:- context:register(con_text:info).
:- context:register(con_text:resources).

% :- dynamic acronym_definition/2.

%%   acronym(+Def)//
%
%    Acronym list
acronym(Def) -->
    {
       acronym_definition(Def, Text),
       format(atom(S), '~w', [Text])
    },
    html(abbr([title=S], Def)).

%%   reply_frameset_page(+Body)
%
%    Similar to reply HTML page, but for frames
reply_frameset_page(Body) :-
	html_current_option(content_type(Type)),
	phrase(page(Body), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

%%   results_page(+Text)
%
%    blank page waiting for results
results_page(Text) :-
   reply_html_page(
     title(Text),
     [p(b(u(Text))) ]
     ).

%%   info(+Request)
%
%    info box
info(Request) :-
    http_parameters(Request, [info(Info, [])]),
    reply_html_page(
     title('info'),
     [p(Info) ]
     ).


%%   back_up(+N)//
%
%    Backup page
back_up(1) -->
    html(
        [p(a(href('javascript:history.back(1);'), '< [back]'))]
        ).

%%   style//
%
%    style sheet
style -->
    html(
        link([ type('text/css'), rel('stylesheet'), href('/html/css/context.css') ])
        ).

%%   style_cliopatria//
%
%    style sheet like cliopatria
style_cliopatria -->
    html(
        link([ type('text/css'), rel('stylesheet'), href('/css/cliopatria.css') ])
        ).

%%   style_submit//
%
%    interactive submit javascript
style_submit -->
    html(
	script([type('text/javascript'),src('/html/js/submit.js')], [])
	).

%%   target_iframe//
%
%    submit form to target iframe
target_iframe -->
    html(
        'subm(this.form,"target_iframe");'
	).

%%   target_render//
%
%    submit form to render iframe
target_render -->
    html(
        'subm(this.form,"render");'
	).

%%   space_list(+L)//
%
%    spaced list of items
space_list([]) --> !.
space_list([F|R]) -->
    html([b(' + '),i(F)]),
    space_list(R).

%%   dlist(+D,+L)//
%
%    paired list of item
dlist(_, []) --> !.
dlist(D, [F|R]) -->
    html([D, F]),
    dlist(D, R).

%%   flist(+F)//
%
%
flist(F) -->
    {with_output_to(atom(A), write(F))},
    html(i(A)).

%%   alert(+Link_Text, +Alert)//
%
%    alert box
alert(Link_Text, Alert) -->
    html(a([href='#',
            onclick='alert("' + Alert +'");'
           ],
           Link_Text)).

alert(Link_Text, Alert, Text) :-
    with_output_to(atom(Info), print(Link_Text)),
    atomic_list_concat(['alert("', Info, '");'], Click),
    Text = a([href='#', onclick=Click],
              Alert).

%%   info(+Link_Text, +Output)//
%
%    info box
info(Link_Text, Output) :-
    with_output_to(atom(Info), print(Link_Text)),
    atomic_list_concat(['javascript:alert("', Info, '");'], Output).



%%   input(+Pair)//
%
%    Simple Form
input([]) --> !.
input([[Name,Default]|Rest]) -->
    html(
        input([type('text'),
               name(Name),
               value(Default)])
        ),
    input(Rest).
input([[Name,Default,Size]|Rest]) -->
    html(
        input([type('text'),
               name(Name),
               value(Default),
               size(Size)])
        ),
    input(Rest).


%%   input_cells(+Pair)//
%
%    Simple Table Form
input_cells([]) --> !.
input_cells([[Name,Default]|Rest]) -->
    html(
        tr([
	    th(Name),
	    td(
		input([type('text'),
		       name(Name),
		       value(Default)])
	      )
	   ])
        ),
    input_cells(Rest).
input_cells([[Name,Default,Size]|Rest]) -->
    html(
	tr([
	    th(Name),
	    td(
		input([type('text'),
		       name(Name),
		       value(Default),
		       size(Size)])
	      )
	   ])
	),
    input_cells(Rest).

%%   form(+Action, +List)//
%
%    Interactive form
form(Action, List) -->
    html(form([class('query'), action(Action)],
              [ \input(List),
               input([type('submit')])])).

form(Action, Target, List) -->
    html(form([class('query'), action(Action), target(Target)],
              [ \input(List),
               input([type('submit')])])).

%%   reload//
%
%    reload page
reload -->
  html(a([href('javascript:location.reload(true);')], ' refresh')).


%%   form_ac(+Action, +Target, +Subject_Filter, +ID)//
%
%    Form autocomplete
form_ac(Action, Target, Subject_Filter, ID) -->
    html([ \(reload),
           form([action(Action), target(Target)],
              [ \autoc(Subject_Filter, ID), br([]),
                input([type('submit')])
               ])
         ]
        ).


%%   autoc(+Callback, +ID)//
%
%    Autocomplete functor
autoc(Callback, ID) -->
    {
     with_output_to(atom(CB),format('~w=""', Callback))
    },
    html( div(\(autocomplete_predicates:autocomplete(find_term,
                                                     [   query_delay(0.3),
                                                         auto_highlight(false),
                                                         max_results_displayed(15),
                                                         width('30em'),
                                                         name(ID),
                                                         value(CB)
                                                     ])))).

%%   ac(+Resource, +ID)//
%
%    Autocomplete functor
ac(Resource, ID) -->
    html( div(\(autocomplete_predicates:autocomplete(Resource, [   query_delay(0.3),
                                                                   auto_highlight(false),
                                                                   max_results_displayed(10),
                                                                   width('30em'),
                                                                   name(ID),
                                                                   value('')
                                                                 ])))).

%%   first_n(+Max, +Completions1, +Completions2)
%
%
first_n(0, _, []) :- !.
first_n(_, [], []) :- !.
first_n(N, [H|T0], [H|T]) :-
	N2 is N - 1,
	first_n(N2, T0, T).

%%   area(+Str, +City, +Name)
%
%    Example autocomplete
area(_Str, 'Baltimore', 'temperature record').
area(_Str, 'Wilmington', 'temperature record').
area(_Str, 'conus', 'slope record').

%%   autocompletions(+Query, +Max, +Count, -Completions)
%
%    Autocompletions function
autocompletions(Query, Max, Count, Completions) :-
    print(user_error,['Q', Query]),
    findall([Obj,Type], area(Query, Obj, Type), Completions0),
    sort(Completions0, Completions1),
    length(Completions1, Count),
    first_n(Max, Completions1, Completions2),
    maplist(obj_result, Completions2, Completions).

%%   obj_result(+Pair, -JSON)
%
%    Used by autocompletion
obj_result([Obj,Type], json([ label=Obj,
                              type=Type,
                              href='javascript:location.reload(false);'
                            ])).

%%   resources(+Request)
%
%    Used by autocompletion
resources(Request) :-
	http_parameters(Request,
			[ query(Query, [description('Typed string')]),
			  maxResultsDisplayed(Max, [integer, default(100), description('Max number of results to show') ])
			]),
	autocompletions(Query, Max, Count, Completions),
	reply_json(json([ query = json([ count=Count ]),
			  results = Completions
			])).

:- http_handler('/con_text/resources', resources, []).

%%   table_header(+List)//
%
%    Table-wrapped Form
table_header([]) --> !.
table_header([[Name,_]|Rest]) -->
    html(th([width('100%')],
            [
             b(Name)
            ])),
    table_header(Rest).


%%   table_input(+List)//
%
%    Table input with autocomplete
table_input([]) --> !.
table_input([[Name,Default]|Rest]) -->
    html(td([valign=top],
            [
            div(\(autocomplete_predicates:autocomplete(resources, [
                                                                   query_delay(0.3),
                                                                   auto_highlight(false),
                                                                   max_results_displayed(10),
                                                                   width('100%'),
                                                                   name(Name),
                                                                   value(Default)
                                                                 ]))),
             br([])
             ]

            )),
    table_input(Rest).

%%   table_input_pair(+Pairs)//
%
%    Table inputs
table_input_pair([]) --> !.
table_input_pair([[Name,Default]|Rest]) -->
    html(tr([
	 th([valign=top, width='20%'],Name),
	 td([valign=top],
            [
            div(\(autocomplete_predicates:autocomplete(resources, [
                                                                   query_delay(0.3),
                                                                   auto_highlight(false),
                                                                   max_results_displayed(10),
                                                                   width('100%'),
                                                                   name(Name),
                                                                   value(Default)
                                                                 ])))
             ]
            )])),
    table_input_pair(Rest).


%%   table_form(+Title, +Action, +List)//
%
%    Table form
table_form(Title, Action, List) -->
    {length(List, L)},
    html(form([action(Action)],
              [ table([class('block'),border(1)],
                      [
                    tr([\table_header(List)]),
                    tr([\table_input(List)]),
                    tr([td([colspan(L), align(right), bgcolor('palegreen')],
                           [b(Title),
                            input([type('submit')])
                           ]
                          )])
                      ]
                     )
                % , input([type('submit')])
              ])).

%%   table_form_target(+Title, +Action, +Target, +List)//
%
%    Table from with target
table_form_target(Title, Action, Target, List) -->
/*    {
     length(List, L)
    },
	*/
    html(form([action(Action), target(Target)],
              [ table([class('block'),border(1),margin(2)],
                      [
                    % tr([\table_header(List)]),
                    % tr([\table_input(List)]),
		    \(table_input_pair(List)),
                    tr([td([colspan(2), align(center), bgcolor('palegreen')],
                           [b(Title),
                            input([type('submit')])
                           ]
                          )])
                      ]
                     )
                % , input([type('submit')])
              ])).



%%   radio_box_input_two(+Name, +Pair1, +Pair2)//
%
%    Radio box, limit two items
radio_box_input_two( Name, [Name1, Value1], [Name2, Value2]) -->
    html([
        input([type('radio'),name(Name),value(Value1)]),b(Name1),
        br([]),
        input([type('radio'),name(Name),value(Value2)]),b(Name2),
        br([])
         ]
        ).

%%   radio_box_two(+Title, +Action, +Target, +Name, +Pair1, +Pair2)//
%
%    Radio box with target, limit two items
radio_box_two(Title, Action, Target, Name, [Name1, Value1], [Name2, Value2]) -->
   html(form([action(Action),
              target(Target)],
        [table(
             [class('block'),border(1)],
             [
              tr(
             [th([colspan(3), align(center)],
                 b(Title)),
              td(table([border(0)],
                       [tr([td(b(Name1)),
                            td(input([type('radio'),
                                      name(Name),
                                      value(Value1)]))]),
                        tr([td(b(Name2)),
                            td(input([type('radio'),
                                      name(Name),
                                      value(Value2 )]))])])),
              td([colspan(3), align(center), bgcolor('palegreen')],
                 input([type('submit')]))
             ]
            )
         ])])).

%%   radio_toggles(+Name,+List)//
%
%    Radio toggles
radio_toggles(_Name, []) --> !.
radio_toggles(Name, [[NameN, ValueN]|Rest]) -->
    html([
        input([type('radio'),
               name(Name),
               value(ValueN)],
              b(NameN)),
        br([])
         ]
        ),
    radio_toggles(Name, Rest).

%%   number_radio_toggles(+Name, +List)
%
%    Numbered radio toggles
number_radio_toggles(_Name, []) --> !.
number_radio_toggles(Name, [Num|Rest]) -->
    html([
        input([type('radio'),
               name(Name),
               value(Num)],
              b(Num))
         ]
        ),
    number_radio_toggles(Name, Rest).

/*
radio_box_input_two( Name, [Name1, Value1], [Name2, Value2]) -->
    html([
        input([type('radio'),name(Name),value(Value1)]),b(Name1),
        br([]),
        input([type('radio'),name(Name),value(Value2)]),b(Name2),
        br([])
         ]
        ).
*/

%%   radio_boxes(+Title, +Action, +Target, +Name, +List)//
%
%    Radio boxes inside table
radio_boxes(Title, Action, Target, Name, List) -->
   html(form([action(Action),
              target(Target)],
        [table(
             [class('block'),border(1)],
             [
              tr(
             [th([colspan(3), align(center)],
                 b(Title)),
              td(table([border(0)],
                       \(radio_toggles(Name, List))
                      )),
              td([colspan(3), align(center), bgcolor('palegreen')],
                 input([type('submit')]))
             ]
            )
         ])])).

%%   button_contents(+List)//
%
%    Button contents, hidden to store info
button_contents([]) --> !.
button_contents([[Content,Value]|Rest]) -->
    html(
        input([type('hidden'), name(Content), value(Value)])
        ),
    button_contents(Rest).



%%   button_link(+Name, +Action, +Target)//
%
%    Button link to target
button_link(Name, Action, Target) -->
    html(form([action(Action),
              target(Target)],
              input([type('submit'), name(Name), value(Name)])
             )
        ).

button_link(Name, Action, Target, [Content,Value]) -->
    {atom(Content), atom(Value)},
    html(form([action(Action),
              target(Target)],
              [input([type('submit'), name(Name), value(Name)]),
               input([type('hidden'), name(Content), value(Value)])
              ]
             )
        ).

button_link(Name, Action, Target, Contents) -->
    html(form([action(Action),
              target(Target)],
              [
               input([type('submit'), name(Name), value(Name)]),
               \button_contents(Contents)
              ]
             )
        ).

%%   check_box(+Name, +Action, +Target)//
%
%   Check box to target
check_box(Name, Value, Description) -->
    html(
	input([type('checkbox'), name(Name), value(Value)], Description)
        ).
check_box(Name, Value, Description,Checked) -->
    html(
	input([type('checkbox'), name(Name), value(Value), Checked], Description)
        ).


%%   table_with_iframe_target(+Request, +Left_Content)//
%
%    Create a table with a left side cell content and iframe on right
table_with_iframe_target(Request, Left_Content) -->
    {
      IName = target_iframe,
      context:holder(Request, Name),
      ref_(Name, IName, Page),
      (
        ref_(Name, feature, Feature),
        rdf_(Feature, ent:image, Img)
      ->
        atomic_concat('/html/static_pages/gems/',Img, I),
        Image = a([href('javascript:location.reload(true);')],
                   img([src(I),
		     height(32),
		     title(Feature)]
		      ))
      ;
        Image = ''
      )
    },
    html([
	% Image,
        table([border(0), width('100%')],
          [tr(
	       [
		td( [valign(top)],
                    Image ),
	        td( [width('25%'),valign(top)], %40
                    Left_Content ),
                td([width('75%'),valign(top)],	%60
                   [iframe([name(IName),
                            width('100%'),
                            height('800'),
                            frameborder(0),
			    % src('/html/images/'+IName+'.gif')
                            src(Page)
                           ],
                           []
                          )
                   ])
               ]
             )
          ]
         )]
        ).


%%   table_with_iframe_lower_target(+Request, +Upper_Content)//
%
%    Create a table with a upper cell content and iframe below
table_with_iframe_lower_target(Request, Upper_Content) -->
    {
      IName = target_iframe,
      context:holder(Request, Name),
      ref_(Name, IName, Page)
    },
    html(
        table([border(0), width('100%')],
          [tr(
	       [td( [width('100%'),valign(top)],
                    Upper_Content )
               ]
             ),
           tr([
               td([width('100%'),valign(top)],
                   [iframe([name(IName),
                            width('100%'),
                            height('800'),
                            frameborder(0),
			    src(Page)
                           ],
                           []
                          )
                   ])
               ]
             )
          ]
         )
        ).

%%   render_iframe(+Request, +Height)//
%
%    Create a render iframe
render_iframe(Request, Height) -->
    {
      (  atom(Request) ->
         IName = Request
      ;
         IName = target_iframe
      ),
      context:holder(Request, Name),
      ref_(Name, IName, Page)
    },
   html(iframe([name(IName),
		width('100%'),
		height(Height),
                frameborder(0),
		src(Page)
	       ],
	       [])).

render_iframe(Request) -->
    {
      (  atom(Request) ->
         IName = Request
      ;
         IName = target_iframe
      ),
      context:holder(Request, Name),
      ref_(Name, IName, Page)
    },
   html(iframe([name(IName),
		width('100%'),
		height('400'),
                frameborder(0),
		src(Page)
	       ],
	       [])).


%%   inline_button(+Content)//
%
%    Inline button
inline_button(Content) -->
    html(div([style='display: inline-block; clear: both; height: 1px;'],
             [Content])).


%%   uri_index_link(+URI)//
%
%    URI index link
uri_index_link(URI) -->
   {
    uri_encoded(path, URI, U)
   },
   html(a(href('/browse/list_resource?r='+U), '[link]')).


%%   two_columns(+Content1, +Content2)//
%
%    Two columns
two_columns(Content1, Content2) -->
   html(table([border(0), width('100%')],
	      [tr([td([width('50%'),valign(top)],[Content1]),
		   td([width('50%'),valign(top)],[Content2])])]
	     )
       ).

each_column([]) --> !.
each_column([F|R]) -->
   html(td([], F)),
   each_column(R).

%%   multi_columns(+Content)//
%
%    Multi columns from list
multi_columns(Content) -->
   html(table([border(0), width('100%')],
	      [tr([
		   \(each_column(Content))
		  ]
		 )
	      ]
	     )
       ).

%%   def_list(+Pair)//
%
%    Definition list
def_list(A=B) -->
    html(dl([dt(A),dd(B)])).

%%   paragpaphs(+List)//
%
%    List of paragraphs
paragraphs([]) --> !.
paragraphs([F|R]) -->
   {with_output_to(atom(Text), write(F))},
   (   {is_list(F)} ->
       html(p(Text)),
       paragraphs(F)
   ;
       def_list(F)
   ),
   paragraphs(R).



each_entry([]) --> !.
each_entry([[Name,Value]|R]) -->
   html(tr([
	    td([width('50%'),valign(top)], b(Name)),
	    td([width('50%'),valign(top)], Value)
	    ]
	  )
       ),
   each_entry(R).

%%   table_entries(+Attribute, +Value, +Content)//
%
%    Table of attibute-value entries
table_entries(Attribute, Value, Content) -->
   html(table([border(0),
	       style('margin-left:40pt;font-family: Arial, Verdana, sans-serif;')
	      ],
	      [tr([th([bgcolor(blue), style('color:white;')], Attribute),
		   th([bgcolor(blue), style('color:white;')], Value)]),
	       \(each_entry(Content))]
	     )
       ).

table_entries(Content) -->
   html(
       \(table_entries('Attribute', 'Value', Content))
       ).

each_cell(_Cell, []) --> !.
each_cell(td, [F|R]) -->   % if list just take head
   {is_list(F)},
   html(td(\(table_multiple_entries([],[F])))),
   each_cell(td,R).
each_cell(td, [F|R]) -->
   html(td(F)),
   each_cell(td,R).
each_cell(th, [F|R]) -->
   html(th(F)),
   each_cell(th,R).

each_row(_Cell, []) --> !.
each_row(Cell, [F|R]) -->
   html(tr([
	    \(each_cell(Cell,F))
	    ]
	  )
       ),
   each_row(Cell, R).

%%   table_multiple_entries(+Header, +Content)//
%
%    table of multiple entries
table_multiple_entries(Header, Content) -->
   html(table([border(1)],
	      [\(each_row(th, Header)),
	       \(each_row(td, Content))
	      ]
	     )
       ).


%%   gif(+Name)//
%
%    Image item
gif(Name) -->
   {
     rdf_(Name, ent:description, Title) ->
     true
    ;
     Title = Name
   },
   html(img([src('/html/images/'+Name+'.gif'),
             title(Title)])).


%%   collect_options(+Functor, +List)
%
%    Used to generate a list of options
collect_options(Functor, List) :-
    findall(option([value(Value)],[Name]),
            call(Functor, Name, Value),
            List).



/*  % do an iframe
       \(context_html_utils:radio_box_two('Testing div', 'tester', 'myid',
                                          'pick', [one, '1'], [two, '2'])),
       iframe(name='myid',[p(nothing)])
*/
