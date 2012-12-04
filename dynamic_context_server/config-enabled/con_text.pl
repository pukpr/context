:- module(con_text, []).

/** <module> HTML utilities
    * Form generation
    * Frames
*/

:- context:register(con_text:info).


% Similar to reply HTML page, but for frames
reply_frameset_page(Body) :-
	html_current_option(content_type(Type)),
	phrase(page(Body), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).

% blank page waiting for results
results_page(Text) :-
   reply_html_page(
     title(Text),
     [p(b(u(Text))) ]
     ).

info(Request) :-
    http_parameters(Request, [info(Info, [])]),
    reply_html_page(
     title('info'),
     [p(Info) ]
     ).


back_up(1) -->
    html(
        [p(a(href('javascript:history.back(1);'), '< [back]'))]
        ).

style -->
    html(
        link([ type('text/css'), rel('stylesheet'), href('/html/css/context.css') ])
        ).

style_cliopatria -->
    html(
        link([ type('text/css'), rel('stylesheet'), href('/css/cliopatria.css') ])
        ).

space_list([]) --> !.
space_list([F|R]) -->
    html([b(' + '),i(F)]),
    space_list(R).

dlist(_, []) --> !.
dlist(D, [F|R]) -->
    html([D, F]),
    dlist(D, R).

flist(F) -->
    {with_output_to(atom(A), write(F))},
    html(i(A)).

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
%    with_output_to(atom(Output), write(Text)).



% Simple Form
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

form(Action, List) -->
    html(form([class('query'), action(Action)],
              [ \input(List),
               input([type('submit')])])).

form(Action, Target, List) -->
    html(form([class('query'), action(Action), target(Target)],
              [ \input(List),
               input([type('submit')])])).

% Table-wrapped Form
table_header([]) --> !.
table_header([[Name,_]|Rest]) -->
    html(th([
             b(Name)
            ])),
    table_header(Rest).

table_input([]) --> !.
table_input([[Name,Default]|Rest]) -->
    html(td([
             input([type('text'),
                    name(Name),
                    value(Default)])
            ])),
    table_input(Rest).


table_form(Title, Action, List) -->
    {length(List, L)},
    html(form([action(Action)],
              [ table([class('block'),border(1)],
                      [
                    tr([\table_header(List)]),
                    tr([\table_input(List)]),
                    tr([td([colspan(L), align(center), bgcolor('palegreen')],
                           [b(Title),
                            input([type('submit')])
                           ]
                          )])
                      ]
                     )
                % , input([type('submit')])
              ])).

table_form_target(Title, Action, Target, List) -->
    {length(List, L)},
    html(form([action(Action), target(Target)],
              [ table([class('block'),border(1)],
                      [
                    tr([\table_header(List)]),
                    tr([\table_input(List)]),
                    tr([td([colspan(L), align(center), bgcolor('palegreen')],
                           [b(Title),
                            input([type('submit')])
                           ]
                          )])
                      ]
                     )
                % , input([type('submit')])
              ])).



radio_box_input_two( Name, [Name1, Value1], [Name2, Value2]) -->
    html([
        input([type('radio'),name(Name),value(Value1)]),b(Name1),
        br([]),
        input([type('radio'),name(Name),value(Value2)]),b(Name2),
        br([])
         ]
        ).

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

button_contents([]) --> !.
button_contents([[Content,Value]|Rest]) -->
    html(
        input([type('hidden'), name(Content), value(Value)])
        ),
    button_contents(Rest).



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
	        td( [width('40%'),valign(top)],
                    Left_Content ),
                td([width('60%'),valign(top)],
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


inline_button(Content) -->
    html(div([style='display: inline-block; clear: both; height: 1px;'],
             [Content])).


uri_index_link(URI) -->
   {
    uri_encoded(path, URI, U)
   },
   html(a(href('/browse/list_resource?r='+U), '[link]')).


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

multi_columns(Content) -->
   html(table([border(0), width('100%')],
	      [tr([
		   \(each_column(Content))
		  ]
		 )
	      ]
	     )
       ).

def_list(A=B) -->
    html(dl([dt(A),dd(B)])).

paragraphs([]) --> !.
paragraphs([F|R]) -->
   {with_output_to(atom(Text), write(F))},
   html(p(Text)),
   (   {is_list(F)} ->
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

table_entries(Content) -->
   html(table([border(0),
	       style('margin-left:40pt;font-family: Arial, Verdana, sans-serif;')
	      ],
	      [tr([th([bgcolor(blue), style('color:white;')], 'Attribute'),
		   th([bgcolor(blue), style('color:white;')], 'Value')]),
	       \(each_entry(Content))]
	     )
       ).

each_cell(_Cell, []) --> !.
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

table_multiple_entries(Header, Content) -->
   html(table([border(1)],
	      [\(each_row(th, Header)),
	       \(each_row(td, Content))
	      ]
	     )
       ).


gif(Name) -->
   {
     rdf_(Name, ent:description, Title) ->
     true
    ;
     Title = Name
   },
   html(img([src('/html/images/'+Name+'.gif'),
             title(Title)])).


collect_options(Functor, List) :-
    findall(option([value(Value)],[Name]),
            call(Functor, Name, Value),
            List).



/*  % do an iframe
       \(context_html_utils:radio_box_two('Testing div', 'tester', 'myid',
                                          'pick', [one, '1'], [two, '2'])),
       iframe(name='myid',[p(nothing)])
*/
