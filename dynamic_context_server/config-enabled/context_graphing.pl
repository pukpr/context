:- module(context_graphing, []).

/** <module> Graphing Interface for PSD,PDF,etc
    * Dynagraphs -- JavaScript
    * Google Charts -- JavaScript (remote host)
    * Visquick/Protovis -- JavaScript
    * R -- Library interface
*/

%%   is_log(+Log, -Bool)
%
%    If log selected
is_log(lin, 0).
is_log(log, 1).
is_log(0, 0).
is_log(1, 1).
is_log(false, 0).
is_log(true, 1).

% %%%%%%%%%%%%
% Scatter Plot
% %%%%%%%%%%%%

:- html_meta
      pre_scatter_script_load(?,?),
      % scatter_script_load(?,?),
      scatter_plot(+,+,+,+,?,?),

      pre_chart_script_load(?,?),
      % chart_script_load(?,?),
      chart_plot(+,+,+,+,+,?,?),

      pre_dygraph_script_load(?,?),
      % dygraph_script_load(?,?),
      dygraph_native(+,+,+,+,+,+,?,?),
      dygraph_error_bars(+,+,+,+,+,+,?,?),
      dygraph_plot(+,+,+,+,+,+,?,?),

      pre_map_load(?,?),
      map_native(+,+,+,?,?),

      pre_plotly_script_load(?,?),
      plotly_contour(+,+,+,+,?,?).

%%   json_complete(+X, +Y, +In, -Out)
%
%    Custom JSON parse
json_complete(x(_,[]), y(_,[],_), In, Out) :-
   atomic_list_concat(['[', In,']'], Out).
json_complete(x(X,S), y(Y,P,Shape), In, Out) :-
   atomic_list_concat(['"',Shape,' ',S,',',P,'"'], Id),
   atomic_list_concat(['[', In, '{"id":',Id,',',X,':',S,',',Y,':',P,'}]'], Out).
%%   json_concat(+X, +Y, +In, -Out)
%
%    Custom JSON parse
json_concat(x(X,S), y(Y,P,Shape), In, Out) :-
   % atomic_list_concat(['"',Shape,' ',S,',',P,'"'], Id),
   format(atom(Id), '"~w ~2g,~2g"', [Shape,S,P]),
   format(atom(Str), '{"id":~w, ~w:~w, ~w:~w},', [Id,X,S,Y,P]),
   atom_concat(In, Str, Out).
   % atomic_list_concat([     In, '{"id":',Id,',',X,':',S,',',Y,':',P,'},'], Out).

%%   construct_json(+X, +Y, +ID, +V, +P, +In, -Out)
%
%    Special JSON parse
construct_json(X, Y, ID, [[V]], [[P]], In, Out) :-
    json_complete(x(X,V), y(Y,P,ID), In, Out).
construct_json(X, Y, ID, [[V]|VR], [[P]|PR], In, Out) :-
    json_concat(x(X,V), y(Y,P,ID), In, Next),
    construct_json(X, Y, ID, VR, PR, Next, Out).

construct_json(X, Y, ID, [V], [P], In, Out) :-
    json_complete(x(X,V), y(Y,P,ID), In, Out).
construct_json(X, Y, ID, [V|VR], [P|PR], In, Out) :-
    json_concat(x(X,V), y(Y,P,ID), In, Next),
    construct_json(X, Y, ID, VR, PR, Next, Out).



%%   pre_scatter_script_load//
%
%    Javascript preload
pre_scatter_script_load -->
    html([
        script([type('text/javascript'),src('/html/js/google-code.js')],     []),
        script([type('text/javascript'),src('/html/js/protovis-d3.3.1.js')], []),
        script([type('text/javascript'),src('/html/js/core/base.js')],       []),
        script([type('text/javascript'),src('/html/js/core/events.js')],     []),
        script([type('text/javascript'),src('/html/js/core/models.js')],     []),
        script([type('text/javascript'),src('/html/js/core/utils.js')],      []),
        script([type('text/javascript'),src('/html/js/core/vis.js')],        []),
        script([type('text/javascript'),src('/html/js/scatterplot/scatterplot.js')],     []),
        script([type('text/javascript'),src('/html/js/handledata.js')],     [])
        ]).

%%   scatter_script_load//
%
%    Javascript preload
scatter_script_load -->
   html([meta([name('description'),content('graph page')])]).

%%   scatter_plot(+LogScale,+XAxis,+YAxis,+XY)//
%
%    Scatter plot graph
scatter_plot(LogScale,XAxis,YAxis,[X,Y]) -->
   {
     is_log(LogScale, Log),
     quote_string(XAxis, XA),
     quote_string(YAxis, YA),
     context_graphing:construct_json(XA, YA, 'o', X, Y, '', Data)
   },
   html([
         \pre_scatter_script_load,
         div([id('masterContainer'), style('display:block;float:none;')], []),
         script([type('text/javascript')],
         [
          % \js_call(handleScatterData(LogScale, XA, YA, Data))
          'handleScatterData(', Log, ',', XA, ',', YA, ',', Data, ');'

         ])
        ]).

scatter_plot(LogScale,X,Y,Data) -->
   {
     is_log(LogScale, Log)
   },
   html([
         \pre_scatter_script_load,
         div([id('masterContainer'), style('display:block;float:none;')], []),
         script([type('text/javascript')],
         [
          %  \js_call(handleScatterData(LogScale, X, Y, Data))
          'handleScatterData(', Log, ',', X, ',', Y, ',', Data, ');'
         ])
        ]).


% %%%%%%%%%%%%
% Chart Plot
% %%%%%%%%%%%%

%%   pre_chart_script_load//
%
%    Javascript preload
pre_chart_script_load -->
   html([
         script([type('text/javascript'),src('https://www.google.com/jsapi')], []),
         script([type('text/javascript')],
                ['google.load("visualization", "1", {packages:["corechart","controls"]});'] ),
         script([type('text/javascript'),src('/html/js/handledata.js')], [])
        ]).

%%   chart_script_load//
%
%    Javascript preload
%    these meta terms are placeholders, not used
chart_script_load -->
   html([meta([name('description'),content('graph page')])]).

%%   chart_plot(+Log, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Chart plot routine
chart_plot(Log, Xaxis, Yaxis, Title, Data) -->
   html([
         \pre_chart_script_load,
         % \slider,
         div([id('dashboard')],
             [div([id('slider_div'), style('display:block;float:none;')], []),
              div([id('chart_div'), style('display:block;float:none;')], [])
             ]) ,
         script([type('text/javascript')],
         [
          'drawVisualization(',Log,',',Xaxis,',',Yaxis,',',Title,',',Data,');'
         ]),
         a([href('#'),onclick('alert(getSliderLo());')], 'check slider low value'),
         p([]),
         a([href('#'),onclick('alert(getSliderHi());')], 'check slider high value')
        ]).


% %%%%%%%%%%%%
% dygraph Plot
% %%%%%%%%%%%%

%%   check_list(+X,-QX)
%
%    Check list of data for proper quotes, etc
check_list(X,QX) :-
    atom(X),
    quote_string(X, QX).
check_list(List, Quoted_String) :-
    concat_strings(List, Quoted_String).

%%   check_data(+Headings, +List, -CSV)
%
%    Generates a CSV list appropiate for charting
check_data('',X,X) :-
    atom(X).
check_data(Headings, List, CSV) :-
    atom_concat(Headings, '\\n', H),
    quote_string(H, First_Line),
    construct_csv(List, First_Line, CSV).

%%   dygraph_script_load//
%
%    Javascript preload
dygraph_script_load -->
   html([meta([name('description'),content('graph page')])]).

%%   pre_dygraph_script_load//
%
%    Javascript preload
pre_dygraph_script_load -->
   html([
         script([type('text/javascript'),src('/html/js/dygraph-combined.js')], []),
         script([type('text/javascript'),src('/html/js/handledata.js')],       [])
        ]).


%%   dygraph_plot(+LogScale, +Headings, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Dynamic Graph plot
dygraph_plot(LogScale, Headings, Xaxis, Yaxis, Title, Data) -->
   {check_list(Xaxis,X),
    check_list(Yaxis,Y),
    check_list(Title,T),
    check_data(Headings,Data,D),
    is_log(LogScale,Log),
    print(user_error, '-dygraph plotting-')},
   html([
         \pre_dygraph_script_load,
         div([id('graphdiv'), style('display:block')], []),
         script([type('text/javascript')],
         [
          'handleDygraphData(', Log, ',', X, ',', Y, ',', T, ',', D, ');'
         ])
        ]).

%%   dygraph_native(+LogScale, +Headings, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Dynamic Graph plot, using the native format which is most efficient
dygraph_native(LogScale, Headings, Xaxis, Yaxis, Title, Data) -->
   {
    is_log(LogScale,Log),
    (	is_list(Title) ->
	atomic_list_concat(Title, Text)
    ;
       Text = Title
    )
   },
   html([
         \pre_dygraph_script_load,
         div([id('graphdiv'), style('display:block')], []),
         script([type('text/javascript')],
         [
          \js_call('handleDygraphList'(Log,Xaxis,Yaxis,Text,Headings,Data))
         ])
        ]).

pad_header([], In, Out) :- reverse(In,Out).
pad_header([F|R], In, Out) :-
   atom_concat(F, ' ', Str),
   pad_header(R, [Str|In], Out).


%%   dygraph_error_bars(+LogScale, +Headings, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Dygraph error bar prototype, still under development
dygraph_error_bars_base(LogScale, Headings, Xaxis, Yaxis, Title, Data, Bars, Length) -->
   {
    % pad_header(Headings, [], Headers),
    is_log(LogScale,Log),
    is_log(Bars,CustomBars),
    (	is_list(Title) ->
	atomic_list_concat(Title, Text)
    ;
       Text = Title
    )   },
   html([
         \pre_dygraph_script_load,
         div([id('graphdiv'), style('display:block')], []),
         script([type('text/javascript')],
         [
          \js_call('handleDygraphListErrorBars'(Log,Xaxis,Yaxis,Text,Headings,Data,CustomBars, Length, 400))
         ])
        ]).

dygraph_error_bars(LogScale, Headings, Xaxis, Yaxis, Title, Data, Bars) -->
    dygraph_error_bars_base(LogScale, Headings, Xaxis, Yaxis, Title, Data, Bars, 1200).


% list of xy pairs
%%   construct_csv(+XY, +In, -Out)
%
%    Construct a CSV list
construct_csv([], Out, Out).
construct_csv([xy(X,Y)|R], In, Out) :-
    atomic_list_concat([In, '+ "', X, ',', Y, '\\n"'], Next),!,
    construct_csv(R, Next, Out).

% list of xyz pairs
% construct_csv([], Out, Out).
construct_csv([xyz(X,Y,Z)|R], In, Out) :-
    atomic_list_concat([In, '+ "', X, ',', Y, ',', Z, '\\n"'], Next),!,
    construct_csv(R, Next, Out).

% list of two lists
construct_csv([[], []], Out, Out).
construct_csv([[F|FR], [R|RR]], In, Out) :-
    atomic_list_concat([In, '+ "', F, ',', R, '\\n"'], Next),!,
    construct_csv([FR, RR], Next, Out).

% list of three lists
construct_csv([[], [], []], Out, Out).
construct_csv([[F|FR], [S|SR], [R|RR]], In, Out) :-
    atomic_list_concat([In, '+ "', F, ',', S, ',', R, '\\n"'], Next),!,
    construct_csv([FR, SR, RR], Next, Out).


% two lists input
construct_csv([], [], Out, Out).
construct_csv([F|FR], [R|RR], In, Out) :-
    atomic_list_concat([In, '+ "', F, ',', R, '\\n"'], Next),!,
    construct_csv(FR, RR, Next, Out).

% three lists input
construct_csv([], [], [], Out, Out).
construct_csv([F|FR], [S|SR], [R|RR], In, Out) :-
    atomic_list_concat([In, '+ "', F, ',', S, ',', R, '\\n"'], Next),!,
    construct_csv(FR, SR, RR, Next, Out).

%%   quote_string(+Term, -Q)
%
%    Quote a term
quote_string(Term, Term) :-  % already has a quote
    atom_codes(Term, [34|_]), !.
quote_string(Term, Quoted) :-
    atomic_list_concat(['"',Term,'"'], Quoted).

%%   concat_strings(+List, -Quoted_String)
%
%    Concat strings into quoted string
concat_strings(List, Quoted_String) :-
    atomic_list_concat(List, String),
    quote_string(String, Quoted_String).


%%   axis_label(+Label, +Units, -Output)
%
%    Label an axis
axis_label(Label, Units, Output) :-
    atomic_list_concat([Label, ' (', Units, ')'], Name),
    quote_string(Name, Output).

%%   plain_axis_label(+Label, +Units, -Name)
%
%    Non-quoted *axis_label*
plain_axis_label(Label, Units, Name) :-
    atomic_list_concat([Label, ' (', Units, ')'], Name).


%  dispatching input
%
%%   plot(+Dynamic, +LogScale, +Headings, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Dynamic plot
plot(dynamic, LogScale, Headings, Xaxis, Yaxis, Title, Data) -->
   html([
       \(dygraph_plot(LogScale, Headings, Xaxis, Yaxis, Title, Data))
        ]).

plot(scatter, LogScale, Headings, Xaxis, Yaxis, Title, Data) -->
   html([
       center(h2([Title, ' - ', Headings])),
       \(scatter_plot(LogScale,Xaxis,Yaxis,Data))
        ]).

%
%%   plot_log(+Log, +Headings, +Xaxis, +Yaxis, +Title, +Data)//
%
%    Log plot
plot_log(false, Headings, Xaxis, Yaxis, Title, Data) -->
   html([
       \(dygraph_plot(true, Headings, Xaxis, Yaxis, Title, Data))
        ]).

plot_log(true, Headings, Xaxis, Yaxis, Title, Data) -->
	{
    % check_list(Title,T),
    check_list(Xaxis,X),
    check_list(Yaxis,Y)
	 % format(atom(Header), '~w - ~w', [Title, Headings])
	},
   html([
       % center(h2([T, ' - ', Headings, ' = '])),
       center(h2(Title)),
       \(scatter_plot(true,X,Y,Data)),
       p(Headings)
        ]).

% Google Map

%%   pre_map_load//
%
%    Preloading javascript
pre_map_load -->
   html([
       script([type('text/javascript'),
	       src('https://maps.googleapis.com/maps/api/js?sensor=false')],
	      []),
       script([type('text/javascript'),
	       src('/html/js/handledata.js')],
	      [])
        ]).


%%   map_native(+Lat, +Lon, +Title)//
%
%    Display map
map_native(Lat, Lon, Title) -->
	% html_requires(css('map.css')),
	html_requires('/html/css/mapping.css'),
	html([
	    \pre_map_load,
	    div([id('map_canvas'), style('display:block')], []),
	    script([type('text/javascript')],
		   [
		    \js_call('drawMap'(Lat, Lon, Title))
		   ])
	     ]).

/*
%%   get_elevation(+Lat, +Lon)//
%
%    Inline elevation
get_elevation(Lat, Lon) -->
	html_requires('/html/css/mapping.css'),
	html([
	    \pre_map_load,
	    script([type('text/javascript')],
		   [
		    \js_new(elev, 'getElevation'(Lat, Lon)),
                    \js_call('document.write'(symbol(elev)))
		   ])
	     ]).
*/

% %%%%%%%%%%%%
% Plotly Contour/Heatmap
% %%%%%%%%%%%%

%%   pre_plotly_script_load//
%
%    Plotly.js script loader
pre_plotly_script_load -->
   html([
         script([type('text/javascript'),src('https://cdn.plot.ly/plotly-2.27.0.min.js')], [])
        ]).

%%   list_to_json(+List, -JsonString)
%
%    Convert a Prolog list to JSON array string
list_to_json(List, JsonString) :-
    list_items_to_json(List, Items),
    atomic_list_concat(['[', Items, ']'], JsonString).

list_items_to_json([], '').
list_items_to_json([X], Str) :- !,
    (is_list(X) -> list_to_json(X, Str) ; format(atom(Str), '~w', [X])).
list_items_to_json([X|Rest], Str) :-
    list_items_to_json(Rest, RestStr),
    (is_list(X) -> list_to_json(X, XStr) ; format(atom(XStr), '~w', [X])),
    atomic_list_concat([XStr, ',', RestStr], Str).

%%   plotly_contour(+X, +Y, +Z, +Title, +IsContour)//
%
%    Generate a contour or heatmap plot using Plotly.js
%    X: list of X coordinates
%    Y: list of Y coordinates  
%    Z: 2D list of Z values (list of lists, row-major order)
%    Title: plot title
%    IsContour: true for contour lines, false for heatmap
plotly_contour(X, Y, Z, Title, IsContour) -->
   {
    % Generate unique div id based on timestamp
    get_time(Time),
    Timestamp is floor(Time * 1000),
    format(atom(DivId), 'plotly_~w', [Timestamp]),
    
    % Convert lists to JSON format
    list_to_json(X, XJson),
    list_to_json(Y, YJson),
    list_to_json(Z, ZJson),
    
    % Determine plot type
    (IsContour = true -> PlotType = 'contour' ; PlotType = 'heatmap'),
    
    % Build colorscale for terrain colors (similar to R's terrain.colors)
    ColorScale = '[[0,"#00A600"],[0.25,"#E6E600"],[0.5,"#E9BD3A"],[0.75,"#ECB176"],[1,"#F2F2F2"]]',
    
    % Add contours option if IsContour is true
    (IsContour = true -> ContoursOption = ', contours: {coloring: "lines"}' ; ContoursOption = '')
   },
   html([
         \pre_plotly_script_load,
         div([id(DivId), style('width:600px;height:500px')], []),
         script([type('text/javascript')],
         [
          'var data = [{',
          '  type: "', PlotType, '",',
          '  x: ', XJson, ',',
          '  y: ', YJson, ',',
          '  z: ', ZJson, ',',
          '  colorscale: ', ColorScale,
          ContoursOption,
          '}];',
          'var layout = {',
          '  title: "', Title, '",',
          '  xaxis: {title: "X"},',
          '  yaxis: {title: "Y"}',
          '};',
          'Plotly.newPlot("', DivId, '", data, layout);'
         ])
        ]).
