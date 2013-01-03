:- module(context_pdf, [
                       % sampling_power_law/3
                       ]).

/** <module> Generic PDF interface, via locale and property
The + are user inputs and - values are retrieved from RDF as lookup
data
    * property +
    * locale +
    * PDF mean value -
    * distribution type -
    * min extent -
    * max extent -
    * plotting interval size -
    * x-axis units -
    * CDF version - (not implemented yet, only PDF)
    * x-axis label -
    * title -
    * log-log vs linear  -
    * quantity for equivalent Monte Carlo generator
    * -- sample return single value
    * -- sample return list of values
*/

:- use_module(context_math).
:- use_module(context).
:- use_module(library(http/http_host)).

:- context:register(context_pdf:process).
:- context:register(context_pdf:loglin).
:- context:register(context_pdf:generate_service).

%%   service_req(-Web_URL)
%
%    Get web service URL
service_req('/context_pdf/process?locale=conus&property=terrain_slopes&operation=sample&quantity=1').

%%   plot_table(+Title, +X, +Mean, +Min, +Max, +Delta, +Log)//
%
%    Plot PDF table
plot_table(Title, X, Mean, Min, Max, Delta, Log) -->
    html(
        table([class('block'),border(1)],
              [
               tr([th('Name'), th('Value')]),
               tr([td('Title'),td(Title)]),
               tr([td('X'),td(X)]),
               tr([td('Mean'),td(Mean)]),
               tr([td('From'),td(Min)]),
               tr([td('To'),td(Max)]),
               tr([td('Interval'),td(Delta)]),
               tr([td('Log'),td(Log)])
              ])
        ).

%%   render_pdf_artifacts(+Title, +X, +Mean, +Min, +Max, +Delta, +Log)
%
%    Display PDF artifacts
render_pdf_artifacts(Title, X, Mean, Min, Max, Delta, Log) :-
    check_log(Log, _LogNum, LogSym),
    service_req(Service),
    reply_html_page(cliopatria(default),
                   [title('PDF')
                    % \(context_graphing:scatter_script_load)
                   ],
                   [
                    \(con_text:table_with_iframe_target(
                                   target_iframe,
                                   [
                                    \(context_pdf:plot_table(Title, X, Mean, Min, Max, Delta, LogSym)),
                                    \(con_text:radio_box_two(
                                                   'Graph scale',
                                                   'loglin',
                                                   'target_iframe',
                                                   'loglin',
                                                   ['log', true],
                                                   ['linear', false])),
                                    br([]),
                                    \(con_text:button_link(
                                                   'Generate Service Request',
                                                   'generate_service',
                                                   'render',
                                                   [service, Service])),
                                    \(con_text:button_link(
                                                   'Generate Semantic Graph',
                                                   '/context/display_directed_graph',
                                                   render)),
                                    br([]),
                                    \(con_text:render_iframe(render))
                                   ]
                                                       )
                     )
                   ]).


%%   plot_pdf(+Model, +Mean, +Min, +Max, +Delta, +X, +Title, +Log)
%
%    Plot PDF
plot_pdf(besselk1, Mean, Min, Max, Delta, X, Title, Log) :-
    render_pdf_artifacts(Title, X, Mean, Min, Max, Delta, Log).

/*
sampling_power_law(Median,_,V) :-
   random(R),
   V is Median/(1.0/R-1.0).
*/

%%   sample_from_pdf(+PDF_Model, +Mean, +N, +X, +Title, +Log)
%
%    Create sample from PDF  and place in table
sample_from_pdf(besselk1, Mean, 1, X, Title, _) :-
    % Sampling mapdot power_law_2_sampling(Mean) ~> [1],
    besselk0_sqrt(Mean,sample, _, Sampling),
    reply_html_page([title('Power Law Sampling'),
		     \(con_text:style)],
                   [
                    table([tr([th(Title)]),
                           tr([td(Sampling),td(X)])
                          ]
                         )
                   ]).

sample_from_pdf(besselk1, Mean, Quantity, X_Axis, Title, _Log) :-
    X_array range [1,Quantity]/1,
    % Sampling mapdot power_law_2_sampling(Mean) ~> X_array,
    Sampling mapdot besselk0_sqrt(Mean,sample) ~> X_array,
    reply_html_page(% cliopatria(default),
                   [title('Power Law Sampling'),
		    \(con_text:style)],
                   [
                    \(context_graphing:dygraph_plot(
                                           true,
                                           'sampleNumber, variateValue',
                                           'sample Number',
                                           X_Axis, % wrong order? for X&Y
                                           Title,
                                           [X_array,Sampling] ))
                   ]).


%%   plot_directed_graph
%
%    Plot directed graph from PDF class
plot_directed_graph :-
    rdfx_collect(List),
    asserta(stored_rdf_graph_list(List)),
    reply_html_page(cliopatria(default), title('Directed Graph Plot'),
                    [
                     \graphviz_graph(stored_rdf_graph_list,
                                     [graph_attributes([rankdir('LR')])])
                    ]),
    rdfx_clean.


:- dynamic(stored_rdf_graph_list/1).


%%   process(+Request)
%
%    Example dynamic page for generating a PDF, from *example*
process(Property_Nickname, Locale_Nickname, Operation*Quantity) :-
    rdfx(Locale, ent:nickname, Locale_Nickname),
    rdfx(Property, ent:nickname, Property_Nickname),
    rdfx(Locale, ent:property, Property),
    rdfx(Property, ent:characterized_by, ent:pdf_distribution),
    rdfx(Feature_Distribution, rdf:type, ent:pdf_distribution),
    rdfx(Distro, rdf:type, Feature_Distribution),
    rdfx(Locale, ent:characterization, Distro),
    rdfx(Distro, ent:distribution_type, Type),
    rdfx(Distro, ent:mean, Mean),
    rdfx(Distro, ent:min_extent, Min),
    rdfx(Distro, ent:max_extent, Max),
    rdfx(Distro, ent:interval_size, Delta),
    rdfx(Distro, ent:x_axis, X),
    rdfx(Distro, ent:title, Title),
    rdfx(Distro, ent:logarithmic, Log),
    atom_concat(D, '/decade', Delta),
    print(user_error, ['decade',D]),!,
    r_open_session,

    (    % A select alternative mode
    Operation = graph ->   plot_pdf(Type, Mean, Min, Max, 0.1, X, Title, Log);
    % Need to fix Delta

    Operation = sample ->  sample_from_pdf(Type, Mean, Quantity, X, Title, Log);

    Operation = codegen -> reply_html_page([title('Context')],[p('under construction')]);

    Operation = map ->     plot_directed_graph
    ),
    r_close.


process(Request) :-
    http_parameters(Request, [locale(Locale, [string]),
                              property(Property, [string]),
                              operation(Operation, [string]),
                              quantity(Quantity, [integer])]),
    process(Property, Locale, Operation*Quantity).


%%   loglin(+Request)
%
%    Switch from log to lin
loglin(Request) :-
    http_parameters(Request, [loglin(Loglin, [boolean,
					      default(true)])]),
    retractall(log_scale(_)),
    asserta(log_scale(Loglin)),
    check_log(Loglin, OutputLogical, _OutputNumeric),
    Mean = 0.039,
    Delta = 0.5,
    Min = 0.01,
    Max = 10.0,
    Power is 1.0/(1.0-Delta),
    Range range [Min,Max]^Power,
    X = 'rise/run',
    % PDF mapdot power_law_2_pdf(Mean) ~> Range,
    r_open_session,
    PDF mapdot besselk0_sqrt(Mean,pdf) ~> Range,
    r_close,
    reply_html_page([title('slopes'),
                     \(con_text:style)],
                    [
        h2('slope distribution over continental USA'),br([]),
        \(context_graphing:scatter_plot( OutputLogical,
                                         X,
                                         'Probability',
                                         [Range, PDF])
         )
                    ]).


%%   encode_service(Hostname, Port, Cmd, Service)
%
%    Encode service call
encode_service(Hostname, Port, Cmd, Service) :-
    atomic_list_concat(['http://', Hostname, ':', Port, Cmd], Service).

%%   generate_service(+Request)
%
%    Generate service call
generate_service(Request) :-
    http_parameters(Request, [service(Service, [string])]),
    http_current_host(Request, Hostname, Port, [global(true)]),
    % print(user_error, [Hostname, Port]),
    encode_service(Hostname, Port, Service, Serv),
    reply_html_page([],
                    [
        p('Service request to sample:'),
        p(b(Serv))
                    ]
                   ).

%  http://localhost:3020/context_pdf/process?locale=conus&property=terrain_slopes&operation=sample&quantity=1


%%   example//
%
%    Example inline dynamic page for generating a PDF
example -->
       html(
	   \(con_text:table_form_target('What artifacts are available? >> ',
					'/context_pdf/process',
					'_blank',
                              [
			       ['locale', 'conus'],
                               ['property', 'terrain_slopes'],
                               ['operation', 'graph'],
                               ['quantity', '1']
			      ]
				      )
		  )
	   ).

% This is a bit of a kludge to allow the view of linear and log scales
% It should really be a toggle button and then a refresh.

:- dynamic(log_scale/1).

%%   log_or_linear(+Log, -Set)
%
%    Check if log or linear
log_or_linear(_, 1) :- log_scale(true).
log_or_linear(_, 0) :- log_scale(false).
log_or_linear(true, 1).
log_or_linear(false, 0).

% toggle(1, false).
% toggle(0, true).
%%   log_value(+Num, -Bool)
%
%    Log set
log_value(1, true).
log_value(0, false).

%%   check_log(+Input, -OutputLogical, -OutputNumeric)
%
%    If logarithmic
check_log(Input, OutputLogical, OutputNumeric) :-
    log_or_linear(Input, OutputNumeric),
    log_value(OutputNumeric, OutputLogical).
%    toggle(OutputNumeric, NextLogValue),
%    retractall(log_scale(_)),
%    asserta(log_scale(NextLogValue)).

% end kludge
