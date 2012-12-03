:- module(context_autocorr, []).

/** <module> Topography interface
    * autocorrelation
    * transition probabilities
*/

:- use_module(context_math).
:- use_module(library(clpfd)).
:- use_module(library('R')).

:- context:register(context_autocorr:navigate).
:- context:register(context_autocorr:contour).
:- context:register(context_autocorr:random_walker).
:- context:register(context_autocorr:google_profile).
:- context:register(context_autocorr:data_service).
:- context:register(context_autocorr:variance).


% Marginal Probability Distribution for elevation distances
%
rdf_data(Name) :-
    rdfS(ID, ent:section_designator, _S),
    rdfS(ID, ent:name, Name).

get_all_sections(List) :-
    findall(option([value(Name)],[Name]), rdf_data(Name), RawList),
    sort(RawList, List).

navigate(Request) :-
   get_all_sections(List),
   reply_html_page(
     cliopatria(default),
     [title('Topographic data sets')],
     [\(con_text:table_with_iframe_target(
		    Request,
		     [h1('Available topographic sets'),
                      b(['Select 1', sup(small(o)), ' digital elevation model section of USA']),
                      \(con_text:two_columns(
                                     p([
                                         form([action('contour'), target(target_iframe)],
                                          [
                                           select([name('uri')], List),
                                           br([]),
					   /*
                                           \(con_text:radio_box_input_two('plot',
                                                                          ['color levels', color],
                                                                          ['contour lines', contour],
									  ['variance', variance]
									 )),
					   */
					   \(con_text:radio_toggles('plot',[ ['color levels', color],
                                                                          ['contour lines', contour],
									  ['variance', variance]])),
                                           %input([type('hidden'), name(render), value(render)]),
                                           input([type('submit'), name(opt), value('MaxEnt')]),
                                           input([type('submit'), name(opt), value('MixEnt')]),
                                           input([type('submit'), name(opt), value('O-U')]),
                                           input([type('submit'), name(opt), value('ME-RMS')])
                                           % input([type('submit'), name(opt), value('Regress')])
                                           % input([type('submit'), name(opt), value('mean')]),
                                          ]),
                                         p('Models avaliable correspond to varying degrees of disorder, MaxEnt has maximum variation in diffusional elevation changes, while O-U corersponds to a classical Ornstein-Uhlenbeck random walk with reversion to the mean.')
                                     ]),
                                     i([ br([]),
                                         'or click\n on map ',
                                         \(context_map:usa_map('contour?lat=~w&lon=~w',target_iframe))
                                        ]
                                       )
                                            )
                       ),
                      br([]),
		      \(con_text:render_iframe(render))
                     ]
                                         ))
     ]
		  ).

:- dynamic
    diffElev/4,
    diffModel/4.

list_min([[L|_]|Ls], Min) :- list_min(Ls, L, Min).

list_min([], Min, Min).
list_min([[L|_]|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).
/*
flat(Fraction, X, Y, Partial) :-
    D is 1.0/40.0/Fraction^2,
    F is D/Fraction*(1-exp(-2*X*Fraction))/2,
    Partial is exp(-Y/sqrt(F)). % /sqrt(F).

ou_model0(N,D,L,X,Y, Fraction, Result) :-
    P is 1-Fraction,
    F is D/L*(1-exp(-2*X*L))/2,
    Result is (P*N/sqrt(2*F)*exp(-Y*Y/2/F) + N/F*exp(-Y/F/Fraction)).
              % Fraction*N/sqrt(2*F/10000)*exp(-Y*Y*10000/2/F) ).

ou_maxent_model0(N,D,L,X,Y, Fraction, Result) :-
    P is 1-Fraction,
    F is sqrt(D/L*(1-exp(-2*X*L))/2),
    Result is (P*N/F*exp(-abs(Y)/F) + N/F*exp(-Y/F/Fraction)).
              % Fraction*N*100/F*exp(-100*abs(Y)/F)).

ou_gamma_model0(N,D,L,X,Y, Fraction, Result) :-
    P is 1-Fraction,
    % flat(Fraction, X, Y, Partial),
    F is sqrt(D/4/L*(1-exp(-2*X*L))/2),
    Result is (P*N/4*(2*F+Y)/F^2*exp(-Y/F)+ N/4/F*exp(-Y/F/Fraction)).
    % F1 is sqrt(D/4/Fraction*(1-exp(-2*X*Fraction))/2),
    % Result is P*N/4*(2*F+Y)/F^2*exp(-Y/F) + Fraction*N/4.
    % Result is (P*N/4*(2*F+Y)/F^2*exp(-Y/F) + Fraction*N/4*100/F*exp(-100*Y/F)).
*/

ou_model(N,D,L,X,Y, Fraction, Result) :-
    F is D/L*(1-exp(-2*X*L))/2,
    Result is Fraction*N/sqrt(pi*F)*exp(-Y*Y/4/F).

ou_maxent_model(N,D,L,X,Y, Fraction, Result) :-
    F is sqrt(D/L*(1-exp(-2*X*L))/2), % /8
    Result is Fraction*N/F*exp(-abs(Y)/F).

ou_mixent_model(N,D,L,X,Y, Fraction, Result) :-
    F is sqrt(D/L*(1-exp(-2*X*L))/2), % /8
    Result is Fraction*N/4*(F+Y)/F^2*exp(-abs(Y)/F).
/*
ou_mixent_model(N,D,L,X,Y, Fraction, Result) :-
    % P is 1-Fraction,
    % ou_maxent_model(N,D,L,X,Y, P, Result1),
    ou_mixent_model(N,D,L,X,Y, Fraction, Result2),
    Result1 is 0,
    Result is (Result1 + Result2)/1.0.
*/

instance_ou(Model,Name, N,D,L, Fraction, Q) :-
    diffElev(Name, X,Y,Z),
    X > 0,
    (	Model = ou ->
	ou_model(N,D,L,X,Y, Fraction, Result)
    ;
        Model = ou_mixent ->
	ou_mixent_model(N,D,L,X,Y, Fraction, Result)
    ;
        ou_maxent_model(N,D,L,X,Y, Fraction, Result)
    ),
   % Q is log((Z+0.01)/(Result+0.01)).  % The delta removes singularities /sqrt(X)

(   Z > 0 ->
           Q is ((Z-Result)/Z)
    ;
           Q = 0
    ).


    % Q is Z/N/log(Result+0.00001).  % The delta removes singularities /sqrt(X)


%   Q is Result*log(Result/(Z+1)).  % The delta removes singularities
%   Q is (log((Z+1)/(Result+1))) /sqrt(X^2+Y^2). % The delta removes  singularities
%    Q is abs(log(Z+0.0001) - log(Result+0.0001)).
% Q is sqrt(abs(Z^2 - Result^2)/(Result+0.000001)^2).
%    Q is log((Z+0.01)/(Result+0.01)).
%    Q is abs(Z - Result)/(Result+0.001).


model_fit(Model, Name, N0,D0,L0, F0, D, L, F, Quality) :-
    member(D, D0),
    member(L, L0),
    member(F, F0),
    findall(Q,instance_ou(Model, Name, N0,D,L, F,Q), Results),
    R mapdot abs ~> Results,
    sumlist(R, Quality).


%    Quality is QQ.

/*
find_best_model(Name, Quality, Df, Lf, N) :-
    N is 1201*1201,
    Df = 32.0,
    Lf = 0.06,
    model_fit(Name, N,[Df],[Lf],Lf,Df,Quality),
    !.
*/


/*

find_model(_Model, Name, 1, 0.01, 0.1, N_Max, 0.1) :-
    diffElev(Name, 40, 0,Z),
    N_Max is 1201*1201,
    Z > 0.81 * N_Max, !.  % 90 percent is flat in the x direction
*/

find_model(ou_maxent_rms, Name, Quality, Df, Lf, Nf, Ff, Flatness) :-
    context_ou:optimize(Name, Diffusion, Drag, Err),
    print(user_error, ['residual error', Err]),
    Nf is 1201*1201,
    diffElev(Name, 40, 0,Z),
    Flatness is Z/Nf,
    F0 range [0.0,1.0]/0.1,
    D0 = [Diffusion],
    L0 = [Drag],
    findall([Q,L,D,F], model_fit(ou_maxent, Name, Nf,D0,L0,F0,D,L,F,Q), Results),
    list_min(Results, Min),
    member([Min,Lf,Df,Ff],Results),
    Quality is integer(Min),
    !.

find_model(Model, Name, Quality, Df, Lf, Nf, Ff, Flatness) :-
    Nf is 1201*1201,
    diffElev(Name, 40, 0,Z),
    Flatness is Z/Nf,
    F0 range [0.0,1.0]/0.1,
    % N_Min = N_Max,
    Scale is sqrt(16), %16
    % D0 range [0.1,8192]^Scale,
    D0 range [0.1,4096]^Scale,
    % L0 = [0.0000001],
    % L0 range [0.000001,0.5]^Scale,
    L0 range [0.00001,1.0]^10,   %0.00001
    % N0 range [N_Min,N_Max]/1,
    findall([Q,L,D,F], model_fit(Model, Name, Nf,D0,L0,F0,D,L,F,Q), Results),
    list_min(Results, Min),
    member([Min,Lf,Df,Ff],Results),
    Quality is integer(Min).

find_best_model(Model, Name, Quality, Df, Lf, Nf, F) :-
    Nf is 1201*1201,
    % diffElev(Name, 40, 0,Z),
    % Fraction is Z/Nf,
    F0 = [F],
    % N_Min = N_Max,
    Scale is sqrt(2),
    % D0 range [0.5,4096]^Scale,
    D0 = [Df],
    L0 range [0.000001,0.5]^Scale,
    % N0 range [N_Min,N_Max]/1,
    findall([Q,L,D,F], model_fit(Model, Name, Nf,D0,L0,F0,D,L,F,Q), Results),
    list_min(Results, Min),
    member([Min,Lf,Df,F],Results),
    Quality is integer(Min).

generate_model(_Model, _,   _,  _,  [],  [], _Fraction, F, R) :- reverse(F,R).
generate_model(Model, N, Df, Lf, [X|DX], [Y|DY], Fraction, In, Out) :-
    (   X > 0 ->
        (   Model = ou ->
        ou_model(N,Df,Lf,X,Y, Fraction, R)
        ;  Model = ou_mixent ->
        ou_mixent_model(N,Df,Lf,X,Y, Fraction, R)
        ;
        ou_maxent_model(N,Df,Lf,X,Y, Fraction, R)
        ),
        Result is log10(R+0.01)
    ;
        Result = 0
    ), !,
    generate_model(Model, N, Df, Lf, DX, DY, Fraction, [Result|In], Out).

generate_model_stats(_Model, _URI, _,   _,  _,  [],  [], _Fraction, F, R) :-
    R is sqrt(F/40/20)*100.
generate_model_stats(Model, URI, N, Df, Lf, [X|DX], [Y|DY], Fraction, Input, Out) :-
    (   X > 0 ->
        diffElev(URI, X, Y, Z),
        (   Model = ou ->
        ou_model(N,Df,Lf,X,Y, Fraction, Result)
        ;  Model = ou_mixent ->
        ou_mixent_model(N,Df,Lf,X,Y, Fraction, Result)
	;
        ou_maxent_model(N,Df,Lf,X,Y, Fraction, Result)
	),
        (   Z > 0 ->
            Diff is (Z-Result)^2/Z^2
        ;
            Diff = 0
        ),
        Sum is Input + Diff
    ;
        Sum is Input
    ), !,
    generate_model_stats(Model, URI, N, Df, Lf, DX, DY, Fraction, Sum, Out).


random_walker(Request) :-
   http_parameters(Request, [d(D, [float]),
                             l(L, [float]),
                             t(N, [string])]), %%% This is a character list-- beware!
   X range [1,4000]/1,
   D0 is D,  %% should be 65 because E-W post data is shorter
   L0 is L,
   Scale is 1.0/90.0,
   context_random_walk:ou_random_walker(D0, Scale, L0, X, Z),
   % Window
   Data tuple X + Z,
   reply_html_page( [title(rw)],
                    [
                     \(context_graphing:dygraph_native(
                                            false,
                                            ['X','Z'],
                                            'distance (m)',
                                            'elevation change (m)',
                                            N,
                                            Data )
                      )
                    ]
                  ).

google_profile(Request) :-
  http_parameters(Request, [z(Zone, [float]),
                            e(East, [float]),
                            n(North, [float]),
                            t(Name, [atom])]),
  L = 4000, % path length
  N = 200, % number of points
  Nt is North + 500000-random(1000000),
  Et is East + 500000-random(1000000),
  E1 is Et - L/2,
  E2 is Et + L/2,
  % Google limits to 500 points at a time
  % Step is L/N,
  context_geo:get_elevation_path([[E1,E2],[Nt,Nt]], Zone, false, N, Elev),
  length(Elev, Length),
  Delta is L/Length,
  X range [Delta,L]/Delta,
  length(X, Xlength),
  print(user_error, [Length, Xlength]),
  Data tuple X + Elev,
  reply_html_page( [title(google)],
                    [
                     \(context_graphing:dygraph_native(
                                            false,
                                            ['X','Z'],
                                            'profile distance (m)',
                                            'Google elevation change (m)',
                                            Name,
                                            Data)
                      )
                    ]
                  ).

load_dem(URI) :-
    atomic_list_concat(['instances/geo/',URI,'.ac.pl'], Path),
    retractall(diffElev(URI,_,_,_)),
    exists_file(Path),!,
    read_file_to_terms(Path, Terms, []),
    maplist(assert, Terms).
load_dem(URI) :-
    print(user_error, [URI, 'not found']),
    fail.

exp_regression(URI, X, Lbar) :-
   findall(Result, (diffElev(URI, X, Y, Result),Y>0,Result>0), YR),
   Y range [1,20]/1,
   YL mapdot log ~> YR,
   yy <- YL,
   xx <- Y,
   fitxy <- lm('yy~xx'),
   Slope <- 'as.double(fitxy$coefficients[2])',
   Lbar is 1/Slope.


find_theta(URI, X1, X2, Theta, D) :-
   exp_regression(URI, X1, L1),
   exp_regression(URI, X2, L2),
   Theta is abs(2*(L1*sqrt(X2)-L2*sqrt(X1))/(X2*sqrt(X2)*L1-X1*sqrt(X1)*L2)),
   D is L1^2/X1/(1-Theta*X1/4)^2.

find_avg(URI, Theta,D) :-
   X1 in 1..5,
   label([X1]),
   X2 is X1 + 35,
   find_theta(URI, X1, X2, Theta,D).

pair_summer([], Total, Total).
pair_summer([[F1,F2]|R], [Total1,Total2], Final) :-
    Sum1 is Total1 + F1,
    Sum2 is Total2 + F2,
    pair_summer(R, [Sum1,Sum2], Final).

pair_sum(List, Sum) :-
    pair_summer(List, [0,0], Sum).

calc_avg(URI, Theta, Diffusion) :-
   r_open([]),
   findall([T,D], find_avg(URI,T,D), Pairs),
   r_close,
   % print(user_error, Pairs),
   length(Pairs,L),
   pair_sum(Pairs,[TT,DT]),
   Theta is TT/L,
   Diffusion is DT/L.

negate(true,color).
negate(false,contour).
maintain(true,contour).
maintain(false,color).

model_options([], _URI, _Opt, _Contour) --> !.
model_options([F|R], URI, Opt, Contour) -->
   {
    F = Opt
   },
   html(i(F)),
   model_options(R, URI, Opt, Contour).
model_options([F|R], URI, Opt, Contour)  -->
   {
    F \= Opt,
    maintain(Contour,Type)
   },
   html(
   \(con_text:inline_button(
		  \(con_text:button_link(  'Model ' + F,
					   'contour',
					   target_iframe,
					   [[uri, URI],
					    [plot, Type],
					    [opt, F]
					   ]))
                                            )
                     )
       ),
   model_options(R, URI, Opt, Contour).

rcontour(Image, X,Y,Z, Contour) -->
     {
     Vector range [3.0,6.0]/0.1,
     r_open_session,
     y <- Y,
     x <- X,
     z <- Z,
     at <- Vector,
     r_in( library(lattice) ),
     dquote(Image, FN),
     r_in( bmp(filename=FN)),
     (   Contour = true,
         r_in( 'contourplot(z~x*y, cuts=10)' )
     ;
         r_in( 'levelplot(z~x*y, col.regions=terrain.colors, at=at)' )
     ),
     r_print( 'dev.off()' ),
     r_close
     },
     html(img(src(Image))).


contour_display(Opt, URI, Contour, Lat, Lon, Info) :-
    load_dem(URI),
    findall([X,Y,Z],
	    (	diffElev(URI,X,Y,Z),
	        Z > 0
	    ),
	    L),
    [[_|DX],[_|DY],[_|DZ]] split L,
    ZZ mapdot log10 ~> DZ,

    (	Opt = 'MaxEnt' ->
      Model = ou_maxent,
      find_model(Model, URI, MinVal, Df, Lf, N, Fraction, Flatness)
      %  MinVal1=MinVal
      % find_best_model(Model, URI, MinVal1, Df, Lf, N, Fraction)
    ;
        Opt = 'ME-RMS' ->
      Model = ou_maxent_rms,
      find_model(Model, URI, MinVal, Df, Lf, N, Fraction, Flatness)
    ;
        Opt = 'Regress' ->
      Model = ou_maxent,
      N is 1201*1201,
      calc_avg(URI, Lf, Df),
      % Fraction=0.01,
      model_fit(Model, URI, N,[Df],[Lf], [0.25], _, _, Fraction, MinVal)
      % MinVal1 = MinVal
    ;
/*        Opt = 'mean' ->
      Model = ou_maxent,
      N1 is 1201*1201,
      calc_avg(URI, Lf1, Df1),

      find_model(Model, URI, _MinVal, Df2, _Lf2, N, Fraction),
      find_best_model(Model, URI, MinVal, Df2, Lf2, N, Fraction),
      Df is sqrt(Df1*Df2),
      Lf is sqrt(Lf1*Lf2),
      model_fit(Model, URI, [N1],[Df],[Lf], _, _, _, MinVal1)
    ; */
        Opt = 'MixEnt' ->
      Model = ou_mixent,
      find_model(Model, URI, MinVal, Df, Lf, N, Fraction, Flatness)
      %  MinVal1=MinVal
      % print(user_error, [fraction, Fraction])
      % find_model(Model, URI, MinVal, Df, _Lf4, N, Fraction, Flatness),
      % find_best_model(Model, URI, MinVal1, Df, Lf, N, Fraction)
    ;
        Opt = 'O-U' ->
      Model = ou,
      find_model(Model, URI, MinVal, Df, Lf, N, Fraction, Flatness)
      % find_best_model(Model, URI, MinVal1, Df, Lf, N, Fraction)

    ),
    generate_model(Model, N, Df, Lf, DX, DY, Fraction, [], ZM),
    Err0 is (exp(MinVal/20/40)-1)*100,
    generate_model_stats(Model, URI, N, Df, Lf, DX, DY, Fraction, 0.0, Err1),
    % Err1 is (exp(MinVal1/20/40)-1)*100,

    context_geo:zone_from_section_designator(Info, Zone),
    context_geo:ll_to_utm(Lat, Lon, Zone, Easting, Northing),
    % Var is sqrt(Df/2/Lf),
    Sigma1 is sqrt(Df*40/2),
    Sigma2 is sqrt(Df/2/Lf),
    Sigma is min(Sigma1, Sigma2),
    Order is 100*Fraction,
    Flat is 100*Flatness,
    negate(Contour, Toggle),

    reply_html_page([title(contour),
                     \(con_text:style)],
		    [
		     % h3([i(URI),  Stats  ]),
                     table([border=0],
                           [tr([th(data),th('marginal PDF model')]),
                            tr([td(
                                    \(rcontour('/contour.bmp',DX,DY,ZZ,Contour))
                                  ),
                                td(
                                    \(rcontour('/level.bmp',DX,DY,ZM, Contour))
                                  )])]
                          ),
		    \(con_text:table_multiple_entries(
             [[i('section'),i('PDF model'), i('Diffusion'), i([&(theta), '=Drag']),  i(&(sigma)),
	       i('Fitness'),  i('Lat/Lon'), i('UTM - Zone,E,N'), i('model error'), i('flat')]],
             [[b(URI), Model, '~2f'-Df, '~5f'-Lf, '~2f'-Sigma, '~0f%'-Order, '~1f/~1f'-[Lat,Lon],
               '~w, ~0f, ~0f'-[Zone,Easting,Northing], '~0f% .. ~0f%'-[Err0,Err1], '~1f%'-Flat ]]
						     )
		     ),
                     br([]),
                     \(con_text:inline_button(
                                   \(con_text:button_link('Display Map',
					   '/context_map/view',
					   render,
					   [[lat, Lat],
					    [lon, Lon],
					    [title, URI]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Generate Terrain Profile',
					   'random_walker',
					   render,
					   [[l, Lf],
					    [d, Df],
					    [t, URI]
					   ]))
                                            )
                     ),

                    \(con_text:inline_button(
                                   \(con_text:button_link('Get Data Set',
					   'data_service',
					   render,
					   [[lat, Lat],
					    [lon, Lon]
					   ]))
                                            )
                     ),

                    \(con_text:inline_button(
                                   \(con_text:button_link('Toggle Plot Style',
					   'contour',
					   target_iframe,
					   [[uri, URI],
					    [plot, Toggle],
					    [opt, Opt]
					   ]))
                                            )
                     ),
                    \(model_options(['MaxEnt', 'MixEnt', 'O-U', 'ME-RMS'],
				    URI, Opt, Contour)),

                    \(con_text:inline_button(
                                   \(con_text:button_link('plot RMS error',
					   'variance',
					   render,
					   [[df, Df],
					    [lf, Lf],
                                            [uri, URI]
					   ]))
                                            )
                     ),
                    \(con_text:inline_button(
                                   \(con_text:button_link('Sampled Google Profile',
					   'google_profile',
					   render,
					   [[z, Zone],
					    [e, Easting],
					    [n, Northing],
                                            [t, URI]
					   ]))
                                            )
                     )

		    ]
	      ).


variance(Request) :-
    http_parameters(Request, [uri(URI, [string]),
                              df(Df, [float]),
                              lf(Lf, [float])]),
    context_ou:rms_data(URI, Vars),
    context_ou:xrange(X0,X1),
    % X1_scale is X1*100,
    X range [X0,X1]/1,
    OU mapdot maxent_ou_variance(Df,Lf) ~> X,
    Profile tuple X + Vars + OU,
    reply_html_page(
	    [title('multi-variance')],
	    [
	     \(context_graphing:dygraph_native(
				       lin,
				       ['Post', 'Data RMS', 'MaxEnt RMS'],
				       'x (post number)', 'RMS (m)',
				       ['sampling deviation of ', URI],
				       Profile)),
	     br([]),
	     p('The RMS is truncated to elevation changes < 20 meters, due to limited sampling')
	    ]
			   ).


contour(Request) :-
    http_parameters(Request, [uri(URI, [string, optional(true)]),
                              plot(Contour, [string, optional(true)]),
			      opt(Opt, [string, default('MaxEnt')]),
                              lat(Lat, [float, optional(true)]),
                              lon(Lon, [float, optional(true)])]),
    (   var(URI),
        (
        context_geo:find_dem_section(Lat, Lon, URI) ->
           context_geo:find_dem_section(_, _, URI, Info),
           print(user_error, [URI]),
           Contour = false, % assumes unbound
           contour_display(Opt, URI, Contour, Lat, Lon, Info)
        ;
            reply_html_page([title('DEM data sets')], [p('section likely not found')])
        )
    ;
        (   Contour = variance ->
            context_geo:find_dem_section(Lat, Lon, URI, Info),
	    context_ou:rms_data(URI, Vars),
	    context_ou:xrange(X0,X1),
	    X range [X0,X1]/1,
            Profile tuple X + Vars,
	    reply_html_page(
		[title('multi-variance')],
		[
		\(context_graphing:dygraph_native(
				       log,
				       ['Post', 'Variance'],
				       'x (post)', 'var (m^2)',
				       ['Variance ', URI],
				       Profile))
		 ]
			   )

	;
	    Contour = contour ->
            context_geo:find_dem_section(Lat, Lon, URI, Info),
            contour_display(Opt, URI, true, Lat, Lon, Info)
	;
	    Contour = color ->
            context_geo:find_dem_section(Lat, Lon, URI, Info),
            contour_display(Opt, URI, false, Lat, Lon, Info)
	)
    ).


data_row(URI,ZList) :-
    Y in 0..40,
    label([Y]),
    findall(td(Z), diffElev(URI,_X,Y,Z), ZList).

data_table(URI, table(ZTable)) :-
    findall(tr(Row), data_row(URI,Row), ZTable).

data_service(Request) :-
    http_parameters(Request, [lat(Lat, [float]),
                              lon(Lon, [float])]),
    context_geo:find_dem_section(Lat, Lon, URI),
    context_geo:find_dem_section(_, _, URI, Info),
    print(user_error, [URI]),
    load_dem(URI),
    data_table(URI, Table),
    reply_html_page([title(Info)],
                    [Table]).


% FIX FIX
%
collect_info(Lat, Lon, ['U'=URI, 'D'=Df, 'theta'=Lf, 'Q'=Q, 'E'=Err,
                        'F'=Fraction, 'S'=Flatness]) :-
    context_geo:find_dem_section(Lat, Lon, URI),
    load_dem(URI),
    % find_model(ou_maxent, URI, _Q, Df, _Lf, N, Fraction),
    find_model(ou_mixent, URI, Quality, Df, Lf, N, Fraction, Flatness),
    findall([X,Y,Z],
    (	diffElev(URI,X,Y,Z),
        Z > 0
    ),
    L),
    [[_|DX],[_|DY],[_|_DZ]] split L,
    generate_model_stats(ou_mixent, URI, N, Df, Lf, DX, DY, Fraction, 0.0, Err),
    Q is (exp(Quality/20/40)-1)*100, !.

%    find_best_model(ou_maxent, URI, Quality, Df, Lf, N, Fraction).

collect_info(URI) :-
    context_geo:find_dem_section(Lat, Lon, URI, _Info),
    load_dem(URI),
    % find_model(ou_maxent, URI, _Q, Df, _Lf, N, Fraction),
    find_model(ou_mixent, URI, Quality, Df, Lf, N, Fraction, Flatness),
    findall([X,Y,Z],
    (	diffElev(URI,X,Y,Z),
        Z > 0
    ),
    L),
    [[_|DX],[_|DY],[_|_DZ]] split L,
    generate_model_stats(ou_mixent, URI, N, Df, Lf, DX, DY, Fraction, 0.0, Err),
    Q is (exp(Quality/20/40)-1)*100,
    writeln([URI, Lat, Lon, Df, Lf, Fraction, Flatness,  Q, Err]),
    !.



/*
collect_info(URI, ['D'=Df, 'theta'=Lf, 'Q'=Quality]) :-
    rdf_data(URI),
    load_dem(URI),
    find_model(ou_mixent, URI, Quality, Df, Lf, _N, _Fraction, _Flatness), !.
%    find_best_model(ou_maxent, URI, Quality, Df, Lf, N, Fraction).
*/

run_sections :-
   Lat in 33..41,
   Lon in -119 .. -80,
   label([Lat,Lon]),
   collect_info(Lat, Lon,
                ['U'=Name,
                 'D'=Df, 'theta'=Lf, 'Q'=Q, 'E'=Err, 'F'=Fraction, 'S'=Flatness]),
   writeln([Name, Lat, Lon, Df, Lf, Fraction, Flatness,  Q, Err]).

run_all_sections :-
   findall(Name, rdf_data(Name), Raw),
   maplist(collect_info, Raw).


aggregate_info(URI) :-
    atomic_list_concat(['instances/geo/',URI,'.ac.pl'], Path),
    read_file_to_terms(Path, Terms, []),
    maplist(assert, Terms).

aggregate_all_sections :-
   retractall(diffElev(_,_,_,_)),
   findall(Name, rdf_data(Name), Raw),
   maplist(aggregate_info, Raw).

find_all_elevs(X,Z, Total) :-
   X in 0 .. 40,
   Z in 0 .. 20 ,
   label([X,Z]),
   findall(Count, diffElev(_, X,Z,Count), Counts),
   length(Counts,L),
   sumlist(Counts,Num),
   Total is Num/L.


sum_all_sections :-
   findall(diffElev('aaa_16_16_test',X,Z,Count), find_all_elevs(X,Z,Count), List),
   tell('instances/geo/aaa_16_16_test.ac.pl'),
   maplist(writeln, List),
   told.


/*
ou_maxent_model(N,D,L,X,Y, Result) :-
    F is sqrt(D/L*(1-exp(-2*X*L))/2),
    Result is 1*N/F*exp(-abs(Y)/F).
*/

/*
ou_maxent_model(N,D,L,X,Y, Result) :-
    F is sqrt(D/L*(1-exp(-2*X*L))/2),
    Result is 1*N/F/(1+(abs(Y)/F))^2.
*/

/*
ou_maxent_model(N,D,L,X,Y, Result) :-
    F is D/L*(1-exp(-2*X*L))/2,
    Result is 1.9*N/sqrt(2*pi*F)*exp(-Y*Y/2/F) +
              0.1*N/sqrt(2*pi*F/10)*exp(-Y*Y*10/2/F).
*/


% Result is 2*N/sqrt(2*pi*F)*exp(-(Y)*(Y)/2/F).
% Result is (0.99*N/F*exp(-abs(Y)/F)+0.01*N*100/F*exp(-100*abs(Y)/F)).
