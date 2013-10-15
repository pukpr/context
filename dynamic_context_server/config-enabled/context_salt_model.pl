:- module(context_salt_model, [
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_salt_model:navigate).
:- context:register(context_salt_model:plot).

navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('SALT model response')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('SALT Model'),
                      p('Explanation'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('t_units')], Tunits),
			  input([type('text'),
				 name('limit'),
				 value('3')]), i(' <= margin'),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [
                                          ['CO2 signal', signal],
					  ['residual', residual],
					  ['components', all],
					  ['model', model]
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('graph')]),
			  input([type('submit'), name(kind), value('table')])
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
                     ]
						       ))
     ]
		  ).


get_fit(Temperature, CO2, SOI, TSI, Volc, LOD,
                     C,   S,   T,   V,    L, Int, R2) :-
   r_open_session,
   y <- Temperature,
   c <- CO2,
   s <- SOI,
   t <- TSI,
   v <- Volc,
   l <- LOD,
   fitxy <- lm('y~c+s+t+v+l'),
   r_print(fitxy),
   Int <- 'as.double(fitxy$coefficients[1])',
   C <- 'as.double(fitxy$coefficients[2])',
   S <- 'as.double(fitxy$coefficients[3])',
   T <- 'as.double(fitxy$coefficients[4])',
   V <- 'as.double(fitxy$coefficients[5])',
   L <- 'as.double(fitxy$coefficients[6])',
   summary <- summary(fitxy),
   r_print(summary),
   R2 <- 'as.double(summary$r.squared)',
   r_close.

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [])]),

    XLabel = 'Time',
    YLabel = 'Temperature',
    XUnits = 'year',
    YUnits = 'C',

    context_box_model:tsi(TSI),
    context_box_model:temperature(T),
    context_box_model:soi(SOI),
    context_box_model:volcanos(V),
    context_box_model:lod(LOD),
    context_co2:co2_knmi(CO2),

    length(T, L),
    H range [1, L]/1,
    Y mapdot H ./ 12,
    Year mapdot 1880 .+ Y,
    Zeros mapdot 0 .* H,

    S0 unbias SOI,
    expsm(S0, 0.84, S2),    % smooth SOI

    sparse_list(Zeros, V, Vol),
    expsm(Vol, 0.97, V1),

    interpolate(Year, TSI, TSI_I),
    TSI_F unbias TSI_I,        % expsm(TSI_U, 0.99, TSI_L),

    interpolate(Year, LOD, LOD_I),
    LOD_U unbias LOD_I,
    expsm(LOD_U, 0.985, LOD_F),

    interpolate(Year, CO2, CO2_I),
    LogCO2 mapdot ln ~> CO2_I,

    get_fit(T, LogCO2, S2, TSI_F, V1, LOD_F,
	    C,     SO, TS, VC,    LO, Int, _R2C),

    T_CO2_R mapdot C .* LogCO2 + SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F,
    T_R mapdot Int .+ T_CO2_R,
    T_Diff mapdot T - T_R,
    corrcoeff(T, T_R, R2C2),
    (
       Characteristic = model ->
	 Data tuple Year + T + T_R,
         Header = [XLabel, temperature, model]
     ;
       Characteristic = residual ->
	 Data tuple Year + T_Diff,
         Header = [XLabel, residual]
     ;
       Characteristic = signal ->
         CO2_Signal mapdot Int .+ C .* LogCO2,
         Signal mapdot CO2_Signal + T_Diff,
         Data tuple Year + Signal + CO2_Signal,
         Header = [XLabel, signal, co2]
     ;
       Characteristic = all ->
         S0S2 mapdot SO .* S2,
         TSTSI_F mapdot TS .* TSI_F,
         VCV1 mapdot VC .* V1,
	 LOLOD_F mapdot LO .* LOD_F,
         Data tuple Year + S0S2 + TSTSI_F + VCV1 + LOLOD_F,
	 Header = [XLabel, soi, tsi, volc, lod]
    ),

    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     i('Corr Coeff = '),  b('~5f ' - R2C2),
		     \(context_graphing:dygraph_native(lin, Header,
						       [XLabel,XUnits], [YLabel, YUnits],
						       'GISS - (lnCO2, SOI, volcanic, TSI, LOD)', Data))
                    ]
		  )
   ;
      reply_html_page([title(Characteristic),
                       \(con_text:style)],
                      [
                       \(con_text:table_multiple_entries(
                                      [Header],
                                      Data
                                                        ))
                      ]
                     )

    ).
