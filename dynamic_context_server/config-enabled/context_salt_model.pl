:- module(context_salt_model, [
			       % yearly_period/4
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_salt_model:navigate).
:- context:register(context_salt_model:plot).

%  yearly_period(Period,Phase, X,Y) :-
%    Y is sin(2*pi*(X+Phase/12)/Period).

temp_data('GISS', giss).
temp_data('HADCRUT4', hadcrut).
temp_data('BEST', best).
temp_data('GISS+HADCRUT4+NOAA', global_combo).
temp_data('HADSST3', sst).
temp_data('CRUTEM4', land).
temp_data('GISSLand', gistemp_dts).
temp_data('BEST+CRUTEM4+GISSLand', land_combo).
temp_data('CRUTEM+2*HADSST', composed).
temp_data('NOAA LAND OCEAN', noaa_land_ocean).
temp_data('NOAA LAND', noaa_land).
temp_data('NOAA OCEAN', noaa_ocean).
temp_data('HADCRUT3', hadcrut3).
temp_data('HADSST2', sst2).

dataset(giss, L) :- giss(L).
dataset(hadcrut, L) :- hadcrut(L).
dataset(best, L) :- best(L).
dataset(sst, L) :- sst(L).
dataset(land, L) :- land(L).
dataset(gistemp_dts, L) :- gistemp_dts(L).
dataset(global_combo, L) :-
	dataset(giss,L1),
	dataset(hadcrut,L2),
	dataset(noaa_land_ocean,L3),
	L mapdot 0.3333 .* L1 + 0.3333 .* L2 + 0.3333 .* L3.
dataset(land_combo, L) :-
	dataset(land,L1),
	dataset(best,L2),
        dataset(gistemp_dts,L3),
	L mapdot 0.3333 .* L1 + 0.3333 .* L2 + 0.3333 .* L3.
dataset(composed, L) :-
	dataset(land,L1),
	dataset(sst,L2),
	L mapdot 0.3 .* L1  +  1.4 .* L2.
dataset(noaa_land_ocean, L) :- noa_land_ocean(L).
dataset(noaa_land, L) :- noa_land(L).
dataset(noaa_ocean, L) :- noa_ocean(L).
dataset(sst2, L) :- hadsst2(L).
dataset(hadcrut3, L) :- hadcrut3(L).


navigate(Request) :-
   collect_unit_options(calendar, Calendar),
   con_text:collect_options(context_salt_model:temp_data, DataSet),

   reply_html_page(cliopatria(default),
                   [title('CSALT model response'),
		    script([type('text/javascript'),src('/html/js/submit.js')], [])],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('CSALT Model'),
                      p('Select a data set and a view of the defluctuated profile'),
                      form([action(plot), target(target_iframe)],
			 [
			  select([name('dataset')], DataSet),
			  % \(con_text:check_box(anthro, 'true', 'anthro aerosols')),
			  \(con_text:check_box(volc, 'true', 'GISS aerosol model')),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [
                                          ['Underlying CO2 signal', signal],
					  ['View the residual error', residual],
					  ['View the fluctuation components', all],
					  ['Match temperature with model', model],
					  ['Show Arctic detrend (arctic_window > 0)', arctic]
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('graph'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('table'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('volcanos'),
                                 onclick('subm(this.form,"render");')]),
			  \(con_text:check_box(aam, 'true', 'AAM')),
			  \(con_text:check_box(arctic_noise, 'true', 'WWII correction')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  br([]),
			  input([type('text'),
				 size(2),
				 name('lag'),
				 value('0')]),
			  select([name('t_units')], Calendar),
			  i(' <= lag filter on Match'),
			  h2([i('lag values (months)'),
			      img([src('/html/images/magnify-clip.png'),
				   title('if negative value entered, factor is zeroed')
				  ])]),
			  table(
			      % [width('20%')],
			      %%%%%%%%%%%%%%%  Hard coded
			      \(con_text:input_cells([[co2_lag,6,2],
						      [soi_lag,6,2],
						      [aero_lag,24,2],
						      [lod_lag,60.0,2],
						      [tsi_lag,6,2],
						      [arctic_window, -120,2]
						     ]))
			       )
			 ]
                          ),

                      br([]),
		      \(con_text:render_iframe(render))
		     ]
						       ))
		   ]
		  ).



get_fit([Temperature, CO2, SOI, TSI, Volc, LOD, AAM, Arctic, NAO],
	[             C,   S,   T,   A,    L,   M, Z, N], Int, R2) :-
   r_open_session,
   y <- Temperature,
   c <- CO2,
   s <- SOI,
   a <- Volc,
   l <- LOD,
   t <- TSI,
   m <- AAM,
   z <- Arctic,
   n <- NAO,
   fitxy <- lm('y~c+s+a+l+t+m+z+n'),  %   Add the variables here !!! don't forger
   r_print(fitxy),
   Int <- 'as.double(fitxy$coefficients[1])',
   C <- 'as.double(fitxy$coefficients[2])',
   S <- 'as.double(fitxy$coefficients[3])',
   A <- 'as.double(fitxy$coefficients[4])',
   L <- 'as.double(fitxy$coefficients[5])',
   T <- 'as.double(fitxy$coefficients[6])',
   M <- 'as.double(fitxy$coefficients[7])',
   Z <- 'as.double(fitxy$coefficients[8])',
   N <- 'as.double(fitxy$coefficients[9])',
   summary <- summary(fitxy),
   r_print(summary),
   R2 <- 'as.double(summary$r.squared)',
   r_close, !.


/*
wars([
          [765,1]   % 1991 1332  766
	 ]).

nowars([
     [3,1], [5,-1], [7,1], [9,-1], [11,1], [13,-1], [15,1], [17,-1], [19,1]
       ]).

get_anthro(true, Lag, Zeros, W) :-
	wars(War),
	sparse_list(Zeros, War, Wars),
	W0 lag Wars/3,
	W lag W0/Lag.

get_anthro(false, _, Zeros, W) :-
	nowars(War),
	sparse_list(Zeros, War, W).
*/

scale(true, log, residual, 'Wavenumber', '2048/Month', 'Power Spectral', 'density') :- !.
scale(_, lin, _, 'Time', 'year', 'Temperature', 'C').

temperature_series(true, DataSet, T) :-
    dataset(DataSet, T0),
    uniform(12,Win),
    T window T0*Win.
temperature_series(false, DataSet, T) :-
    dataset(DataSet, T).

temperature_series(Correction, Window, DataSet, T, Offset) :-
   temperature_series(Window, DataSet, TT),
   (   Correction ->
       Front range 0*[1,696],
       Ramp range [0,0.1]/0.0025,
       Middle range 0.1*[1,57],
       % Middle range 0.1*[1,90],
       Back range 0*[1,807],
       Clip cat [Front,Ramp,Middle,Back],
       Offset invert Clip,
       T mapdot TT + Offset
   ;
       T = TT,
       Offset mapdot 0 .* TT
   ).


get_years_from_1880(T, Years, Zeros) :-
    length(T, L),
    H range [1, L]/1,
    Y mapdot H ./ 12.0,
    Years mapdot 1880 .+ Y,
    Zeros mapdot 0 .* H.

get_lod(Years, Lag, LOD_F) :-
    lod(LOD),
    interpolate(Years, LOD, LOD_I),
    LOD_U unbias LOD_I,
    (	Lag >= 0.0 ->
        LOD_F delay LOD_U / Lag
    ;
        LOD_F mapdot 0 .* LOD_U
    ).

get_soi_noise(_, false, Noise_F) :-
    soi_noise(Noise),
    Noise_F mapdot 0 .* Noise.
get_soi_noise(Lag, true, Noise_F) :-
    soi_noise(Noise),
    Noise0 unbias Noise,
    (   Lag >= 0.0  ->
        Noise_F lag Noise0 / Lag
    ;
        Noise_F mapdot 0 .* Noise0
    ).

get_soi(Lag, SOI_F) :-
    soi(SOI),
    S0 unbias SOI,
    (	Lag >= 0.0 ->
        SOI_F lag S0 / Lag
    ;
        SOI_F mapdot 0 .* S0
    ).

get_volcanos(true, _, Lag, Vol_F) :-
    Lag >= 0.0,
    sato_volc(V),
    Vol_F lag V/6.
get_volcanos(false, Zeros, Lag, Vol_F) :-
    Lag >= 0.0,
    context_box_model:volcanos(V),
    sparse_list(Zeros, V, Vol),
    V0 lag Vol/6,
    Vol_F lag V0/Lag.
get_volcanos(_, Zeros, _, Zeros).


get_tsi(Years, Lag, TSI_F) :-
    tsi(TSI),
    interpolate(Years, TSI, TSI_I),
    TSI_U unbias TSI_I,
    (	Lag >= 0.0 ->
        TSI_F lag TSI_U / Lag
    ;
        TSI_F mapdot 0 .* TSI_U
    ).

get_arctic(Years, Win, Arctic_F) :-
    arctic(Arc),
    interpolate(Years, Arc, Arc_I),
    Arc_L unbias Arc_I,
    (	Win > 0 ->
        Arctic window Arc_L * Win,
	Arctic_I mapdot Arc_L - Arctic,
        Arctic_F = Arctic_I
    ;
        Arctic_F mapdot 0 .* Arc_L
    ).



get_nao(Years, Lag, Arctic_F) :-
    nao_hurrell_year(Arc),
    interpolate(Years, Arc, Arc_I),
    Arc_U unbias Arc_I,
    Arc_L lag Arc_U / Lag,
    (	Lag > 0 ->
        Arctic_F = Arc_L
    ;
        Arctic_F mapdot 0 .* Arc_L
    ).

/*
get_nao(_Noise, Win, Arctic_F) :-
    nao_hurrell(Arc_I),
    Arc_U unbias Arc_I,
    Arc_W window Arc_U * 12,
    Arc_L lag Arc_W / 6,
    (	Win > 0 ->
        %(   Noise ->
        %    Front range 0*[1,696],
        %    Middle range 2*[1,90],
        %    Back range 0*[1,815],
        %    Clip cat [Front,Middle,Back],
        %    Arctic_F mapdot Arc_L + Clip
        %;
            Arctic_F = Arc_L
        %)
    ;
        Arctic_F mapdot 0 .* Arc_L
    ).
*/

get_co2(Years, Lag, LogCO2) :-
    context_co2:co2_knmi(CO2),
    interpolate(Years, CO2, CO2_I),
    (	Lag > 0.0 ->
	CO2_Lag lag CO2_I / Lag,
        LogCO2 mapdot ln ~> CO2_Lag
     ;
        LogCO2 mapdot 0 .* CO2_I
     ).

get_aam(Years, Lag, AAM_F) :-
    aam_graph(AAM),
    interpolate(Years, AAM, AAM_I),
    (	Lag >= 0.0 ->
	AAM_F lag AAM_I / Lag
    ;
        AAM_F mapdot 0 .* AAM_I
    ).

get_zonal(Lag, Zonal_F) :-
    zonal_average(Zonal),
    Zonal_U unbias Zonal,
    Zonal_W window Zonal_U*12,
    Front range 1*[1,930],
    Back range 0*[1,671],
    Clip cat [Front,Back],
    Zonal_Front mapdot Clip * Zonal_W,
    glaam(AAM),
    Profile mapdot 6.0 .* Zonal_Front + AAM,
    (	Lag >= 0.0 ->
	Zonal_F lag Profile / Lag
    ;
        Zonal_F mapdot 0 .* Profile
    ).

check_coefficients([], List, Final ) :- reverse(List, Final), !.
check_coefficients([F|Rest], List, Final ) :-
    F = 'NA', !,
    check_coefficients(Rest, [0.0|List], Final).
check_coefficients([F|Rest], List, Final ) :-
    check_coefficients(Rest, [F|List], Final).

plot(Request) :-
    http_parameters(Request, [kind(volcanos, [])]),
    context_box_model:volcano_data(Data),
    reply_html_page([title('volcano data'),
                       \(con_text:style)],
                      [
                       \(con_text:table_multiple_entries(
                                      [['eruption',year,month,'month#',intensity]],
                                      Data
                                                        ))
                      ]
                     ).

plot(Request) :-
    garbage_collect,
    http_parameters(Request, [kind(Kind, []),
			      fft(FFT, [boolean, default(false)]),
			      window(Window, [boolean, default(false)]),
			      volc(Sato, [boolean, default(false)]),
			      aam(AAM_ON, [boolean, default(false)]),
			      arctic_noise(ARC_N, [boolean, default(false)]),
			      t_units(Cal, []),
			      lag(LagCal, [float]),
			      soi_lag(SL, [number]),
			      aero_lag(VL, [number]),
			      co2_lag(AL, [number]),
			      lod_lag(LL, [number]),
			      tsi_lag(TL, [number]),
			      arctic_window(AW, [number]),
                              dataset(DataSet, []),
                              evaluate(Characteristic, [default(model)])]),

    scale(FFT, LogLin, Characteristic, XLabel, XUnits, YLabel, YUnits),
    scaling(Cal, month, Scale),
    Lag is Scale*LagCal,

    % Get the temperature series
    temperature_series(ARC_N, Window, DataSet, T, Correction),

    get_years_from_1880(T, Year, Zeros),

    get_lod(Year, LL, LOD_F),
    get_soi(SL, S2),
    % get_soi_noise(SL, SOI_Noise, Noise2),
    (	AAM_ON ->
        AAM_Lag is abs(SL)
    ;
        AAM_Lag = -1
    ),
    % get_aam(Year, AAM_Lag, AAM),
    get_zonal(AAM_Lag, AAM),
    get_volcanos(Sato, Zeros, VL, V1),
    get_tsi(Year, TL, TSI_F),
    get_co2(Year, AL, LogCO2),
    get_arctic(Year, AW, Arctic),
    get_nao(Year, SL, NAO),

    get_fit([T, LogCO2, S2, TSI_F, V1, LOD_F, AAM, Arctic, NAO],
	    Coefficients, Int, _R2C),
	    % [NoiseA, C, SO, TS, VC,   LO],

    check_coefficients(Coefficients, [], [C, SO, TS, VC,   LO, AA, ARC, NI]),

    Fluct mapdot SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F + AA .* AAM + ARC .* Arctic + NI .* NAO,
    T_CO2_R mapdot C .* LogCO2 + Fluct,
    T_R mapdot Int .+ T_CO2_R,
    T_Diff mapdot T - T_R,
    !,
    T_lag lag T / Lag,
    T_R_lag lag T_R / Lag,
    corrcoeff(T_lag, T_R_lag, R2C2),
    (
       Characteristic = arctic ->
	 Data tuple Year + Arctic,
         Header = [XLabel, arctic]
     ;
       Characteristic = model ->
	 Data tuple Year + T_lag + T_R_lag + Correction,
         Header = [XLabel, temperature, model, correction]
     ;
       Characteristic = residual ->
         (   FFT ->
	     R_FFT fft T_Diff,
	     Range ordinal R_FFT,
	     Data tuple Range + R_FFT
	 ;
	     Data tuple Year + T_Diff
	 ),

         Header = [XLabel, residual]
     ;
       Characteristic = signal ->
         CO2_Signal mapdot Int .+ C .* LogCO2,
         Signal mapdot CO2_Signal + T_Diff,
         Data tuple Year + Signal + CO2_Signal + Fluct,
         Header = [XLabel, signal,  co2,         fluctuation]
     ;
       Characteristic = all ->
         S0S2 mapdot SO .* S2,
         TSTSI_F mapdot TS .* TSI_F,
         VCV1 mapdot VC .* V1,
	 LOLOD_F mapdot LO .* LOD_F,
         % Noise_D mapdot NoiseA .* Noise2,
         AAM_D mapdot AA .* AAM,
         ARCTIC_D mapdot ARC .* Arctic,
         NAO_D mapdot NI .* NAO,
         Data tuple Year + S0S2 + VCV1 + LOLOD_F + TSTSI_F + AAM_D + ARCTIC_D + NAO_D,
	 Header = [XLabel, soi,   aero,  lod,      tsi,      aam,    arctic,    nao]
    ),
    temp_data(NameData, DataSet),
    TCR is C*ln(2),
    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th(corrcoeff), th('ln(co2)'),th(soi),th('a(volc)'),
				th(lod),th(tsi), th(aam), th(nao), th(arctic)]),
			    tr([td(b('~5f'-R2C2)),td('~3f'-C),td('~3f'-SO),td('~3f'-VC),
				td('~3f'-LO),td('~3f'-TS), td('~5f'-AA), td('~5f'-NI), td('~5f'-ARC)])
			   ]),
		     br([]),
		     div([id('legend')],[]),
		     \(context_graphing:dygraph_native(LogLin, Header,
						       [XLabel,XUnits], [YLabel, YUnits],
						       [NameData, ' - CSALT:', Characteristic], Data)),
		     br([]),
		     br([]),
		     p(i('TCR = ~4f C for doubling of CO2'-TCR))
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







