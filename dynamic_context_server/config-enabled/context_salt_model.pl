:- module(context_salt_model, [
			       yearly_sin_period/4,
			       yearly_cos_period/4,
			       square_with_sign/2
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_salt_model:navigate).
:- context:register(context_salt_model:plot).

yearly_sin_period(Period,true,X,Y) :- Y is sin(2*pi*X/Period).
yearly_sin_period(_,false,_,0.0).
yearly_cos_period(Period,true,X,Y) :- Y is cos(2*pi*X/Period).
yearly_cos_period(_,false,_,0.0).
/*
square_with_sign(X,Y) :- X > 0, Y is X + 0.01*X*X, !.
square_with_sign(X,X).
*/
square_with_sign(X,Y) :- Y is X^2 + 1*X.

temp_data('GISS', giss).
temp_data('HADCRUT4', hadcrut).
temp_data('BEST', best).
temp_data('GISS+HADCRUT4_CW+NOAA', global_combo).
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
temp_data('NRDC', nrdc).
temp_data('HADCRUT4_CW', hadcrut4_cw).

orbital_period('tide precession',  7.3,   'spring tides to realign with calendar date').
orbital_period('tidal cycle',      9.015, 'sun-moon-earth configuration induced oscillation').
orbital_period('lunar standstill', 18.6,  'nodal period when declination of the moon reaches max').
orbital_period('lunar absidal',    8.85,  'when sidereal period exceeds anomalistic period by one month').
orbital_period('jupiter sidereal', 11.86, 'tidal sidereal period of jupiter').
orbital_period('tidal saros',      18.03, 'Saros cycle period of eclipses of the sun and moon').


dataset(giss, L) :- giss(L).
dataset(hadcrut4_cw, L) :-
        hadcrut(L0),
        hadcrut_cw(W),
        Front1 range 1*[1,1188],
        Front0 range 0*[1,1188],
        Change range 0*[1,417],
        % Back1 range 1*[1,5],
        % Back0 range 0*[1,5],
        Mask cat [Front1,Change],
        W_Offset mapdot 0.296 .+ W,
        CW cat [Front0, W_Offset],
        M mapdot Mask * L0,
        L mapdot M + CW.

dataset(hadcrut, L) :- hadcrut(L).
dataset(best, L) :- best(L).
dataset(sst, L) :- sst(L).
dataset(land, L) :- land(L).
dataset(gistemp_dts, L) :- gistemp_dts(L).
dataset(global_combo, L) :-
	dataset(giss,L1),
	dataset(hadcrut4_cw,L2),
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
dataset(nrdc, L) :- nrdc(L).


navigate(Request) :-
   collect_unit_options(calendar, Calendar),
   con_text:collect_options(context_salt_model:temp_data, DataSet),
   findall([N,D,P], orbital_period(N,P,D), CycleData),

   reply_html_page(cliopatria(default),
                   [title('CSALT model response'),
		    script([type('text/javascript'),src('/html/js/submit.js')], [])
		    %, \(con_text:style)
		   ],
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
					  ['Correlate CO2 with model', correlate],
					  ['Correlate temperature with model', map]
					  % ['Correlate AMO with periodic', cross]
					  % , ['Cross-Correlate distance vs speed orbital modes ', cross],
					  % ['Show Arctic detrend (arctic_window > 0)', arctic]
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('graph'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('table'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('volcanos'),
                                 onclick('subm(this.form,"render");')]),
			  % \(con_text:check_box(aam, 'true', 'AAM')),
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(triple, 'true', 'Pratt filter')),
                          \(con_text:check_box(wwii_adjust, 'true', 'WWII correction')),
			  % \(con_text:check_box(lunar, 'true', 'lunar')),

			  br([]),
			  input([type('text'),
				 size(2),
				 name('lag'),
				 value('0')]),
			  select([name('t_units')], Calendar),
			  small(i(' <= lag filter on Match, smooths data and model')),

			  small(\multi_columns([
				     div([
					 style([ type('text/css'), scoped ],
					       '@import url("/html/css/context.css")'),
					 table(  %%%%%%%%%%%%%%%  Hard coded  values
				       [
					h2([i('lags[months]'),
					   img([src('/html/images/magnify-clip.png'),
						title('if negative value entered, factor is zeroed')
					       ])]),
					\(con_text:input_cells([[co2_lag,0,2],
						      [soi_lag,6,2],
						      [aero_lag,15,2],
						      [lod_lag,60.0,2],
						      [tsi_lag,6,2],
                                                      [aam_lag,6,2],
                                                      [orbit_lag,6,2],
                                                      [amo_win,-120,2],
						      [arctic_win, -120,2]
						     ]))
				       ]
					)]),
				     p([' ...... ']),
				     div([style([ type('text/css'), scoped ],
					 '@import url("/html/css/context.css")'),
					  \(con_text:check_box(wave, 'true', 'add periodic elements below')),
					  \(con_text:table_multiple_entries(
						  [[cycle, 'period description', year]],
						  CycleData
								    ))
				 ])
					 ]))
			 ]
			  ),
		      br([]),
		      \(con_text:render_iframe(render))
		     ]
						       ))
		   ]
		  ).



get_fit([Temperature, CO2, SOI, TSI, Volc, LOD, AAM, Arctic, NAO, Sin, Cos,
	                                                          S2,  C2,
	                                                          S3,  C3,
	                                                          S4,  C4,
	                                                          S5,  C5,
	                                                          S6,  C6,
	                                                          SC, CM],
	[             C,   S,   T,   A,    L,   M, Z, N, V, W, P, Q, E, F, G, H, D, I, R, U, J, K], Int, R2) :-
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
   v <- Sin,
   w <- Cos,
   p <- S2,
   q <- C2,
   e <- S3,
   f <- C3,
   g <- S4,
   h <- C4,
   d <- S5,
   i <- C5,
   r <- S6,
   u <-	C6,
   j <- SC,
   k <- CM,
   fitxy <- lm('y~c+s+a+l+t+m+z+n+v+w+p+q+e+f+g+h+d+i+r+u+j+k'),  %   Add the variables here !!! don't forger
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
   V <- 'as.double(fitxy$coefficients[10])',
   W <- 'as.double(fitxy$coefficients[11])',
   P <- 'as.double(fitxy$coefficients[12])',
   Q <- 'as.double(fitxy$coefficients[13])',
   E <- 'as.double(fitxy$coefficients[14])',
   F <- 'as.double(fitxy$coefficients[15])',
   G <- 'as.double(fitxy$coefficients[16])',
   H <- 'as.double(fitxy$coefficients[17])',
   D <- 'as.double(fitxy$coefficients[18])',
   I <- 'as.double(fitxy$coefficients[19])',
   R <- 'as.double(fitxy$coefficients[20])',
   U <- 'as.double(fitxy$coefficients[21])',
   J <- 'as.double(fitxy$coefficients[22])',
   K <- 'as.double(fitxy$coefficients[23])',
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


scale(_, lin, cross, 'factor a', '#', 'factor b', '#', false) :- !.
scale(_, lin, map, 'Model Temperature', 'C', 'Real Temperature', 'C', false) :- !.
scale(_, lin, correlate, 'TCR*ln(CO2)/ln(2)', 'C', 'Temperature', 'C', false) :- !.
scale(true, log, residual, 'Wavenumber', '2048/Month', 'Power Spectral', 'density', false) :- !.
scale(_, lin, model, 'Time', 'year', 'Temperature', 'C', true) :- !.
scale(_, lin, _, 'Time', 'year', 'Temperature', 'C', false).

single_filter(In, Out) :-
    median_filter(In, In0),
    Out window In0*12.
triple_filter(In, Out) :-
    median_filter(In, In0),
    A window In0*12,
    B window A*9,
    Out window B*7.

% temperature_series(true, true, DataSet, T) :-
%     dataset(DataSet, T0),
%    triple_filter(T0, T1),
%    T window T1*12.
temperature_series(_, true, DataSet, T) :-
    dataset(DataSet, T0),
    triple_filter(T0, T).
temperature_series(true, false, DataSet, T) :-
    dataset(DataSet, T0),
    single_filter(T0, T).
%    uniform(12,Win),
%    T window T0*Win.
temperature_series(false, false, DataSet, T) :-
    dataset(DataSet, T0),
    median_filter(T0, T).

/*
temperature_series(true, DataSet, T) :-
    dataset(DataSet, T0),
    uniform(12,Win),
    T window T0*Win.
temperature_series(false, DataSet, T) :-
    dataset(DataSet, T).
*/

temperature_series(Correction, Window, Triple, DataSet, T, Offset) :-
   temperature_series(Window, Triple, DataSet, TT),
   get_years_from_1880(TT, Year, _),
   (   Correction ->
       Profile = [[1880,0],
		  [1938,0],
		  [1943.5, -0.14],
		  % [1944, -0.12],
		  [1947,0],
		  [2014,0]],
       interpolate(Year, Profile, Offset),
       % Front range 0*[1,696],
       % Ramp range [0,0.1]/0.0025,
       % Middle range 0.1*[1,57],
       % Back range 0*[1,811],
       % Clip cat [Front,Ramp,Middle,Back],
       % Offset invert Clip,
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

get_months_from_start(T, H) :-
    length(T, L),
    H range [1, L]/1.

get_lod(Years, Lag, LOD_F) :-
    lod(LOD),
    interpolate(Years, LOD, LOD_I),
    LOD_U unbias LOD_I,
    (	Lag >= 0.0 ->
        LOD_F delay LOD_U / Lag
    ;
        LOD_F mapdot 0 .* LOD_U
    ).

get_scmss(Years, Lag, S_F) :-
    scmss(SCMSS),
    interpolate(Years, SCMSS, S_I),
    S_U unbias S_I,
    (	Lag >= 0.0 ->
        S_F delay S_U / Lag
    ;
        S_F mapdot 0 .* S_U
    ).
get_cmss(Years, Lag, C_F) :-
    cmss(CMSS),
    interpolate(Years, CMSS, C_I),
    C_U unbias C_I,
    (	Lag >= 0.0 ->
        C_F delay C_U / Lag
    ;
        C_F mapdot 0 .* C_U
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
    S unbias SOI,
    % Square
    % SU unbias S0,
    % S mapdot square_with_sign ~> SU,
    (	Lag >= 0.0 ->
        SOI_F lag S / Lag
    ;
        SOI_F mapdot 0 .* S
    ).
get_tahiti_darwin(Lag, SOI_F) :-
    tahiti(T),
    TU unbias T,
    % T2 mapdot T * T,
    darwin(D),
    DU unbias D,
    % D2 mapdot D * D,
    SOI mapdot TU - DU,
    S unbias SOI,
    (	Lag >= 0.0 ->
        SOI_F lag S / Lag
    ;
        SOI_F mapdot 0 .* S
    ).

get_tahiti(Lag, SOI_F) :-
    tahiti(SOI),
    (	Lag >= 0.0 ->
        SOI_F lag SOI / Lag
    ;
        SOI_F mapdot 0 .* SOI
    ).

get_darwin(Lag, SOI_F) :-
    darwin(SOI),
    (	Lag >= 0.0 ->
        SOI_F lag SOI / Lag
    ;
        SOI_F mapdot 0 .* SOI
    ).


get_soi_peak(Lag, SOI_F) :-
    soi(SOI),
    % triple_filter(SOI, S0),
    % peak_detector(S0, S1),
    S unbias SOI,
    (	Lag >= 0.0 ->
        SOI_F lag S / Lag
    ;
        SOI_F mapdot 0 .* S
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

get_eclipses(false, Zeros, Zeros) :- !.
get_eclipses(_, Zeros, Ecl_F) :-
    eclipses(E),
    sparse_list(Zeros, E, Ecl),
    Ecl_F lag Ecl/1.


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
    (	Lag >= 0 ->
        Arctic_F = Arc_L
    ;
        Arctic_F mapdot 0 .* Arc_L
    ).

get_amo(Win, AMO_F) :-
    amo_raw(AMO),
    A unbias AMO,
    (	Win >= 0 ->
        AW window A * Win,
	AI mapdot A - AW,
        % AMO_F lag AI / Lag
	triple_filter(AI, AMO_F)
    ;
        AMO_F mapdot 0 .* A
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
    (	Lag >= 0.0 ->
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
    Back range 0*[1,675],  % 671
    Clip cat [Front,Back],
    Zonal_Front mapdot Clip * Zonal_W,
    glaam(AAM),
    Profile mapdot 6.0 .* Zonal_Front + AAM,
    % Square
    % PU unbias P0,
    % Profile mapdot square_with_sign ~> PU,
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

show_rms([], Out, Out).
show_rms([[Name,A]|R], In, Out) :-
    rms(A, RMS),
    R1000 is 1000*RMS,
    format(atom(Value), '~1f', [R1000]),
    show_rms(R, [Name=Value|In], Out).
show_rms(Array, Values) :-
    show_rms(Array, [], Values).

show_periods([], Out, Out).
show_periods([[Period,S,C]|R], In, Out) :-
    RMS is 1000*sqrt(S*S+C*C),
    format(atom(Value), '~1f', [RMS]),
    Year is Period / 12,
    % format(atom(Name), '~dyears', [Year]),
    show_periods(R, [Year=Value|In], Out).
show_periods(Array, Values) :-
    show_periods(Array, [], Values).

html_rms(RMS, _) --> {var(RMS)}.
html_rms(RMS, Periodic) -->
    html(p(i('Temperature variance as RMS values in milliKelvin, baseline=1960'))),
    con_text:paragraphs(RMS),
    html(p(i('Individual periodic cycles in years'))),
    con_text:paragraphs(Periodic).


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
			      triple(Triple, [boolean, default(false)]),
			      volc(Sato, [boolean, default(false)]),
			      % aam(AAM_ON, [boolean, default(false)]),
			      wwii_adjust(WWII_Adjust, [boolean, default(false)]),
			      wave(WL, [boolean, default(false)]),
			      t_units(Cal, []),
			      lag(LagCal, [float]),
			      soi_lag(SL, [number]),
			      aero_lag(VL, [number]),
			      co2_lag(AL, [number]),
			      lod_lag(LL, [number]),
			      tsi_lag(TL, [number]),
			      aam_lag(ML, [number]),
			      orbit_lag(OL, [number]),
			      amo_win(NL, [number]),
			      arctic_win(AW, [number]),
                              dataset(DataSet, []),
                              evaluate(Characteristic, [default(model)])]),

    scale(FFT, LogLin, Characteristic, XLabel, XUnits, YLabel, YUnits, Show_Error_Bars),
    scaling(Cal, month, Scale),
    Lag is Scale*LagCal,

    % Get the temperature series
    temperature_series(WWII_Adjust, Window, Triple, DataSet, T, Correction),

    get_years_from_1880(T, Year, Zeros),


    get_months_from_start(T, Months),
    %Period = 11.83,
       % Period is  327.85994/24/(365/12),
       % Period = 12,
       % Period2 = 0.9057,
       % Period2 = 0.52267,
    %Period2 = 6.0,
       %Period3 is 0.5, %354.367066/24/(365/12),
%    (	WL >= 0 ->
%    Lunar_Factor = 1
%    ;
%    Lunar_Factor = 0
%    ),
    % Period2 is 22 * 12,  % 20
    % Period3 is 5.3 * 12,
    % Period4 is 12 * 12,

/*  % Eureqa
    Period is 7.3 * 12,
    Period2 is 9.1 * 12,   % 9.1 Soli-lunar cycle 20
    Period3 is 4.5 * 12,    % 4.5
    Period4 is 18.03 * 12,  % 18.03 Soros cycle tide 18.03, Lunar precessional 18.6 5.5
    Period5 is 11.86 * 12,  % Tidal sidereal period of Jupiter
    Period6 is 3.35 * 12,   % 3.35 short duration sunspot
*/
    % Scafetta
    Period is 7.3 * 12,      % precession cycle with the time for Spring tides to realign with the same day each year
    Period2 is 9.015 * 12,   % Sun-Moon-Earth tidal configuration
    Period3 is 18.6 * 12,    % Lunar precessional
    Period4 is 8.85 * 12,    % Lunar apsidal precession
    Period5 is 11.86 * 12,   % Tidal sidereal period of Jupiter
    Period6 is 18.03 * 12,   % Soros cycle tide
    % 7.3 = http://tallbloke.wordpress.com/2013/02/07/short-term-forecasting-uah-lower-troposphere/

/*
    Period is 22 * 12,   % 22
    Period2 is 11 * 12,  % 11
    Period3 is 7.35 * 12,
    Period4 is 5.5 * 12,
    Period5 is 4.4 * 12,
    Period6 is 3.65 * 12,  % 3.65
*/

    Sin mapdot yearly_sin_period(Period,WL) ~> Months,
    Cos mapdot yearly_cos_period(Period,WL) ~> Months,
    Sin2 mapdot yearly_sin_period(Period2,WL) ~> Months,
    Cos2 mapdot yearly_cos_period(Period2,WL) ~> Months,
    Sin3 mapdot yearly_sin_period(Period3,WL) ~> Months,
    Cos3 mapdot yearly_cos_period(Period3,WL) ~> Months,
    Sin4 mapdot yearly_sin_period(Period4,WL) ~> Months,
    Cos4 mapdot yearly_cos_period(Period4,WL) ~> Months,
    Sin5 mapdot yearly_sin_period(Period5,WL) ~> Months,
    Cos5 mapdot yearly_cos_period(Period5,WL) ~> Months,
    Sin6 mapdot yearly_sin_period(Period6,WL) ~> Months,
    Cos6 mapdot yearly_cos_period(Period6,WL) ~> Months,
    get_scmss(Year, OL, SCMSS),
    get_cmss(Year, OL, CMSS),


    /*
    Cos = Zeros,
    Sin2 = Zeros,
    Cos2 = Zeros,
    get_eclipses(Lunar,Zeros, Sin),
    */

    get_lod(Year, LL, LOD_F),
    get_soi(SL, S2),
    % get_tahiti_darwin(SL, S2),
    % get_soi_noise(SL, SOI_Noise, Noise2),
    % (	AAM_ON ->
    %     AAM_Lag is abs(SL)
    % ;
    %     AAM_Lag = -1
    % ),
    % get_aam(Year, AAM_Lag, AAM),
    get_zonal(ML, AAM),
    get_volcanos(Sato, Zeros, VL, V1),
    get_tsi(Year, TL, TSI_F),
    get_co2(Year, AL, LogCO2),
    get_arctic(Year, AW, Arctic),
    % get_nao(Year, NL, NAO),
    get_amo(NL, NAO),
    % get_darwin(NL, NAO),
    % get_soi_peak(NL, NAO),

    get_fit([T, LogCO2, S2, TSI_F, V1, LOD_F, AAM, Arctic, NAO, Sin,  Cos,  Sin2, Cos2, Sin3, Cos3,
	                                                        Sin4, Cos4, Sin5, Cos5, Sin6, Cos6, SCMSS, CMSS],
	    Coefficients, Int, _R2C),
	    % [NoiseA, C, SO, TS, VC,   LO],

    check_coefficients(Coefficients, [], [C, SO, TS, VC,   LO, AA, ARC, NI, SW,	 CW,  SW2, CW2,
					                                    SW3, CW3, SW4, CW4,
					                                    SW5, CW5, SW6, CW6, SC, CM]),

    Fluct mapdot SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F + AA .* AAM +
                 ARC .* Arctic + NI .* NAO + SW .* Sin + CW .* Cos
		                           + SW2 .* Sin2 + CW2 .* Cos2
		                           + SW3 .* Sin3 + CW3 .* Cos3
		                           + SW4 .* Sin4 + CW4 .* Cos4
		                           + SW5 .* Sin5 + CW5 .* Cos5
		                           + SW6 .* Sin6 + CW6 .* Cos6
                                           + SC .* SCMSS + CM .* CMSS,

    T_CO2_R mapdot C .* LogCO2 + Fluct,
    T_R mapdot Int .+ T_CO2_R,
    T_Diff mapdot T - T_R,
    %
    % Drift_Baseline mapdot C .* LogCO2 + LO .* LOD_F,
    % T_From_Baseline mapdot T - Drift_Baseline,
    !,
    sum_of_squares(T, T_R, Sum_of_Sq),
    length(Months, Length_Series),
    Error_Bar is 2*sqrt(Sum_of_Sq/Length_Series),

    T_lag lag T / Lag,
    T_R_lag lag T_R / Lag,
    corrcoeff(T_lag, T_R_lag, R2C2),
    (
       Characteristic = arctic ->
	 Data tuple Year + Arctic,
         Header = [XLabel, arctic]
     ;
       Characteristic = model ->
         T_lag_low mapdot T_lag .- Error_Bar,
         T_lag_high mapdot Error_Bar .+ T_lag,
         T_lags tuple T_lag_low + T_lag + T_lag_high,
         T_R_lags tuple T_R_lag + T_R_lag + T_R_lag,
         Corrections tuple Correction + Correction + Correction,
	 Data group Year + T_lags + T_R_lags + Corrections,
         Header = [XLabel, DataSet, model, correction]
     ;
       Characteristic = map ->
	 Data tuple T_R_lag + T_lag,
         Header = [model, DataSet]
     ;
/*       Characteristic = cross ->
         Cross correlate T_R_lag * T_lag,
         get_months_from_start(T, Months),
	 Data tuple Months + Cross,
         Header = [month, cross]
     ;
       Characteristic = cross ->
	 Y_Lunar mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2,
	 Data tuple Year + Y_Lunar + T_Diff,
         Header = [XLabel, yearly, residual]

     ;
       Characteristic = cross ->
	 % Waves mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
         %                                     + SW3 .* Sin3 + CW3 .* Cos3
         %                                     + SW4 .* Sin4 + CW4 .* Cos4
         %                                     + SW5 .* Sin5 + CW5 .* Cos5
         %                                     + SW6 .* Sin6 + CW6 .* Cos6,
         % Y_Lunar_Win lag Y_Lunar/60,
         % Orbit mapdot  SC .* SCMSS + CM .* CMSS,
         % TSI_C mapdot  TS .* TSI_F,
         % S0S2 mapdot SO .* S2,
         % AAM_D mapdot AA .* AAM,
         Distance mapdot  CM .* CMSS,
         Speed mapdot  SC .* SCMSS,
	 Data tuple Distance + Speed,
         Header = [distance, speed]
     ;
*/
       Characteristic = cross ->
	 Waves mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
                                              + SW3 .* Sin3 + CW3 .* Cos3
                                              + SW4 .* Sin4 + CW4 .* Cos4
                                              + SW5 .* Sin5 + CW5 .* Cos5
                                              + SW6 .* Sin6 + CW6 .* Cos6,
         AMO_C mapdot  NI .* NAO,
         % Combo mapdot Waves + AMO_C,
         Orbit mapdot  SC .* SCMSS + CM .* CMSS,
	 Data tuple Year + AMO_C + Waves + Orbit,
         Header = [XLabel, amo, periods, orbit]
     ;
       Characteristic = residual ->
         (   FFT ->
	     R_FFT fft T_Diff,
	     Range ordinal R_FFT,
	     Data tuple Range + R_FFT
	 ;
             % T_Base mapdot Int .+ C .* LogCO2 + LO .* LOD_F ,
             % Noise_Level mapdot T - T_Base,
	     Data tuple Year + T_Diff %  + Noise_Level
	 ),

         Header = [XLabel, residual] % , fluctuation]
     ;
       Characteristic = signal ->
         CO2_Signal mapdot Int .+ C .* LogCO2,
         Signal mapdot CO2_Signal + T_Diff,
         % median_filter(Sig, Signal),
         Data tuple Year + Signal + CO2_Signal + Fluct,
         Header = [XLabel, signal,  co2,         fluctuation]
     ;
       Characteristic = correlate ->
         CO2_Signal mapdot Int .+ C .* LogCO2,
         Signal mapdot CO2_Signal + T_Diff,
         Data tuple CO2_Signal + Signal + CO2_Signal,
         Header = [co2, data, log_model]
     ;
       Characteristic = all ->
         S0S2 mapdot SO .* S2,
         TSTSI_F mapdot TS .* TSI_F,
         VCV1 mapdot VC .* V1,
	 LOLOD_F mapdot LO .* LOD_F,
         % Noise_D mapdot NoiseA .* Noise2,
         AAM_D mapdot AA .* AAM,
         % ARCTIC_D mapdot ARC .* Arctic,
         NAO_D mapdot NI .* NAO,
	 Waves mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
                                              + SW3 .* Sin3 + CW3 .* Cos3
                                              + SW4 .* Sin4 + CW4 .* Cos4
					      + SW5 .* Sin5 + CW5 .* Cos5
                                              + SW6 .* Sin6 + CW6 .* Cos6,
         Orbit mapdot  SC .* SCMSS + CM .* CMSS,
	 CO2_Strength mapdot C .* LogCO2,
	 show_rms([[amo, NAO_D],
		   ['set of observed cycle periods',Waves],
		   [tsi,TSTSI_F],
		   [orbit,Orbit],
		   [aero, VCV1],
		   [aam,AAM_D],
		   [soi,S0S2],
		   [lod,LOLOD_F],
		   [co2, CO2_Strength]], RMS),
	 show_periods([[Period, SW, CW],
		       [Period2, SW2, CW2],
		       [Period3, SW3, CW3],
		       [Period4, SW4, CW4],
		       [Period5, SW5, CW5],
		       [Period6, SW6, CW6]], Periodic),

     /*
         Angular mapdot AAM_D + LOLOD_F + Y_Lunar,
         Data tuple Year + S0S2 + VCV1 + TSTSI_F + ARCTIC_D + NAO_D + Angular,
	 Header = [XLabel, soi,   aero,  tsi,      arctic,    nao,    angular]
     */
         Data tuple Year + S0S2 + VCV1 + LOLOD_F + TSTSI_F + AAM_D + Orbit + Waves + NAO_D,
	 Header = [XLabel, soi,   aero,  lod,      tsi,      aam,    orbit,  wave,    nao]

    ),
    temp_data(NameData, DataSet),
    TCR is C*ln(2),
    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th('R=cc'), th('ln(co2)'),th(soi),th('a(volc)'),
				th(lod),th(tsi), th(aam), th(orbit), th(nao), th(arctic)]),
			    tr([td(b('~5f'-R2C2)),td('~3f'-C),td('~3f'-SO),td('~3f'-VC),
				td('~3f'-LO),td('~3f'-TS), td('~5f'-AA), td('~5f'-SC), td('~5f'-NI), td('~5f'-ARC)])
			   ]),
		     br([]),
		     div([id('legend')],[]),
		     % \(context_graphing:dygraph_native(LogLin, Header,
		     %				       [XLabel,XUnits], [YLabel, YUnits],
		     %				       [NameData, ' - CSALT:', Characteristic], Data)),
		     \(context_graphing:dygraph_error_bars(LogLin, Header,
						       [XLabel,XUnits], [YLabel, YUnits],
						       [NameData, ' - CSALT:', Characteristic], Data, Show_Error_Bars)),
		     br([]),
		     br([]),
		     p(i('TCR = ~4f C for doubling of CO2'-TCR)),
		     \html_rms(RMS, Periodic)
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







