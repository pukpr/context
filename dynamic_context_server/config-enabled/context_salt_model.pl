:- module(context_salt_model, [
			       yearly_sin_period/4,
			       yearly_cos_period/4
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_salt_model:navigate).
:- context:register(context_salt_model:plot).

yearly_sin_period(Period,L,X,Y) :- Y is L*sin(2*pi*X/Period).
yearly_cos_period(Period,L,X,Y) :- Y is L*cos(2*pi*X/Period).

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
					  ['Correlate CO2 with model', correlate],
					  ['Correlate temperature with model', map],
					  ['Cross-Correlate scmss with lunar', cross],
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
			  \(con_text:check_box(wwii_adjust, 'true', 'WWII correction')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(triple, 'true', 'Pratt window')),
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  \(con_text:check_box(lunar, 'true', 'lunar')),
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


scale(_, lin, cross, 'Month', '#', 'Correlation', ' ') :- !.
scale(_, lin, map, 'Model Temperature', 'C', 'Real Temperature', 'C') :- !.
scale(_, lin, correlate, 'TCR*ln(CO2)/ln(2)', 'C', 'Temperature', 'C') :- !.
scale(true, log, residual, 'Wavenumber', '2048/Month', 'Power Spectral', 'density') :- !.
scale(_, lin, _, 'Time', 'year', 'Temperature', 'C').

triple_filter(In, Out) :-
    A window In*12,
    B window A*9,
    Out window B*7.

temperature_series(true, true, DataSet, T) :-
    dataset(DataSet, T0),
    triple_filter(T0, T1),
    T window T1*12.
temperature_series(false, true, DataSet, T) :-
    dataset(DataSet, T0),
    triple_filter(T0, T).
temperature_series(true, false, DataSet, T) :-
    dataset(DataSet, T0),
    uniform(12,Win),
    T window T0*Win.
temperature_series(false, false, DataSet, T) :-
    dataset(DataSet, T).

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
   (   Correction ->
       Front range 0*[1,696],
       Ramp range [0,0.1]/0.0025,
       Middle range 0.1*[1,57],
       % Middle range 0.1*[1,90],
       Back range 0*[1,811],
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
			      triple(Triple, [boolean, default(false)]),
			      volc(Sato, [boolean, default(false)]),
			      aam(AAM_ON, [boolean, default(false)]),
			      wwii_adjust(WWII_Adjust, [boolean, default(false)]),
			      lunar(Lunar, [boolean, default(false)]),
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
    (	Lunar ->
    Lunar_Factor = 1
    ;
    Lunar_Factor = 0
    ),
    % Period2 is 22 * 12,  % 20
    % Period3 is 5.3 * 12,
    % Period4 is 12 * 12,

    Period is 7.35 * 12,
    Period2 is 9.15 * 12,  % 20
    Period3 is 4.4 * 12,
    Period4 is 5.5 * 12,
    Period5 is 11.79 * 12,
    Period6 is 3.35 * 12,

/*
    Period is 22 * 12,   % 22
    Period2 is 11 * 12,  % 11
    Period3 is 7.35 * 12,
    Period4 is 5.5 * 12,
    Period5 is 4.4 * 12,
    Period6 is 3.65 * 12,  % 3.65
*/

    Sin mapdot yearly_sin_period(Period,Lunar_Factor) ~> Months,
    Cos mapdot yearly_cos_period(Period,Lunar_Factor) ~> Months,
    Sin2 mapdot yearly_sin_period(Period2,Lunar_Factor) ~> Months,
    Cos2 mapdot yearly_cos_period(Period2,Lunar_Factor) ~> Months,
    Sin3 mapdot yearly_sin_period(Period3,Lunar_Factor) ~> Months,
    Cos3 mapdot yearly_cos_period(Period3,Lunar_Factor) ~> Months,
    Sin4 mapdot yearly_sin_period(Period4,Lunar_Factor) ~> Months,
    Cos4 mapdot yearly_cos_period(Period4,Lunar_Factor) ~> Months,
    Sin5 mapdot yearly_sin_period(Period5,Lunar_Factor) ~> Months,
    Cos5 mapdot yearly_cos_period(Period5,Lunar_Factor) ~> Months,
    Sin6 mapdot yearly_sin_period(Period6,Lunar_Factor) ~> Months,
    Cos6 mapdot yearly_cos_period(Period6,Lunar_Factor) ~> Months,
    get_scmss(Year, 0, SCMSS),
    get_cmss(Year, 0, CMSS),


    /*
    Cos = Zeros,
    Sin2 = Zeros,
    Cos2 = Zeros,
    get_eclipses(Lunar,Zeros, Sin),
    */

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
         Header = [XLabel, DataSet, model, correction]
     ;
       Characteristic = map ->
	 Data tuple T_R_lag + T_lag,
         Header = [model, DataSet]
/*     ;
       Characteristic = cross ->
         Cross correlate T_R_lag * T_lag,
         get_months_from_start(T, Months),
	 Data tuple Months + Cross,
         Header = [month, cross]
     ;
       Characteristic = cross ->
	 Y_Lunar mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2,
	 Data tuple Year + Y_Lunar + T_Diff,
         Header = [XLabel, yearly, residual]
*/
     ;
       Characteristic = cross ->
	 Y_Lunar mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
                                              + SW3 .* Sin3 + CW3 .* Cos3
                                              + SW4 .* Sin4 + CW4 .* Cos4
                                              + SW5 .* Sin5 + CW5 .* Cos5
                                              + SW6 .* Sin6 + CW6 .* Cos6,
         % Y_Lunar_Win lag Y_Lunar/60,
         TSI_C mapdot  TS .* TSI_F,
	 Data tuple Year + Y_Lunar + TSI_C,
         Header = [XLabel, solar, tsi]
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
         ARCTIC_D mapdot ARC .* Arctic,
         NAO_D mapdot NI .* NAO,
	 Y_Lunar mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
                                              + SW3 .* Sin3 + CW3 .* Cos3
                                              + SW4 .* Sin4 + CW4 .* Cos4
					      + SW5 .* Sin5 + CW5 .* Cos5
                                              + SW6 .* Sin6 + CW6 .* Cos6 +
                                               SC .* SCMSS + CM .* CMSS,

     /*
         Angular mapdot AAM_D + LOLOD_F + Y_Lunar,
         Data tuple Year + S0S2 + VCV1 + TSTSI_F + ARCTIC_D + NAO_D + Angular,
	 Header = [XLabel, soi,   aero,  tsi,      arctic,    nao,    angular]
     */
         Data tuple Year + S0S2 + VCV1 + LOLOD_F + TSTSI_F + AAM_D + ARCTIC_D + NAO_D + Y_Lunar,
	 Header = [XLabel, soi,   aero,  lod,      tsi,      aam,    arctic,    nao,    sun]

    ),
    temp_data(NameData, DataSet),
    TCR is C*ln(2),
    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th('R=cc'), th('ln(co2)'),th(soi),th('a(volc)'),
				th(lod),th(tsi), th(aam), th(nao), th(year), th(arctic)]),
			    tr([td(b('~5f'-R2C2)),td('~3f'-C),td('~3f'-SO),td('~3f'-VC),
				td('~3f'-LO),td('~3f'-TS), td('~5f'-AA), td('~5f'-NI), td('~5f'-SW), td('~5f'-ARC)])
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







