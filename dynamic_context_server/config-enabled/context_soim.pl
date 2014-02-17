:- module(context_soim, [
			       yearly_sin_period/5,
			       yearly_cos_period/5,
			       square_with_sign/2,
                               fake_power_law/4
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_soim:navigate).
:- context:register(context_soim:plot).

% test_period(N,_,Period) :- Period is N*N/20*12+N*2.134.
test_period(_,P,P).

yearly_sin_period(0.0,N,M,_,0) :- N =< M.
yearly_sin_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is sin(2*pi*X/P).
yearly_sin_period(_,N,M,_,0.0) :- N > M.
yearly_cos_period(0.0,N,M,_,0) :- N =< M.
yearly_cos_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is cos(2*pi*X/P).
yearly_cos_period(_,N,M,_,0.0) :- N > M.
square_with_sign(X,Y) :- Y is X^2 + 1*X.
fake_power_law(S,N,X,Y) :- Y is (X/S)^N.


temp_data('SOI', soi).
temp_data('Darwin', darwin).
temp_data('Tahiti', tahiti).
temp_data('OU Random Walk', ou_rw).


dataset(soi, L) :-
    soi(L0),
    L unbias L0.
dataset(darwin, L) :-
    darwin(L0),
    L unbias L0.
dataset(tahiti, L) :-
    tahiti(L0),
    L unbias L0.


dataset(ou_rw, L) :-
    Range range [1,1605]/1,
    context_random_walk:ou_random_walker(0.005,1,0.1,Range,RW),
    L mapdot % PL +
             RW, !.



navigate(Request) :-
   collect_unit_options(calendar, Calendar),
   con_text:collect_options(context_salt_model:temp_data, DataSet),

   reply_html_page(cliopatria(default),
                   [title('CSALT model response'),
		    script([type('text/javascript'),src('/html/js/submit.js')], [])
		   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('CSALT Model'),
                      form([action(plot), target(target_iframe)],
			 [
                           p(['Select data set and a profile view ',
			      input([type('text'),
				 size(4),
				 name('startYear'),
				 value('1880')]),
			      ' : ',
			      input([type('text'),
				 size(4),
				 name('fitYear'),
				 value('2014')])
			     ]),

			  select([name('dataset')], DataSet),
			  \(con_text:check_box(volc, 'true', 'GISS aerosol model')),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [
					  ['View the residual error', residual],
					  ['Match temperature with model', model]
                                         ])),
                          br([]),
			  input([type('submit'), name(kind), value('graph'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('table'),
                                  onclick('subm(this.form,"target_iframe");')]),
			  input([type('submit'), name(kind), value('volcanos'),
                                 onclick('subm(this.form,"render");')]),
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(triple, 'true', 'Pratt filter')),
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
					       ])])
				       ]
					)]),
				     p([' ...... '])
					 ]))
			 ]
			  ),
		      br([]),
		      \(con_text:render_iframe(render))
		     ]
						       ))
		   ]
		  ).



get_fit(StartDate, EndDate, [Temperature, CO2, SOI, TSI, Volc, LOD, AAM, Arctic, NAO, Sin, Cos,
	                                                          S2,  C2,
	                                                          S3,  C3,
	                                                          S4,  C4,
	                                                          S5,  C5,
	                                                          S6,  C6,
	                                                          S7,  C7,
	                                                          S8,  C8,
	                                                          S9,  C9,
	                                                          SA,  CA,
	                                                          SB,  CB,
	                                                          SC,  CC,
	                                                          SD,  CD,
	                                                          SE,  CE,
	                                                          SF,  CF,
	                                                          SG,  CG,
	                                                          SI,  CI,
	                                                          SJ,  CJ,
	                                                          BSC, BCM,
								  Dynamo
			                                          %, Methane
								  ],
	[             C,   S,   T,   A,    L,   M, Z, N, V, W, P, Q,
		                                E, F, G, H, D, I, R, U,
		                                A1, B1, C1, D1, E1, F1,
		                                G1, H1, I1, J1, K1, L1,
		                                M1, N1, O1, P1, Q1, R1,
		                                S1, T1, U1, V1, W1, X1, J, K, DY], Int, R2) :-
   r_open_session,
   y <- Temperature,
   c <- CO2,
   s <- SOI,
   a <- Volc,
   l <- LOD,
   t <- TSI,
   m <- AAM,
   veS <- Arctic,
   veC <- NAO,
   h3S <- Sin,
   h3C <- Cos,
   s9S <- S2,
   s9C <- C2,
   diS <- S3,
   diC <- C3,
   t1S <- S4,
   t1C <- C4,
   j1S <- S5,
   j1C <- C5,
   h7S <- S6,
   h7C <- C6,
   j2S <- S7,
   j2C <- C7,
   d3S <- S8,
   d3C <- C8,
   t2S <- S9,
   t2C <- C9,
   h4S <- SA,
   h4C <- CA,
   h5S <- SB,
   h5C <- CB,
   h6S <- SC,
   h6C <- CC,
   h2S <- SD,
   h2C <- CD,
   h1S <- SE,
   h1C <- CE,
   q3S <- SF,
   q3C <- CF,
   loS <- SG,
   loC <- CG,
   v4S <- SI,
   v4C <- CI,
   s3S <- SJ,
   s3C <- CJ,
   bsc <- BSC,
   bcm <- BCM,
   dyn <- Dynamo,
   B is (EndDate - 1880)*12,
   O is (StartDate - 1880)*12 + 1,
   % O = 1, % 840,
   format(atom(Eq), 'y[~d:~d]~~c[~d:~d]+s[~d:~d]+a[~d:~d]+l[~d:~d]+t[~d:~d]+m[~d:~d]+veS[~d:~d]+veC[~d:~d]+h3S[~d:~d]+h3C[~d:~d]+s9S[~d:~d]+s9C[~d:~d]+diS[~d:~d]+diC[~d:~d]+t1S[~d:~d]+t1C[~d:~d]+j1S[~d:~d]+j1C[~d:~d]+h7S[~d:~d]+h7C[~d:~d]+j2S[~d:~d]+j2C[~d:~d]+d3S[~d:~d]+d3C[~d:~d]+t2S[~d:~d]+t2C[~d:~d]+h4S[~d:~d]+h4C[~d:~d]+h5S[~d:~d]+h5C[~d:~d]+h6S[~d:~d]+h6C[~d:~d]+h2S[~d:~d]+h2C[~d:~d]+h1S[~d:~d]+h1C[~d:~d]+q3S[~d:~d]+q3C[~d:~d]+loS[~d:~d]+loC[~d:~d]+v4S[~d:~d]+v4C[~d:~d]+s3S[~d:~d]+s3C[~d:~d]+bsc[~d:~d]+bcm[~d:~d]+dyn[~d:~d]', % +y1[~d:~d]',
	  [O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B]),
   fitxy <- lm(Eq),


   %   Add the variables here !!! don't forget
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
   A1 <- 'as.double(fitxy$coefficients[22])',
   B1 <- 'as.double(fitxy$coefficients[23])',
   C1  <- 'as.double(fitxy$coefficients[24])',
   D1 <- 'as.double(fitxy$coefficients[25])',
   E1 <- 'as.double(fitxy$coefficients[26])',
   F1 <- 'as.double(fitxy$coefficients[27])',
   G1 <- 'as.double(fitxy$coefficients[28])',
   H1 <- 'as.double(fitxy$coefficients[29])',
   I1 <- 'as.double(fitxy$coefficients[30])',
   J1 <- 'as.double(fitxy$coefficients[31])',
   K1 <- 'as.double(fitxy$coefficients[32])',
   L1 <- 'as.double(fitxy$coefficients[33])',
   M1 <- 'as.double(fitxy$coefficients[34])',
   N1 <- 'as.double(fitxy$coefficients[35])',
   O1 <- 'as.double(fitxy$coefficients[36])',
   P1 <- 'as.double(fitxy$coefficients[37])',
   Q1 <- 'as.double(fitxy$coefficients[38])',
   R1 <- 'as.double(fitxy$coefficients[39])',
   S1 <- 'as.double(fitxy$coefficients[40])',
   T1 <- 'as.double(fitxy$coefficients[41])',
   U1 <- 'as.double(fitxy$coefficients[42])',
   V1 <- 'as.double(fitxy$coefficients[43])',
   W1 <- 'as.double(fitxy$coefficients[44])',
   X1 <- 'as.double(fitxy$coefficients[45])',
   J <- 'as.double(fitxy$coefficients[46])',
   K <- 'as.double(fitxy$coefficients[47])',
   DY <- 'as.double(fitxy$coefficients[48])',
   % LM <- 'as.double(fitxy$coefficients[48])',
   summary <- summary(fitxy),
   r_print(summary),
   R2 <- 'as.double(summary$r.squared)',
   r_close, !.



% scale(_, lin, cross, 'factor a', '#', 'factor b', '#', false) :- !.
scale(_, lin, map, 'Model Temperature', 'C', 'Real Temperature', 'C', false) :- !.
scale(_, lin, correlate, 'TCR*ln(CO2)/ln(2)', 'C', 'Temperature', 'C', false) :- !.
scale(true, log, residual, 'Wavenumber', '2048/Month', 'Power Spectral', 'density', false) :- !.
scale(_, lin, model, 'Time', 'year', 'Temperature', 'C', true) :- !.
scale(_, lin, _, 'Time', 'year', 'Temperature', 'C', false).

single_filter(In, Out) :-
    % median_filter(In, In0),
    Out window In*13.
triple_filter(In, Out) :-
    median_filter(In, In0),
    A window In0*13,
    B window A*9, % 9
    Out window B*7. % 7

temperature_series(_, true, DataSet, T) :-
    dataset(DataSet, T0),
    triple_filter(T0, T).
temperature_series(true, false, DataSet, T) :-
    dataset(DataSet, T0),
    single_filter(T0, T).
temperature_series(false, false, DataSet, T) :-
    dataset(DataSet, T0),
    median_filter(T0, T).


temperature_series(_, Window, Triple, DataSet, T, Offset) :-
   temperature_series(Window, Triple, DataSet, TT),
   T = TT,
   Offset mapdot 0 .* TT
   .


get_years_from_1880(T, Years, Zeros) :-
    length(T, L),
    H range [1, L]/1,
    Y mapdot H ./ 12.0,
    Years mapdot 1880 .+ Y,
    Zeros mapdot 0 .* H.

get_months_from_start(T, H) :-
    length(T, L),
    H range [1, L]/1.



check_coefficients([], List, Final ) :- reverse(List, Final), !.
check_coefficients([F|Rest], List, Final ) :-
    F = 'NA', !,
    check_coefficients(Rest, [0.0|List], Final).
check_coefficients([F|Rest], List, Final ) :-
    check_coefficients(Rest, [F|List], Final).


html_rms(RMS, _, _) --> {var(RMS)}.
html_rms(RMS, Periodic, Equation) -->
	html(
	    [
	    p(Equation),
	    \(con_text:multi_columns([
		     div([
			 p(i('Temperature variance as RMS values in milliKelvin, baseline=1960')),
			 \(con_text:table_multiple_entries([[factor,rms]],RMS))
			 % \(con_text:paragraphs(RMS))
		       ]),
		     div([
			 p(i('Individual periodic cycles in years')),
			 \(con_text:table_multiple_entries([[period,rms]],Periodic))
			 % \(con_text:paragraphs(Periodic))
			])
			       ]
				     )
	      )
	    ]
	    ).    % .



plot(Request) :-
    garbage_collect,
    http_parameters(Request, [kind(Kind, []),
			      fft(FFT, [boolean, default(false)]),
			      window(Window, [boolean, default(false)]),
			      triple(Triple, [boolean, default(false)]),
			      wave(WL, [integer, default(0)]),
			      t_units(Cal, []),
			      startYear(StartYear, [integer]),
			      fitYear(FitYear, [integer]),
			      lag(LagCal, [float]),
                              dataset(DataSet, []),
                              evaluate(Characteristic, [default(model)])]),

    scale(FFT, LogLin, Characteristic, XLabel, XUnits, YLabel, YUnits, Show_Error_Bars),
    scaling(Cal, month, Scale),
    Lag is Scale*LagCal,

    % Get the temperature series
    temperature_series(_, Window, Triple, DataSet, T, Correction),

    get_years_from_1880(T, Year, Zeros),


    get_months_from_start(T, Months),

    Q is 1 , % 0.9333,

    Hale=21.98,
    Add =1,
    Others=1,

    % Scafetta
    Period is Hale/3 * 12 *Q,      % precession cycle with the time for Spring tides to realign with the same day each year
    Period2 is 9.015 * 12 *Q,   % 9.015 Sun-Moon-Earth tidal configuration 8.715
    Period3 is 18.613 * 12 *Q,  % Lunar precessional
    Period4 is Add*8.848 * 12 *Q,   % Lunar apsidal precession
    Period5 is Add*11.86* 12 *Q,   % 11.86 Tidal sidereal period of Jupiter
    Period6 is Others*Hale/7 * 1.026* 12 *Q,       % Soros cycle tide -------
    Period7 is Add*Period5/2,       % Period5/2 24*Q,
    Period8 is Period3/3,       % 20.5
    Period9 is 8.848 * 12 *Q/2,
    PeriodA is Add*Hale/4 *0.955*12 *Q,
    PeriodB is Others*Hale/5 *0.955*12 *Q, % -------
    PeriodC is Hale/6 *12 *Q,
    PeriodD is Add*Hale/2 *12 *Q,
    PeriodE is Add*Hale/1 *12 *Q,
    PeriodF is Others*3.35*12 *Q, % 3.344 Random_F*12*20, % 2*12 *Q, % ------
    PeriodG is 9.015 * 12 *Q*3,  % 27
    PeriodH is Add*7.944*12*Q, % 7.944 2.54  Venus 9.315*12*Q,  %
    PeriodI is Add*PeriodH/4,  % 1.986
    PeriodJ is 9.015 * 12 *Q/3,

    %X Sin mapdot yearly_sin_period(Period,1,WL) ~> Months,
    r2s(Sin),
    %X Cos mapdot yearly_cos_period(Period,1,WL) ~> Months,
    r2c(Cos),
    %X Sin2 mapdot yearly_sin_period(Period2,2,WL) ~> Months,
    r15s(Sin2),
    %X Cos2 mapdot yearly_cos_period(Period2,2,WL) ~> Months,
    r15c(Cos2),
    Sin3 mapdot yearly_sin_period(Period3,8 ,WL) ~> Months,
    Cos3 mapdot yearly_cos_period(Period3,8 ,WL) ~> Months,
    Sin4 mapdot yearly_sin_period(Period4,6,WL) ~> Months,
    % r27s(Sin4),
    Cos4 mapdot yearly_cos_period(Period4,6,WL) ~> Months,
    % r27c(Cos4),
    Sin5 mapdot yearly_sin_period(Period5,3,WL) ~> Months,
    % r18s(Sin5),
    Cos5 mapdot yearly_cos_period(Period5,3,WL) ~> Months,
    % r18c(Cos5),
    Sin6 mapdot yearly_sin_period(Period6,14,WL) ~> Months,
    % r11s(Sin6),
    Cos6 mapdot yearly_cos_period(Period6,14,WL) ~> Months,
    % r11c(Cos6),
    Sin7 mapdot yearly_sin_period(Period7,20,WL) ~> Months,
    % TSI_I mapdot planck_hale(20.05,23.62,-1.25,-1.05) ~> Months,
    % Sin7 mapdot planck_hale(21.8,22.2,-1.45,-1.25) ~> Months,
    % Sin7 lag TSI_I / TL,

    Cos7 mapdot yearly_cos_period(Period7,20,WL) ~> Months,
    Sin8 mapdot yearly_sin_period(Period8,7 ,WL) ~> Months,
    Cos8 mapdot yearly_cos_period(Period8,7 ,WL) ~> Months,
    Sin9 mapdot yearly_sin_period(Period9,9 ,WL) ~> Months,
    Cos9 mapdot yearly_cos_period(Period9,9 ,WL) ~> Months,
    SinA mapdot yearly_sin_period(PeriodA,13,WL) ~> Months,
    CosA mapdot yearly_cos_period(PeriodA,13,WL) ~> Months,
    SinB mapdot yearly_sin_period(PeriodB,12 ,WL) ~> Months,
    CosB mapdot yearly_cos_period(PeriodB,12 ,WL) ~> Months,
    SinC mapdot yearly_sin_period(PeriodC,11 ,WL) ~> Months,
    CosC mapdot yearly_cos_period(PeriodC,11 ,WL) ~> Months,
    SinD mapdot yearly_sin_period(PeriodD,17,WL) ~> Months,
    CosD mapdot yearly_cos_period(PeriodD,17,WL) ~> Months,
    SinE mapdot yearly_sin_period(PeriodE,19,WL) ~> Months,
    CosE mapdot yearly_cos_period(PeriodE,19,WL) ~> Months,
    % CosE mapdot comb ~> Months,
    SinF mapdot yearly_sin_period(PeriodF,10 ,WL) ~> Months,
    CosF mapdot yearly_cos_period(PeriodF,10 ,WL) ~> Months,
    %X SinG mapdot yearly_sin_period(PeriodG,5,WL) ~> Months,
    r7s(SinG),
    %X CosG mapdot yearly_cos_period(PeriodG,5,WL) ~> Months,
    r7c(CosG),
    SinH mapdot yearly_sin_period(PeriodH,15,WL) ~> Months,
    CosH mapdot yearly_cos_period(PeriodH,15,WL) ~> Months,

    Dynamo = Zeros,

    SinI mapdot yearly_sin_period(PeriodI,18,WL) ~> Months,
    CosI mapdot yearly_cos_period(PeriodI,18,WL) ~> Months,
    SinJ mapdot yearly_sin_period(PeriodJ,16,WL) ~> Months,
    CosJ mapdot yearly_cos_period(PeriodJ,16,WL) ~> Months,
    r3c(SCMSS),
    r3s(CMSS),

    r5s(LOD_F),
    r6c(S2),
    r4c(AAM),
    r5c(V1),
    r4s(TSI_F),
    r6s(LogCO2),

    get_fit(StartYear, FitYear, [T, LogCO2, S2, TSI_F, V1, LOD_F, AAM, SinH, CosH, % Arctic, NAO,
	                                                        Sin,  Cos,  Sin2, Cos2,
	                                                        Sin3, Cos3, Sin4, Cos4,
								Sin5, Cos5, Sin6, Cos6,
								Sin7, Cos7, Sin8, Cos8,
	                                                        Sin9, Cos9, SinA, CosA,
	                                                        SinB, CosB, SinC, CosC,
	                                                        SinD, CosD, SinE, CosE,
	                                                        SinF, CosF, SinG, CosG,
	                                                        SinI, CosI, SinJ, CosJ,
	                                                        SCMSS, CMSS, Dynamo],
	    Coefficients, Int, _R2C),

    check_coefficients(Coefficients, [], [C, SO, TS, VC,   LO, AA, SWH, CWH, % ARC, NI,
					                                    SW,	 CW,  SW2, CW2,
					                                    SW3, CW3, SW4, CW4,
					                                    SW5, CW5, SW6, CW6,
					                                    SW7, CW7, SW8, CW8,
					                                    SW9, CW9, SWA, CWA,
					                                    SWB, CWB, SWC, CWC,
					                                    SWD, CWD, SWE, CWE,
					                                    SWF, CWF, SWG, CWG,
									    SWI, CWI, SWJ, CWJ,
					                                    SC, CM, DY]),

    Fluct mapdot SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F + AA .* AAM +
                                           + SWH .* SinH + CWH .* CosH  % ARC .* Arctic + NI .* NAO
		                           + SW .* Sin + CW .* Cos
		                           + SW2 .* Sin2 + CW2 .* Cos2
		                           + SW3 .* Sin3 + CW3 .* Cos3
		                           + SW4 .* Sin4 + CW4 .* Cos4
		                           + SW5 .* Sin5 + CW5 .* Cos5
		                           + SW6 .* Sin6 + CW6 .* Cos6
		                           + SW7 .* Sin7 + CW7 .* Cos7
		                           + SW8 .* Sin8 + CW8 .* Cos8
		                           + SW9 .* Sin9 + CW9 .* Cos9
		                           + SWA .* SinA + CWA .* CosA
		                           + SWB .* SinB + CWB .* CosB
		                           + SWC .* SinC + CWC .* CosC
		                           + SWD .* SinD + CWD .* CosD
		                           + SWE .* SinE + CWE .* CosE
		                           + SWF .* SinF + CWF .* CosF
		                           + SWG .* SinG + CWG .* CosG
		                           + SWI .* SinI + CWI .* CosI
		                           + SWJ .* SinJ + CWJ .* CosJ
                                           + SC .* SCMSS + CM .* CMSS
					   + DY .* Dynamo,

    T_CO2_R mapdot C .* LogCO2 + Fluct,
    T_R mapdot Int .+ T_CO2_R,
    T_Diff mapdot T - T_R,
    !,
    sum_of_squares(T, T_R, Sum_of_Sq),
    length(Months, Length_Series),
    Error_Bar is 2*sqrt(Sum_of_Sq/Length_Series),

    T_lag lag T / Lag,
    T_R_lag lag T_R / Lag,
    corrcoeff(T_lag, T_R_lag, R2C2),
    (
       Characteristic = model ->
         T_lag_low mapdot T_lag .- Error_Bar,
         T_lag_high mapdot Error_Bar .+ T_lag,
         T_lags tuple T_lag_low + T_lag + T_lag_high,
         T_R_lags tuple T_R_lag + T_R_lag + T_R_lag,
         Corrections tuple Correction + Correction + Correction,
	 Data group Year + T_lags + T_R_lags + Corrections,
         Header = [XLabel, DataSet, model, correction]

     ;

       Characteristic = residual ->
         (   FFT ->
	     R_FFT fft T_Diff,
	     Range ordinal R_FFT,
	     Data tuple Range + R_FFT
	 ;
	     Data tuple Year + T_Diff   %  + Noise_Level
	 ),

         Header = [XLabel, residual, qbo] % , fluctuation]

    ),
    temp_data(NameData, DataSet),
    PPP is Period7/12,
    print(user_error, ['random period ', PPP]),

    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th('R=cc'), th('ln(co2)'),th(soi),th('a(volc)'),
				th(lod),th(tsi), th(aam), th(bary) % , th(amo), th(arctic)
			       ]),
			    tr([td(b('~5f'-R2C2)),td('~3f'-C),td('~3f'-SO),td('~3f'-VC),
				td('~3f'-LO),td('~3f'-TS), td('~5f'-AA), td('~5f'-SC)
			        % ,('~5f'-NI), td('~5f'-ARC)
			       ])
			   ]),
		     br([]),

		     table( % [class('fixed')],
			   [caption(div([id('legend')],[])),
			    tr([
				td(
		     \(context_graphing:dygraph_error_bars(LogLin, Header,
						       [XLabel,XUnits], [YLabel, YUnits],
						       [NameData, FitYear, ' - CSALT:', Characteristic], Data, Show_Error_Bars))
				  )
			       ])])
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







