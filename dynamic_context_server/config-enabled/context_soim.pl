:- module(context_soim, [
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_soim:navigate).
:- context:register(context_soim:plot).

soi_data('SOI', soi).
soi_data('Darwin', darwin).
soi_data('Tahiti', tahiti).
soi_data('OU Random Walk', ou_rw).
soi_data('Tahiti-Darwin', combined).

dataset(1, soi, L) :-
    soi(L0),
    L unbias L0.
dataset(2, darwin, L) :-
    darwin(L0),
    L unbias L0.
dataset(1, tahiti, L) :-
    tahiti(L0),
    L unbias L0.


dataset(1, ou_rw, RW) :-
    Range range [1,1605]/1,
    context_random_walk:ou_random_walker(0.005,1,0.1,Range,RW),!.


navigate(Request) :-
   collect_unit_options(calendar, Calendar),
   con_text:collect_options(context_soim:soi_data, DataSet),

   reply_html_page(cliopatria(default),
                   [title('SOI Model response'),
		    script([type('text/javascript'),src('/html/js/submit.js')], [])
		   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('SOI Model'),
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
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(triple, 'true', 'Pratt filter')),
			  br([]),
			  input([type('text'),
				 size(2),
				 name('lag'),
				 value('0')]),
			  select([name('t_units')], Calendar)
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

temperature_series(N, _, true, DataSet, T) :-
    dataset(N, DataSet, T0),
    triple_filter(T0, T).
temperature_series(N, true, false, DataSet, T) :-
    dataset(N, DataSet, T0),
    single_filter(T0, T).
temperature_series(N, false, false, DataSet, T) :-
    dataset(N, DataSet, T0),
    median_filter(T0, T).


temperature_series(N, Window, Triple, DataSet, T, Offset) :-
   temperature_series(N, Window, Triple, DataSet, TT),
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


gen_model(N, Window,Triple,DataSet,Year,Months,StartYear,FitYear,Int,Fluct,T) :-
    temperature_series(N, Window, Triple, DataSet, T, _Correction),

    get_years_from_1880(T, Year, Zeros),
    get_months_from_start(T, Months),

    % Q is 1 , % 0.9333,

    r2s(Sin),
    r2c(Cos),
    r15s(Sin2),
    r15c(Cos2),
    Sin3  = Zeros,
    Cos3 = Zeros,
    Sin4 = Zeros,
    Cos4 = Zeros,
    % r27c(Cos4),
    Sin5 = Zeros,
    % r18s(Sin5),
    Cos5 = Zeros,
    % r18c(Cos5),
    Sin6 = Zeros,
    % r11s(Sin6),
    Cos6 = Zeros,
    % r11c(Cos6),
    Sin7 = Zeros,

    Cos7 =Zeros,
    Sin8 =Zeros,
    Cos8 =Zeros,
    Sin9 =Zeros,
    Cos9 =Zeros,
    SinA =Zeros,
    CosA =Zeros,
    SinB =Zeros,
    CosB =Zeros,
    SinC =Zeros,
    CosC =Zeros,
    SinD =Zeros,
    CosD =Zeros,
    SinE =Zeros,
    CosE =Zeros,
    SinF =Zeros,
    CosF = Zeros,
    r7s(SinG),
    r7c(CosG),
    SinH =Zeros,
    CosH = Zeros,

    Dynamo = Zeros,

    SinI = Zeros,
    CosI = Zeros,
    SinJ = Zeros,
    CosJ = Zeros,
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

    Fluct0 mapdot SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F + AA .* AAM +
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
    Fluct mapdot C .* LogCO2 + Fluct0
    .



plot(Request) :-
    garbage_collect,
    http_parameters(Request, [kind(Kind, []),
			      fft(FFT, [boolean, default(false)]),
			      window(Window, [boolean, default(false)]),
			      triple(Triple, [boolean, default(false)]),
			      wave(_WL, [integer, default(0)]),
			      t_units(Cal, []),
			      startYear(StartYear, [integer]),
			      fitYear(FitYear, [integer]),
			      lag(_LagCal, [float]),
                              dataset(DataSet, []),
                              evaluate(Characteristic, [default(model)])]),

    scale(FFT, LogLin, Characteristic, XLabel, XUnits, YLabel, YUnits, Show_Error_Bars),
    scaling(Cal, month, _Scale),
    % Lag is Scale*LagCal,

    ( DataSet = combined ->
      temperature_series(_, Window, Triple, soi, T, _Correction),
      gen_model(1, Window,Triple,tahiti,Year,Months,StartYear,FitYear,Int1,Fluct1,_T1),
      gen_model(2, Window,Triple,darwin,Year,Months,StartYear,FitYear,Int2,Fluct2,_T2),
      Int is Int1 - Int2,
      Fluct mapdot Fluct1 - Fluct2
      % T mapdot T1 - T2
    ;
      gen_model(_, Window,Triple,DataSet,Year,Months,StartYear,FitYear,Int,Fluct,T)
    ),
    T_R mapdot Int .+ Fluct,
    T_Diff mapdot T - T_R,
    !,
    sum_of_squares(T, T_R, Sum_of_Sq),
    length(Months, Length_Series),
    Error_Bar is 2*sqrt(Sum_of_Sq/Length_Series),

    corrcoeff(T, T_R, R2C2),
    (
       Characteristic = model ->
         T_low mapdot T .- Error_Bar,
         T_high mapdot Error_Bar .+ T,
         Ts tuple T_low + T + T_high,
         T_Rs tuple T_R + T_R + T_R,
	 Data group Year + Ts + T_Rs,
         Header = [XLabel, DataSet, model]
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

    ),
    soi_data(NameData, DataSet),

    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th('R=cc')]),
			    tr([td(b('~5f'-R2C2))])
			   ]),
		     br([]),

		     table( % [class('fixed')],
			   [caption(div([id('legend')],[])),
			    tr([
				td(
		     \(context_graphing:dygraph_error_bars(LogLin, Header,
						       [XLabel,XUnits], [YLabel, YUnits],
						       [NameData, FitYear, ' - SOIM:', Characteristic], Data, Show_Error_Bars))
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







