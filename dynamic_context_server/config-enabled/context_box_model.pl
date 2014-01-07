:- module(context_box_model, [
			      emulated_gt/4,
			      log_scaler/2,
			      delayed_exp_model/6
			    ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_box_model:navigate).
:- context:register(context_box_model:plot).

navigate(Request) :-
   collect_unit_options(time, Tunits),

   reply_html_page(cliopatria(default),
                   [title('Simple box model response')],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('Box Model with SOI'),
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
					 [['Power Law', 'power'],
                                          ['Residual', 'residual'],
                                          ['SOI model', 'model'],
                                          ['SOI only', 'soi'],
                                          ['SOI FFT', 'fft'],
					  ['dump data', all],
					  ['OU model FFT', rw_fft],
					  ['OU model RW', rw],
					  ['OU model AC', rw_ac],
					  ['SOI cross-spectrum', ac_fft],
					  ['SOI cross-correlate', ac],
					  ['SOI auto-correlate', ac_soi],
					  ['SOI PSD', ac_soi_fft],
					  ['Volcanos', volcano],
					  ['components', comp],
					  ['co2', co2]
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

emulated_gt(Scale,Exp,X,Y) :-
   Y is Scale*X^Exp.

log_scaler(0,0) :- !.
log_scaler(X,Y) :-
   log(X,V),
   log(10,Ten),
   Y is V/Ten.

delayed_exp_model(Scale,BG,A,B,X,Y) :-
   Y is Scale*A*A/((X-A*sin(X/B))^2 + A*A*(1+cos(X/B))^2) + BG.

/*
volcanos([
          [48,22], % 22 36
          [276,4], % 264
          [396,11], % 384
	  % [552,11], % 540
          [1006,21], % 996
          [1164,4.7], % 1152
          [1236,14],  % 1224
          [1344,30]   % 1332
	 ]).
*/
/*
volcanos([
          [36, 22],    % 1883    4  36     1884 3.332  35.5
          [72, 2],    % 1886               1887 0.85  9
          % [120,9.5],    % 1892           1890 0.893 9.5
          [144, 5.1], %			   1892 0.48  5.1
          % [192, 4.3] %                   1896 0.4   4.3
          [264,3.8],    % 1902   264       1903 1.654 17.6
          [396,11],   % 1913   384c	   1913	0.538 5.7
          % [480, 2],  %                   1920 0.2   2
	  [552,11], % 1926   540           1929 0.207 2.2
          % [624, 1.8],                    1932 0.169 1.8
          [1006,21],  % 1963   996         1964 1.661 17.7
          [1080,2],   % 1970               1969 0.783 8.3
          [1152,5],   % 1976   1152        1975 0.682 7.3
          [1180,2],   % 1980   1152        1978 0.193 1.6
          [1224,14],  % 1982   1224	   1983 1.744 18.6
          [1344,30]   % 1992 1332          1992 2.822 30 *
	 ]).
*/

volcanos([
          [42, 12],   % 1883    4  36      1884 3.33  35.5
          [126, 4], % 1892               1890 0.893 9.5
          [150, 4], %			   1892 0.48  5.1
          [202, 6], %                    1896 0.4   4.3
          [274, 18],% 1902   264         1903 1.654 17.6
          [327, 20],
          [390, 8], % 1913   384c	   1913	0.538 5.7
          % [396, 6],
          [628, 6],
          [901,4],
          [1006,22],% 1963   996         1964 1.661 17.7
          % [1150,2], % 1976   1152        1975 0.682 7.3
          % [1204,10],
          [1228,24],% 1982   1224	   1983 1.744 18.6
          [1340,30]   % 1992 1332          1992 2.822 30 *
	 ]).

volcano_data([
          [krakatoa, 1883,8, 42, 12, 6],   % 22 1883    4  36      1884 3.33  35.5
          [colima,   1890,6, 126, 4, 4], % 9.5 1892               1890 0.893 9.5
          [calbuco,  1893,1, 144, 4, 4], % 5.1		     1892 0.48  5.1
          % [mayon,    1897,6, 210, 4.3], %                    1896 0.4   4.3
         [santamaria,1902,10,274, 18, 6],% 1902   264         1903 1.654 17.6
          [ksudach,  1907,5, 324, 20, 5],   % guess
          [novarupta,1912,6, 390, 8, 6], % 5.7 1913   384         1913 0.538 5.7
          % [colima,   1913, 0, 396, 5],
          [cerroazul,1932,4, 624, 6, 5],  % 4
          [bezymianny,1955,1, 901, 4, 5],
          [agung,    1963,2, 998,22, 5],%17.7  1963   996         1964 1.661 17.7
          % [augustine,1976,0, 1152,2], % 7.3 1976   1152        1975 0.682 7.3
          [elchichon,1982,4, 1228,24, 5],% 18.6 1982   1224	   1983 1.744 18.6
          [pinatubo, 1991,6, 1338,30, 6]   % 1992 1332          1992 2.822 30 *
	 ]).


scale(ac_soi_fft, log).
scale(ac_fft, log).
scale(rw_fft, log).
scale(fft, log).
scale(_, lin).

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


lod_get(Years, LOD_F) :-
    lod(LOD),
    interpolate(Years, LOD, LOD_I),
    LOD_U unbias LOD_I,
    expsm(LOD_U, 0.985, LOD_S),
    LOD_Scale is -0.05*0.708,
    %
    % LOD_Scale is -0.05 * 0.22,
    % LOD_S mapdot LOD_U * LOD_U * LOD_U,
    %
    LOD_F mapdot LOD_Scale .* LOD_S.

plot(Request) :-
    http_parameters(Request, [kind(Kind, []),
			      limit(_Limit, [number]),
                              t_units(_TUnits, []),
                              evaluate(Characteristic, [])]),

    SS = 1.0,
    % SOI_S is 0.0637*SS,
    SOI_S is 0.0731*SS,
    AMO_S is 0.000026*SS,
    Trend is 2.26e-10*SS,
    V_S is 0.42*SS,  % 0.42
    V_lag = 0.97, % 0.98
    TSI_Scale is 0.0541,     % 0.3

    tsi(TSI),
    % best(T),
    giss(T),
    soi(SOI),
    %
    /*
    tahiti(Tahiti),
    SOI1 shrink Tahiti/SOI_s,
    darwin(Darwin),
    SOI2 shrink Darwin/SOI_s,
    SOI mapdot SOI2 - SOI1,
    */
    %
    amo(AMO),
    volcanos(V),
    volcano_number(W),
    S0 unbias SOI,
    S1 mapdot SOI_S .* S0,  % scale SOI
    % S1 mapdot S0^2,  % scale SOI
    expsm(S1, 0.84, S2),    % smooth SOI
    A0 unbias AMO,
    A1 mapdot AMO_S .* A0,
    expsm(A1, 0.86, A2),
    Offset = 0.21,  % 0.214 0.2053,
    S mapdot Offset .+ S2,
    length(T, L),
    H range [1, L]/1,
    BL mapdot emulated_gt(Trend,3) ~> H, % 235
    % BL mapdot emulated_gt(362e-12,2.942) ~> H, % 235
    %BL mapdot emulated_gt(3.9e-7,2) ~> H, % 235
    % BL mapdot emulated_gt(95e-10) ~> H,
    Mod0 mapdot BL - S,
    Zeros mapdot 0 .* H,
    sparse_list(Zeros, V, Vol),
    V0 mapdot V_S .* Vol,  % 0.7205
    W0 mapdot 0.0 .* W,  % 0.15
    expsm(V0, V_lag, V1),   % 0.98
    expsm(W0, 0.978, W1),
    % W2 shrink W1/V1,
    %length(V1, LV),
    %length(W1, LW),
    %print(user_error, [LV,LW]),
    VV mapdot V1 + W1,
    Mod1 mapdot Mod0 - VV,
    X = 'Time',
    Y1 = 'Temperature',
    Y2 = 'SOI',
    XUnits = 'year',
    YUnits = 'C',
    Y mapdot H ./ 12,
    Year mapdot 1880 .+ Y,
    Year1 mapdot 1880.5 .+ Y,
    % Year2 mapdot 1878.5 .+ Y,
    interpolate(Year1, TSI, TSI_I),
    TSI_U unbias TSI_I,
    % expsm(TSI_U, 0.99, TSI_L),
    TSI_F mapdot TSI_Scale .* TSI_U,
    ModS mapdot Mod1 + TSI_F,
    ModA mapdot ModS + A2,

    lod_get(Year, LOD_F),
    Mod mapdot ModA + LOD_F,

    corrcoeff(T, Mod, RMS),
    scale(Characteristic, Log),
    (
       Characteristic = ac ->
         tahiti(Tahiti),
         darwin(Darwin),
         AC correlate Tahiti * Darwin,
         ACL ordinal AC,
         ACL0 mapdot 1 .+ ACL,
         ACN mapdot AC / ACL0,
         Data tuple ACL  + ACN,
         Header = [months, autocorrelate]
     ;
       Characteristic = ac_soi ->
         AC correlate SOI * SOI,
         ACL ordinal AC,
         ACL0 mapdot 1 .+ ACL,
         ACN mapdot AC / ACL0,
         Data tuple ACL  + ACN,
         Header = [months, autocorrelate]
     ;
       Characteristic = ac_fft ->
         tahiti(Tahiti),
         darwin(Darwin),
         AC correlate Tahiti * Darwin,
         ACL ordinal AC,
         ACL0 mapdot 1 .+ ACL,
         ACN mapdot AC / ACL0,
         FFT fft ACN,
         Range ordinal FFT,
         Data tuple Range + FFT,
         Header = [months, autocorrelate]
     ;
       Characteristic = ac_soi_fft ->
         AC correlate SOI * SOI,
         ACL ordinal AC,
         ACL0 mapdot 1 .+ ACL,
         ACN mapdot AC / ACL0,
         ACH shrink ACN/SOI,
         FFT fft ACH,
         Range ordinal FFT,
         Data tuple Range + FFT,
         Header = [months, autocorrelate]
     ;
       Characteristic = rw ->
	 uniform(18, Win),
         % Zrw0 mapdot 0.0 .* H,
         % semi_random_walker(H, 1.2, [6.0,12.0], [6.0,12.0], Zrw0, ZRW),
         context_random_walk:ou_random_walker(0.01, 1.0, 0.25, H, ZRW),
         ZRWD window ZRW/Win,
         % context_random_walk:ou_random_walker(0.0000001, 1.0, 0.35, H, ZRW2),
         % expsm(ZRW, 0.95, ZRWS),
         % ZRWD derivative ZRWS/1,
         Range ordinal ZRWD,
         Data tuple Range + ZRWD,
         Header = [X, rw, model]
     ;
       Characteristic = rw_ac ->
	 uniform(18, Win),
         % Zrw0 mapdot 0.0 .* H,
         % semi_random_walker(H, 1.2, [6.0,12.0], [6.0,12.0], Zrw0, ZRW),
         context_random_walk:ou_random_walker(0.01, 1.0, 0.25, H, ZRW),
         ZRWD window ZRW/Win,
         % expsm(ZRW, 0.95, ZRWS),
         % ZRWD derivative ZRWS/1,

         AC correlate ZRWD * ZRWD,
         ACL ordinal AC,
         ACL0 mapdot 1 .+ ACL,
         ACN mapdot AC / ACL0,

         Range ordinal ACN,
         Data tuple Range + ACN,
         Header = [X, rw, model]
    ;
       Characteristic = rw_fft ->
	 uniform(18, Win),
         % context_random_walk:ou_random_walker(0.00000001, 1.0, 0.035, H, ZRW),
         % Zrw0 mapdot 0.0 .* H,
         % semi_random_walker(H, 1.2, [6.0,12.0], [6.0,12.0], Zrw0, ZRW),
         context_random_walk:ou_random_walker(0.01, 1.0, 0.25, H, ZRW),
         ZRWD window ZRW/Win,
         % ZRW mapdot ZRW1 - ZRW2,
         % context_random_walk:ou_random_walker(0.0000001, 1.0, 0.025, H, ZRW),
         % expsm(ZRW, 0.95, ZRWS),
         % ZRWD derivative ZRWS/1,
         % Range ordinal ZRWD,
         FFT fft ZRWD,
         Range ordinal FFT,
	 EModel mapdot delayed_exp_model(0.1,0.0,24,35) ~> Range,
         Data tuple Range + FFT + EModel,
         Header = [X, rw_fft, model]
    ;
         % expsm(Z, 0.95, S),
       Characteristic = all ->
         T_T mapdot T - TSI_F,
         Data tuple H + T_T + S2 + V1 + LOD_F,
         Header = [months, t, soi, v1, lod]
    ;
       Characteristic = comp ->
         Data tuple Year + BL + TSI_F + S2 + V1 + LOD_F,
         Header = [year, trend, tsi, soi, volc, lod]
    ;
       Characteristic = power ->
         Trend_BL mapdot  BL .- Offset,
         Data tuple Year + T + Mod + Trend_BL,
         Header = [X, Y1, Y2, trend]
     ;
       Characteristic = co2 ->
         context_co2:co2_knmi(CO2),
         interpolate(Year, CO2, CO2_I),
         % temperature = lod + 3.108*co2 - 17.9 - soi - 1.244*v
         LogCO2 mapdot 3.11 * ln ~> CO2_I,

         LOD_CO2 mapdot 1.0 .* LOD_F,
         SOI_CO2 mapdot -1.0 .* S2,
         V_CO2 mapdot -1.24 .* V1,
         % T_CO2 mapdot LogCO2 + LOD_CO2 + V_CO2 + SOI_CO2 + TSI_F ,
         % T_CO2_O mapdot T_CO2 .- 17.9,
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         get_fit(T, LogCO2, SOI_CO2, TSI_F, V_CO2, LOD_CO2,
		    C,      SO,	     TS,    VC,    LO, Int, _R2C),
         T_CO2_R mapdot C .* LogCO2 + SO .* SOI_CO2 + TS .* TSI_F + VC .* V_CO2 + LO .* LOD_CO2,
         T_R mapdot Int .+ T_CO2_R,
	 corrcoeff(T, T_R, R2C2),
         % Data tuple Year + T + T_CO2_O + LOD_CO2 + V_CO2 + SOI_CO2 + TSI_F,
	 T_Diff mapdot T - T_R,
	 _ModelCo2 mapdot Int .+  C .* LogCO2 + T_Diff,
	 Data tuple Year + T + T_R,
         %%% Data tuple Year + ModelCo2,
         % Data tuple Year + T_Diff + V_CO2,
         % Header = [X, temperature, model, lod, volc, soi, tsi]
         Header = [X, temperature, model, corrected]
     ;
       Characteristic = residual ->
         R mapdot T - Mod,
         % Initial mapdot T - BL,
         % I mapdot Offset .+ Initial,
         Data tuple Year + R, % + I,
         Header = [X, residual] % , Y2]
      ;
       Characteristic = model ->
         Smooth = 0.95,
	 expsm(T, Smooth, T1),
	 % uniform(3,Window),
	 % T1 window T*Window,
         R mapdot  S - A2 + VV - TSI_F - LOD_F,
	 expsm(R, Smooth, R1),
	 Residual mapdot R1 + T1,
         Data tuple Year + Residual + BL,
         Header = [X, Y2, trend]
      ;
       Characteristic = soi ->
         S3 mapdot -1 .* S2,
         Data tuple Year + S3 + TSI_F,
         Header = [X, Y2, tsi]
      ;
       Characteristic = volcano ->
         % Volcs mapdot 1 .* S2,
         Data tuple Year + V1 + W1,
         Header = [X, v1, w1]
      ;
       Characteristic = fft ->
         %darwin(Darwin),
	 %FFT fft Darwin,
         FFT fft SOI,
         Range ordinal FFT,
	 % R mapdot log_scaler ~> Range,
	 EModel mapdot delayed_exp_model(48,0.9,24,35) ~> Range,
         Data tuple Range + FFT + EModel,
         Header = [X, fft, model]
    ),
   (not(number(R2C2)) -> R2C2 = 0.0; true),
   (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     i('Corr Coeff time = '), b('~5f ' - RMS),
		     i('Corr Coeff CO2 = '),  b('~5f ' - R2C2),
		     \(context_graphing:dygraph_native(Log, Header,
						       [X,XUnits], [Y1, YUnits],
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

