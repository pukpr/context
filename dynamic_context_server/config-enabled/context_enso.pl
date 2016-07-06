:- module(context_enso, [
			       mod_sin_period/5,
			       mod_cos_period/5,
			       sin_biennial_period/2,
			       cos_biennial_period/2,
			       mathieu_mod/2,
			       yearly_mod/2
		      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_enso:navigate).
:- context:register(context_enso:plot).


% test_period(N,_,Period) :- Period is N*N/20*12+N*2.134.
test_period(_,P,P).
%diup(-0.16,-1.5,W) :- W is 2*pi/(17.8*12).
%yp(0.0,1.0).
% diup(0.0,0.0,0.0).
 diup(0.35,-2.8,W) :- W is 2*pi/(11.15*12).
% diup(0.0,-0.4,W) :- W is 2*pi/(16.0*12).
%diup(0.5,2.1,W) :- W is 2*pi/(18.6*12).
% yp(0.0,-2.5).
yp(0.0,-2.5).
yp(0.4,-2.5).
yp(0.4,1.0).
yp(0.4,-1.8).
% diup(-0.4,-3.0,W) :- W is 2*pi/(18.1*12).
% yp(0.11,1.0).


mod_sin_period(0.0,N,M,_,0) :- N =< M.
mod_sin_period(Period,N,M,X,Y) :-
	 N =< M, test_period(N,Period,P), Y is sin(2.0*pi*X/P).
	 % N =< M,diup(A,T,W), yp(AY,TY), test_period(N,Period,P), Y is sin(2*pi*1.000*X/P+A*sin(W*X+T)+AY*sin(2*pi/12*X+TY)).
mod_sin_period(_,N,M,_,0.0) :- N > M.
% mod_sin_period(Period,N,M,X,Y) :- P is 1.0*Period, Y is sin(2*pi*X/P).

mod_cos_period(0.0,N,M,_,0) :- N =< M.
mod_cos_period(Period,N,M,X,Y) :-
	N =< M, test_period(N,Period,P), Y is cos(2*pi*X/P).
	% N =< M,diup(A,T,W), yp(AY,TY), test_period(N,Period,P), Y is cos(2*pi*1.000*X/P+A*sin(W*X+T)+AY*sin(2*pi/12*X+TY)).
mod_cos_period(_,N,M,_,0.0) :- N > M.
% mod_cos_period(Period,N,M,X,Y) :- P is 1.0*Period, Y is cos(2*pi*X/P).


%biamp(X,Amp) :- Amp is (1-0.2*cos(2*pi/(10.1*12)*X - 0.7)).
%bimod(X,Mod) :- Mod is 0.7*cos(2*pi/(17.9*12)*X + 0.9).
biamp(X,Amp) :- Amp is (1-0.21*cos(2*pi/(10.09*12)*X - 0.66)).
bimod(X,Mod) :- Mod is 0.68*cos(2*pi/(17.9*12)*X + 0.9).
% biamp(_,1.0).
% bimod(_,0.0).
% sin_biennial_period(X,Y) :- X > 76*12, bimod(X,M), biamp(X,A), Y is A*sin(2*pi*X/24+M), !.
%
% sin_biennial_period(X,Y) :- bimod(X,M), biamp(X,A), Y is
% -A*sin(2*pi*X/24+M).

%  cos_biennial_period(X,Y) :- X > 76*12, bimod(X,M), biamp(X,A), Y is A*cos(2*pi*X/24+M), !.
%
% cos_biennial_period(X,Y) :- bimod(X,M), biamp(X,A), Y is
% -A*cos(2*pi*X/24+M+0.0).

sin_biennial_period(X,Y) :- T is X/12 + 15.7,
  Y is 39.3019083159901*sin(0.180278 + 0.1557116*sin(3.9699*T))*
  sin(2.2408*sin(0.34205 + 2.39805*sin(3.10522*sin(3.04172*sin(2.30595*sin(2.31241*T)))))).
cos_biennial_period(X,Y) :- T is X/12 - 0,
  Y is 39.3019083159901*cos(0.180278 + 0.1557116*sin(3.9699*T))*
  sin(2.2408*sin(0.34205 + 2.39805*sin(3.10522*sin(3.04172*sin(2.30595*sin(2.31241*T)))))).



yearly_mod(X,Y) :- Y is 1.0 + 0*0.15*cos(2*pi*X/12+0.4).

% mathieu_mod(X,Y) :- Y is 5.0*cos(2*pi*X/10.201+2.4).
% mathieu_mod(X,Y) :- Y is -0.6*(1.0+0*0.3*cos(2*pi*X/9.3+0.2)).
% mathieu_mod(X,Y) :- Y is -1.0*(1.0 +0.3*cos(2*pi*X/8.3-1.8)).
% mathieu_mod(X,Y) :- Y is -0.7*(1.0+1.0*cos(2*pi*X/2.0+2.6)).
%  mathieu_mod(X,Y) :- Y is
% -0.7*(1.0+0*0.9*cos(2*pi*X/10.6+4.4)*cos(2*pi*X/2.33-2.4)).
mathieu_mod(X,Y) :- Y is -0.7*(1.0+1*0.4*cos(2*pi*X/2.33-2.2)).

/*yearly_sin_period(0.0,N,M,_,0) :- N =< M.
yearly_sin_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is sin(2*pi*X/P).
yearly_sin_period(_,N,M,_,0.0) :- N > M.
yearly_cos_period(0.0,N,M,_,0) :- N =< M.
yearly_cos_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is cos(2*pi*X/P).
yearly_cos_period(_,N,M,_,0.0) :- N > M.
*/

temp_data('SOI', soi).
temp_data('Darwin-Tahiti', dt).
temp_data('QBO', qbo).
temp_data('Tides', sydney).
temp_data('Tides Filtered', sydney2).
temp_data('Tides Biennial Flip', sydney_flip).
temp_data('nino3', nino3).
temp_data('NAO', nao).
temp_data('AMO', amo).
temp_data('GISS', giss).
temp_data('HADCRUT4', hadcrut).
temp_data('BESTLand', best).
temp_data('BEST', best_land_ocean).
temp_data('BEST ER', bester).
temp_data('GISS+HADCRUT4_CW+NOAA', global_combo).
temp_data('HADSST3', sst).
temp_data('CRUTEM4', land).
temp_data('GISSLand', gistemp_dts).
temp_data('BEST+CRUTEM4+GISSLand', land_combo).
temp_data('GISS+NRDC+NOAA', global_combo2).
temp_data('CRUTEM+2*HADSST', composed).
temp_data('NOAA LAND OCEAN', noaa_land_ocean).
temp_data('NOAA LAND', noaa_land).
temp_data('NOAA OCEAN', noaa_ocean).
temp_data('HADCRUT3', hadcrut3).
temp_data('HADSST2', sst2).
temp_data('NCDC', ncdc).
temp_data('HADCRUT4_CW', hadcrut4_cw).
temp_data('OU Random Walk', ou_rw).
temp_data('ER SST', er).

orbital_period('tide precession',  7.3,   'spring tides to realign with calendar date').
orbital_period('tidal cycle',      9.015, 'sun-moon-earth configuration induced oscillation').
orbital_period('lunar standstill', 18.6,  'nodal period when declination of the moon reaches max, Kola cycle').
orbital_period('lunar absidal',    8.85,  'when sidereal period exceeds anomalistic period by one month').
orbital_period('jupiter sidereal', 11.86, 'tidal sidereal period of jupiter').
orbital_period('tidal saros',      18.03, 'Saros cycle period of eclipses of the sun and moon').
orbital_period('lunar harmonic',   6.2,   'one third of the lunar standstill, Kola cycle').
orbital_period('absidal harmonic', 4.42,    'one half lunar absidal').
orbital_period('jupiter harmonic', 5.93,    'one half jupiter sidereal').
% extra

dataset(ou_rw, RW) :-
    Range range [1,1605]/1,
    % PL mapdot fake_power_law(1650.0,3) ~> Range,
    context_random_walk:ou_random_walker(0.005,1,0.1,Range,RW), !.
    % L mapdot PL + RW, !.

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
dataset(best, L) :- best(L). %best_land_update(L). %  best(L).
dataset(best_land_ocean, L) :- best_land_ocean(L).
dataset(sst, L) :- sst(L).
dataset(land, L) :- land(L).
dataset(gistemp_dts, L) :- gistemp_dts(L).
dataset(global_combo, L) :-
	dataset(giss,L1),
	dataset(hadcrut4_cw,L2),
	dataset(noaa_land_ocean,L3),
	L mapdot 0.3333 .* L1 + 0.3333 .* L2 + 0.3333 .* L3.
dataset(global_combo2, L) :-
	dataset(giss,L1),
	dataset(ncdc,L2),
	dataset(noaa_land_ocean,L3),
	L mapdot 0.3333 .* L1 + 0.3333 .* L2 + 0.3333 .* L3.
dataset(er, L) :-
	er_sst(L).
dataset(bester, L) :-
	% er_sst(L1),
	dataset(noaa_ocean, L1),
	dataset(best,L2),
	L mapdot 0.72 .* L1 + 0.28 .* L2.

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
dataset(ncdc, L) :- ncdc(L).
dataset(soi, L) :-
        soi(L0),
	% L1 mapdot abs ~> L0,
	% L2 mapdot sign ~> L0,
	% L3 mapdot sqrt ~> L1,
	% L4 mapdot L2 * L3,
	L unbias L0.
dataset(dt, L) :-
        darwin(D),
        tahiti(T),
	L0 mapdot 0.75 .* D -T,
	% L1 mapdot abs ~> L0,
	% L2 mapdot sign ~> L0,
	% L3 mapdot sqrt ~> L1,
	% L4 mapdot L2 * L3,
	L unbias L0.
dataset(qbo, L) :-
        qbo(L0),
	% L1 mapdot abs ~> L0,
	% L2 mapdot sign ~> L0,
	% L3 mapdot sqrt ~> L1,
	% L4 mapdot L2 * L3,
	L unbias L0.

dataset(sydney, L) :-
	sydney_tides2(L0),
	L1 unbias L0,
        F window L1*134,
        L mapdot L1 - F.

dataset(sydney2, L) :-
	sydney_tides2(L0),
	L1 unbias L0,
        F window L1*32,
        L mapdot L1 - F.

dataset(sydney_flip, L) :-
	sydney_tides2(L0),
	L1 unbias L0,
        F window L1*32,
        L2 mapdot L1 - F,
        Front range -1*[1,912],
        Back range 1*[913,1607],
        Mask cat [Front,Back],
	L mapdot L2 * Mask.


dataset(nino3, L) :-
        nino3(L0),
	L unbias L0.

dataset(nao, L) :-
        nao(L0),
	L unbias L0.

dataset(amo, L) :-
        amo(L0),
        F window L0*240,
        L1 mapdot L0 - F,
	L unbias L1.


navigate(Request) :-
   collect_unit_options(calendar, Calendar),
   con_text:collect_options(context_enso:temp_data, DataSet),
   findall([N,D,P], orbital_period(N,P,D), CycleData),

   reply_html_page(cliopatria(default),
                   [title('ENSO model response'),
		    script([type('text/javascript'),src('/html/js/submit.js')], [])
		    % , \(con_text:style)
		   ],
                   [\(con_text:table_with_iframe_target(
                                    Request,
		     [
                      h1('ENSO Model'),
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
				 value('2013')])
			     ]),

			  select([name('dataset')], DataSet),
			  % \(con_text:check_box(anthro, 'true', 'anthro aerosols')),
			  % \(con_text:check_box(volc, 'true', 'GISS aerosol model')),
			      input([type('text'),
				 size(4),
				 name('volc'),
				 value('1')]),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [
                                          ['Underlying CO2 signal', signal],
					  ['View the residual error', residual],
					  ['View staggered fluctuation components', all],
					  ['Match index with model', model],
					  ['Correlate CO2 with model', correlate],
					  ['Correlate index with model', map],
					  ['Match natural variability', fluctuation],
					  ['View composite cycles', cycles],
					  ['Dump all component factors', cross]
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
			  \(con_text:check_box(eq, 'true', 'CC on interval')),
			  \(con_text:check_box(sign, 'true', 'sign metric')),
			  \(con_text:check_box(fft, 'true', 'FFT of residual')),
			  br([]),
			  \(con_text:check_box(window, 'true', 'Apply 12 month window')),
			  \(con_text:check_box(triple, 'true', 'Pratt filter')),
                          \(con_text:check_box(wwii_adjust, 'true', 'Second Deriv')),
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
					\(con_text:input_cells([[co2_lag,-1,2],
						      [soi_lag,-6,2],
						      [aero_lag,-15,2],
						      [lod_lag,-48.0,2],
						      [tsi_lag,-6,2],
                                                      [aam_lag,-6,2],
                                                      [bary_lag,-6,2]
                                                      % [amo_win,-120,2],
						      % [arctic_win, -120,2]
						     ]))
				       ]
					)]),
				     p([' ...... ']),
				     div([style([ type('text/css'), scoped ],
					 '@import url("/html/css/context.css")'),
					  input([type('text'), size(2), name('wave'), value('30')]),
					  \(con_text:check_box(fit_res, 1, 'fit flip')),
					  \(con_text:check_box(high_res, 1, 'high res', checked))
				%	  \(con_text:table_multiple_entries(
				%		  [[cycle, 'period description', year]],
				%		  CycleData
				%				    ))
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

% check_for_nan('$NAN', 0.0) :- !.
% check_for_nan(V, V).


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
	                                                          SF,  CF

								  ],
	[             C,   S,   T,   A,    L,   M, Z, N, V, W, P, Q,
		                                E, F, G, H, D, I, R, U,
		                                A1, B1, C1, D1, E1, F1,
		                                G1, H1, I1, J1, K1, L1,
		                                M1, N1, O1, P1, Q1, R1], Int, R2) :-
   % r_open_session,
   y <- Temperature,
   c <- CO2,
   s <- SOI,
   a <- Volc,
   l <- LOD,
   t <- TSI,
   m <- AAM,
   msS <- Arctic,
   msC <- NAO,
   c7S <- Sin,
   c7C <- Cos,
   s9S <- S2,
   s9C <- C2,
   diS <- S3,
   diC <- C3,
   q4S <- S4,
   q4C <- C4,
   qmS <- S5,
   qmC <- C5,
   q1S <- S6,
   q1C <- C6,
   c5S <- S7,
   c5C <- C7,
   cwS <- S8,
   cwC <- C8,
   sdS <- S9,
   sdC <- C9,
   qsS <- SA,
   qsC <- CA,
   spS <- SB,
   spC <- CB,
   q2S <- SC,
   q2C <- CC,
   clS <- SD,
   clC <- CD,
   q3S <- SE,
   q3C <- CE,
   qbS <- SF,
   qbC <- CF,
   % pccr <- princomp('~y+c+s+a+l+t+m+z+j+k+n+v+w+p+q+e+f+g+h+d+i+r+u+a1+b1+c1+d1+e1+f1+g1+h1+i1+j1+k1+l1+m1+n1+o1+p1+q1+r1+s1+t1+u1+v1+w1+x1'),
   % r_print(summary(pccr)),
   % Test <- 'as.double(pccr$coefficients[1])',
   % print(user_error, Test),
   % r_print(loadings(pccr)),
   B is (EndDate - 1880)*12,
   O is (StartDate - 1880)*12 + 1,
   % O = 1, % 840,
   format(atom(Eq), 'y[~d:~d]~~c[~d:~d]+s[~d:~d]+a[~d:~d]+l[~d:~d]+t[~d:~d]+m[~d:~d]+msS[~d:~d]+msC[~d:~d]+c7S[~d:~d]+c7C[~d:~d]+s9S[~d:~d]+s9C[~d:~d]+diS[~d:~d]+diC[~d:~d]+q4S[~d:~d]+q4C[~d:~d]+qmS[~d:~d]+qmC[~d:~d]+q1S[~d:~d]+q1C[~d:~d]+c5S[~d:~d]+c5C[~d:~d]+cwS[~d:~d]+cwC[~d:~d]+sdS[~d:~d]+sdC[~d:~d]+qsS[~d:~d]+qsC[~d:~d]+spS[~d:~d]+spC[~d:~d]+q2S[~d:~d]+q2C[~d:~d]+clS[~d:~d]+clC[~d:~d]+q3S[~d:~d]+q3C[~d:~d]+qbS[~d:~d]+qbC[~d:~d]',
	  [O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B]),
   fitxy <- lm(Eq),


   %   Add the variables here !!! don't forget
   % r_print(fitxy),
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
/*
   S1 <- 'as.double(fitxy$coefficients[40])',
   T1 <- 'as.double(fitxy$coefficients[41])',
   U1 <- 'as.double(fitxy$coefficients[42])',
   V1 <- 'as.double(fitxy$coefficients[43])',
   W1 <- 'as.double(fitxy$coefficients[44])',
   X1 <- 'as.double(fitxy$coefficients[45])',
   J <- 'as.double(fitxy$coefficients[46])',
   K <- 'as.double(fitxy$coefficients[47])',
   DY <- 'as.double(fitxy$coefficients[48])',
   LM <- 'as.double(fitxy$coefficients[49])',
*/
   summary <- summary(fitxy),
   % r_print(summary),
   R2 <- 'as.double(summary$r.squared)',
   % r_close,
   !.


% scale(_, lin, cross, 'factor a', '#', 'factor b', '#', false) :- !.
scale(_, lin, map, 'Model Temperature', 'C', 'Real Temperature', 'C', false) :- !.
scale(_, lin, correlate, 'TCR*ln(CO2)/ln(2)', 'C', 'Temperature', 'C', false) :- !.
scale(true, log, residual, 'Wavenumber', '2048/Month', 'Power Spectral', 'density', false) :- !.
scale(_, lin, model, 'Time', 'year', 'Index', '#', false) :- !.
scale(_, lin, _, 'Time', 'year', 'Temperature', 'C', false).

single_filter(In, Out) :-
    % median_filter(In, In0),
    Out window In*13.
triple_filter(In, Out) :-
    median_filter(In, In0),
    A window In0*13, %13
    B window A*9, % 9
    Out window B*7. % 7

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

get_fit(1, -1.0).
get_fit(0, 1.0).

temperature_series(Second, Window, Triple, DataSet, T, Fit, Offset) :-
   temperature_series(Window, Triple, DataSet, TTT),
   get_fit(Fit, Flip),
   get_years_from_1880(TTT, Year, _),
   length(Year,L),
   TT1 derivative TTT / Year,
   TT2 derivative TT1 / Year,
   % TT2 derivative TTT / Year,
   TT3 offset TT2 - 1,
   TT4 cat [TT3, [0]],
   % Omega2 is 1.0,
   Omega2 mapdot -4.3*mathieu_mod ~> Year,
   Scaled mapdot Omega2 * TTT,
   Front range 1*[1,1188],
   Middle range Flip*[1189,1386],
   Back range 1*[1387,L],
   Clip cat [Front,Middle,Back],
       % Offset invert Clip,

   TT mapdot TT4 - Scaled,
   (  Second ->
      T mapdot TT * Clip
      % T = TT
   ;
      T mapdot TTT * Clip
      % T = TTT
   ),
   Offset mapdot 0 .* TT.


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

get_scmss(EQ_ON,Time, Lag, S_F) :-
    (	EQ_ON ->
        S_U mapdot scmss_fit ~> Time % Months
    ;
        % scmss(SCMSS),
        % interpolate(Time, SCMSS, S_I),
        bary_speed(S_I),
        S_U unbias S_I
    ),
    (	Lag >= 0.0 ->
        S_F delay S_U / Lag
    ;
        S_F mapdot 0 .* S_U
    ).
get_cmss(EQ_ON, Time, Lag, C_F) :-
    (	EQ_ON ->
        C_U mapdot cmss_fit ~> Time
    ;
        % cmss(CMSS),
        % interpolate(Time, CMSS, C_I),
        bary_distance(C_I),
        C_U unbias C_I
    ),
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
/*
get_soi(Lag, SOI_F) :-
    soim(SOIM),
    context_math:splitter(SOIM, [_,SOI]),
    S unbias SOI,
    (	Lag >= 0.0 ->
        SOI_F lag S / Lag
    ;
        SOI_F mapdot 0 .* S
    ).
*/

get_soi(Lag, SOI_F) :-
    soi(SOI), %_close
    % context_soim: soi_result(SOI),
    S unbias SOI,
    % Square
    % SU unbias S0,
    % S mapdot square_with_sign ~> SU,
    (	Lag >= 0.0 ->
        SOI_F lag S / Lag
    ;
        SOI_F mapdot 0 .* S
    ).

get_pdo(Lag, PDO_F) :-
    pdo(PDO),
    P unbias PDO,
    (	Lag >= 0.0 ->
        PDO_F lag P / Lag
    ;
        PDO_F mapdot 0 .* P
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
    % V0 lag V/6,
    Vol_F lag V/6.
get_volcanos(false, Zeros, Lag, Vol_F) :-
    Lag >= 0.0,
    context_box_model:volcanos(V),
    sparse_list(Zeros, V, Vol),
    V0 lag Vol/9,
    Vol_F lag V0/Lag.
get_volcanos(_, Zeros, _, Zeros).


get_tsi(Years, Lag, TSI_F) :-
    % equinox(TSI),
    tsi(TSI),
    interpolate(Years, TSI, TSI_I),
    TSI_U unbias TSI_I,
    (	Lag >= 0.0 ->
        TSI_F lag TSI_U / Lag
    ;
        TSI_F mapdot 0 .* TSI_U
    ).


get_nao(Lag, NAO_F) :-
    nao_hurrell(NAO),
    (	Lag >= 0 ->
        NAO_F lag NAO / Lag
    ;
        NAO_F mapdot 0 .* NAO
    ).


get_co2(Years, Lag, LogCO2) :-
    context_co2:co2_knmi(CO2),
    interpolate(Years, CO2, CO2_I),
    (	Lag >= 0.0 ->
	CO2_Lag lag CO2_I / Lag,
        LogCO2 mapdot ln ~> CO2_Lag
     ;
        LogCO2 mapdot 0 .* CO2_I
     ).

get_chandler_wobble(Years, Lag, CW_F) :-
    chandler_wobble(CW),
    interpolate(Years, CW, CW_I),
    (	Lag >= 0.0 ->
	CW_F delay CW_I / Lag
    ;
        CW_F mapdot 0 .* CW_I
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

get_qbo(T) :-
    qbo(T).

is_proper_float(X) :-
catch(_ is X+0.0, _, fail).


check_coefficients([], List, Final ) :- reverse(List, Final), !.
check_coefficients([F|Rest], List, Final ) :-
    print(user_error, ['v=', F]),
    % F = '$NaN', !, %  '$NaN'
    is_proper_float(F),
    check_coefficients(Rest, [F|List], Final).
% check_coefficients([F|Rest], List, Final ) :-
%    F = 'NA', !,
%    check_coefficients(Rest, [0.0|List], Final).
check_coefficients([_|Rest], List, Final ) :-
    check_coefficients(Rest, [0.0|List], Final).

show_rms([], Out, Out).
show_rms([[Name,A]|R], In, Out) :-
    rms(A, RMS),
    R1000 is 1000*RMS,
    format(atom(Value), '~1f', [R1000]),
    show_rms(R, [[Name,Value]|In], Out).
show_rms(Array, Values) :-
    show_rms(Array, [], Values).

show_equation([], Out, Out).
show_equation([[0.0,0.0,0.0]|R], In, Out) :-
    show_equation(R,In, Out).
show_equation([[Period,S,C]|R], In, Out) :-
    Val is sqrt(S*S+C*C),
    Phase is atan(S,C),
    format(atom(Value), '+ ~5g * sin(2*pi()*$a1/~5g+~5g) ', [Val,Period,Phase]),
    concat(In, Value, Eq),
    show_equation(R,Eq, Out).

show_periods([], Out, Out).
show_periods([[0.0,0.0,0.0]|R], In, Out) :-
    show_periods(R, In, Out).
show_periods([[Period,S,C]|R], In, Out) :-
    RMS is sqrt(S*S+C*C),
    format(atom(Value), '~3f', [RMS]),
    Year is Period / 12,
    % format(atom(Name), '~dyears', [Year]),
    show_periods(R, [[Year,Value]|In], Out).

show_periods(Array, out(Values, Equation)) :-
    show_periods(Array, [], Values),
    show_equation(Array, '', Equation).

html_rms(RMS, _, _) --> {var(RMS)}.
html_rms(RMS, Periodic, Equation) -->
	html(
	    [
	    p(Equation),
	    \(con_text:multi_columns([
		     /* div([
			 p(i('Temperature variance as RMS values in milliKelvin, baseline=1960')),
			 \(con_text:table_multiple_entries([[factor,rms]],RMS))
			 % \(con_text:paragraphs(RMS))
		       ]), */
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
    http_parameters(Request, [kind(volcanos, [])]),
    context_box_model:volcano_data(Data),
    reply_html_page([title('volcano data'),
                       \(con_text:style)],
                      [
                       \(con_text:table_multiple_entries(
                                      [['eruption',year,month,'month#',intensity, 'VEI']],
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
			      volc(Sato, [float, default(1.0)]),
			      eq(EQ_ON, [boolean, default(false)]),
			      wwii_adjust(WWII_Adjust, [boolean, default(false)]),
			      sign(SignMetric, [boolean, default(false)]),
			      wave(WL, [integer, default(0)]),
			      fit_res(Fit, [integer, default(0)]),
			      high_res(Add, [integer, default(0)]),
			      t_units(Cal, []),
			      startYear(StartYear, [integer]),
			      fitYear(FitYear, [integer]),
			      lag(LagCal, [float]),
			      soi_lag(SL, [number]),
			      aero_lag(VL, [number]),
			      co2_lag(AL, [number]),
			      lod_lag(LL, [number]),
			      tsi_lag(TL, [number]),
			      aam_lag(ML, [number]),
			      bary_lag(OL, [number]),
			      % amo_win(NL, [number]),
			      % arctic_win(AW, [number]),
                              dataset(DataSet, []),
                              evaluate(Characteristic, [default(model)])]),

    scale(FFT, LogLin, Characteristic, XLabel, XUnits, YLabel, YUnits, Show_Error_Bars),
    scaling(Cal, month, Scale),
    Lag is Scale*LagCal,

    % Get the temperature series
    temperature_series(WWII_Adjust, Window, Triple, DataSet, T, Fit, Correction),

    get_years_from_1880(T, Year, Zeros),


    get_months_from_start(T, Months),

    Q is 1.0,
    E = Add,

    Period is E*7.53*12,  %7.46 Hale/3 * 12 *Q, % precession cycle with the time for Spring tides to realign with the same day each year
    Period2 is 8.9 * 12,   % 9.08 9.015 Sun-Moon-Earth tidal configuration 8.715
    Period3 is Q*18.63* 12,  % 18.63 Lunar precessional
    Period4 is Q*4.08 * 12,   % 4.06 8.848 Lunar apsidal precession
    Period5 is Q*2.18*12, % 2.245 Add*11.86* 12 *Q,   % 11.86 Tidal sidereal period of Jupiter
    Period6 is Q*11.86*12, % 1.765 1.7655 1.7405 Others*Hale/7 * 1.026* 12 *Q, % Soros cycle tide --
    Period7 is E*5.63*12,  % 5.77 Add*Period5/2,       % Period5/2 24*Q,
    Period8 is 6.48*12, % Period3/3,       % 20.5
    Period9 is Q*8.848 * 12/2,
    PeriodA is Q*2.66*12,   % 2.72 2.763 Add*Hale/4 *0.955*12 *Q,
    PeriodB is 2.89*12, %2.898  Others*Hale/5 *0.955*12 *Q, % -------
    PeriodC is Q*2.09*12, % Hale/6 *12 *Q,
    PeriodD is E*12.9*12, % 13.52 0*Hale/2 *12,
    PeriodE is Q*3.62*12, % 3.53 Add*Hale/1 *12 *Q,
    PeriodF is 2.334*12, % 2.334 2.329 3.35 Random_F*12*20, % 2*12 *Q, % ------
    PeriodH is Q*1.93*12, %1.93 1.583 0*7.944*12, % 7.944 2.54  Venus 9.315*12*Q,  %

    Sin0 mapdot mod_sin_period(PeriodH,18,WL) ~> Months,
    Cos0 mapdot mod_cos_period(PeriodH,18,WL) ~> Months,
    Sin mapdot mod_sin_period(Period,10,WL) ~> Months,
    Cos mapdot mod_cos_period(Period,10,WL) ~> Months,
    Sin2 mapdot mod_sin_period(Period2,8,WL) ~> Months,
    Cos2 mapdot mod_cos_period(Period2,8,WL) ~> Months,
    Sin3 mapdot mod_sin_period(Period3,9,WL) ~> Months,
    Cos3 mapdot mod_cos_period(Period3,9,WL) ~> Months,
    Sin4 mapdot mod_sin_period(Period4,13,WL) ~> Months,
    Cos4 mapdot mod_cos_period(Period4,13,WL) ~> Months,
    Sin5 mapdot mod_sin_period(Period5,5,WL) ~> Months,
    Cos5 mapdot mod_cos_period(Period5,5,WL) ~> Months,
    Sin6 mapdot mod_sin_period(Period6,14,WL) ~> Months,
    Cos6 mapdot mod_cos_period(Period6,14,WL) ~> Months,
    Sin7 mapdot mod_sin_period(Period7,15,WL) ~> Months,
    Cos7 mapdot mod_cos_period(Period7,15,WL) ~> Months,
    Sin8 mapdot mod_sin_period(Period8,4 ,WL) ~> Months,
    Cos8 mapdot mod_cos_period(Period8,4 ,WL) ~> Months,
    Sin9 mapdot mod_sin_period(Period9,7 ,WL) ~> Months,
    Cos9 mapdot mod_cos_period(Period9,7 ,WL) ~> Months,
    SinA mapdot mod_sin_period(PeriodA,2,WL) ~> Months,
    CosA mapdot mod_cos_period(PeriodA,2,WL) ~> Months,
    SinB mapdot mod_sin_period(PeriodB,3 ,WL) ~> Months,
    CosB mapdot mod_cos_period(PeriodB,3 ,WL) ~> Months,
    SinC mapdot mod_sin_period(PeriodC,11 ,WL) ~> Months,
    CosC mapdot mod_cos_period(PeriodC,11 ,WL) ~> Months,
    SinD mapdot mod_sin_period(PeriodD,12,WL) ~> Months,
    CosD mapdot mod_cos_period(PeriodD,12,WL) ~> Months,
    SinE mapdot mod_sin_period(PeriodE,6,WL) ~> Months,
    CosE mapdot mod_cos_period(PeriodE,6,WL) ~> Months,
    SinF mapdot mod_sin_period(PeriodF,1 ,WL) ~> Months,
    CosF mapdot mod_cos_period(PeriodF,1 ,WL) ~> Months,

    Evection is 19.1*12, %12*31.854/365.25,
    % Evection is 12*1.0,
    LogCO2 mapdot mod_sin_period(Evection,16,WL) ~> Months,
    S2 mapdot mod_cos_period(Evection,16,WL) ~> Months,

    CWobble is 50.0*12,  % 1.599 1.185*12,
    TSI_F mapdot mod_sin_period(CWobble,17,WL) ~> Months,
    V1 mapdot mod_cos_period(CWobble,17,WL) ~> Months,

    K1 is 12*2.49, % 2.3697 12*3.73,
    AAM mapdot mod_sin_period(K1,0,WL) ~> Months,
    LOD_F mapdot mod_cos_period(K1,0,WL) ~> Months,
    %AAM mapdot sin_biennial_period ~> Months,
    %LOD_F mapdot cos_biennial_period ~> Months,


/*
    Period is E*7.289*12,  %Hale/3 * 12 *Q, % precession cycle with the time for Spring tides to realign with the same day each year
    Period2 is 9.22 * 12,   % 9.03 9.015 Sun-Moon-Earth tidal configuration 8.715
    Period3 is Q*18.23* 12,  % 18.63 Lunar precessional
    Period4 is Q*4.032 * 12,   % 4.06 8.848 Lunar apsidal precession
    Period5 is Q*2.2498*12, % 2.245 Add*11.86* 12 *Q,   % 11.86 Tidal sidereal period of Jupiter
    Period6 is Q*1.771*12, % 1.7655 Others*Hale/7 * 1.026* 12 *Q,       % Soros cycle tide -------
    Period7 is E*5.731*12,  % 5.77 Add*Period5/2,       % Period5/2 24*Q,
    Period8 is 6.09*12, % 6.48 Period3/3,       % 20.5
    Period9 is Q*8.851 * 12/2,
    PeriodA is Q*2.755*12,   % 2.763 Add*Hale/4 *0.955*12 *Q,
    PeriodB is 2.896*12, %2.898  Others*Hale/5 *0.955*12 *Q, % -------
    PeriodC is Q*2.087*12, % 2.09 Hale/6 *12 *Q,
    PeriodD is E*13.83*12, % 13.52 0*Hale/2 *12,
    PeriodE is Q*3.523*12, % 3.523  3.53 Add*Hale/1 *12 *Q,
    PeriodF is 2.347*12, %  2.329 3.35 Random_F*12*20, % 2*12 *Q, % ------
    PeriodH is Q*1.903*12, %1.93 1.583 0*7.944*12, % 7.944 2.54  Venus 9.315*12*Q,  %
    Sin0 mapdot mod_sin_period(PeriodH,14,WL) ~> Months,
    Cos0 mapdot mod_cos_period(PeriodH,14,WL) ~> Months,
    Sin mapdot mod_sin_period(Period,10,WL) ~> Months,
    Cos mapdot mod_cos_period(Period,10,WL) ~> Months,
    Sin2 mapdot mod_sin_period(Period2,8,WL) ~> Months,
    Cos2 mapdot mod_cos_period(Period2,8,WL) ~> Months,
    Sin3 mapdot mod_sin_period(Period3,9,WL) ~> Months,
    Cos3 mapdot mod_cos_period(Period3,9,WL) ~> Months,
    Sin4 mapdot mod_sin_period(Period4,13,WL) ~> Months,
    Cos4 mapdot mod_cos_period(Period4,13,WL) ~> Months,
    Sin5 mapdot mod_sin_period(Period5,5,WL) ~> Months,
    Cos5 mapdot mod_cos_period(Period5,5,WL) ~> Months,
    Sin6 mapdot mod_sin_period(Period6,14,WL) ~> Months,
    Cos6 mapdot mod_cos_period(Period6,14,WL) ~> Months,
    Sin7 mapdot mod_sin_period(Period7,15,WL) ~> Months,
    Cos7 mapdot mod_cos_period(Period7,15,WL) ~> Months,
    Sin8 mapdot mod_sin_period(Period8,4 ,WL) ~> Months,
    Cos8 mapdot mod_cos_period(Period8,4 ,WL) ~> Months,
    Sin9 mapdot mod_sin_period(Period9,7 ,WL) ~> Months,
    Cos9 mapdot mod_cos_period(Period9,7 ,WL) ~> Months,
    SinA mapdot mod_sin_period(PeriodA,2,WL) ~> Months,
    CosA mapdot mod_cos_period(PeriodA,2,WL) ~> Months,
    SinB mapdot mod_sin_period(PeriodB,3 ,WL) ~> Months,
    CosB mapdot mod_cos_period(PeriodB,3 ,WL) ~> Months,
    SinC mapdot mod_sin_period(PeriodC,11 ,WL) ~> Months,
    CosC mapdot mod_cos_period(PeriodC,11 ,WL) ~> Months,
    SinD mapdot mod_sin_period(PeriodD,12,WL) ~> Months,
    CosD mapdot mod_cos_period(PeriodD,12,WL) ~> Months,
    SinE mapdot mod_sin_period(PeriodE,6,WL) ~> Months,
    CosE mapdot mod_cos_period(PeriodE,6,WL) ~> Months,
    SinF mapdot mod_sin_period(PeriodF,1 ,WL) ~> Months,
    CosF mapdot mod_cos_period(PeriodF,1 ,WL) ~> Months,

    Evection is  12*21.97, %  31.854/365.25,
    % Evection is 12*1.0,
    LogCO2 mapdot mod_sin_period(Evection,16,WL) ~> Months,
    S2 mapdot mod_cos_period(Evection,16,WL) ~> Months,

    CWobble is 27.77*12, % 1.595*12,  % 1.595 1.185*12,
    TSI_F mapdot mod_sin_period(CWobble,17,WL) ~> Months,
    V1 mapdot mod_cos_period(CWobble,17,WL) ~> Months,

    %AAM mapdot mod_sin_period(24,0,WL) ~> Months,
    %LOD_F mapdot mod_cos_period(24,0,WL) ~> Months,
    AAM mapdot sin_biennial_period ~> Months,
    LOD_F mapdot cos_biennial_period ~> Months,

*/

    get_fit(StartYear, FitYear, [T, LogCO2, S2, TSI_F, V1, LOD_F, AAM, Sin0, Cos0, % Arctic, NAO,
	                                                        Sin,  Cos,  Sin2, Cos2,
	                                                        Sin3, Cos3, Sin4, Cos4,
								Sin5, Cos5, Sin6, Cos6,
								Sin7, Cos7, Sin8, Cos8,
	                                                        Sin9, Cos9, SinA, CosA,
	                                                        SinB, CosB, SinC, CosC,
	                                                        SinD, CosD, SinE, CosE,
	                                                        SinF, CosF],
	    Coefficients, Int, _R2C),
	    % [NoiseA, C, SO, TS, VC,   LO],

    check_coefficients(Coefficients, [], [C, SO, TS, VC,   LO, AA, SW0, CW0, % ARC, NI,
					                           SW,  CW,  SW2, CW2,
					                           SW3, CW3, SW4, CW4,
					                           SW5, CW5, SW6, CW6,
					                           SW7, CW7, SW8, CW8,
					                           SW9, CW9, SWA, CWA,
					                           SWB, CWB, SWC, CWC,
					                           SWD, CWD, SWE, CWE,
					                           SWF, CWF]),

    Fluct mapdot SO .* S2 + TS .* TSI_F + VC .* V1 + LO .* LOD_F + AA .* AAM +
                                           + SW0 .* Sin0 + CW0 .* Cos0  % ARC .* Arctic + NI .* NAO
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
		                           + SWF .* SinF + CWF .* CosF,

   % LaMod mapdot laplace_mod ~> Months,
   % context_complex:conv(Fluct0, LaMod, Fluct1),
   % Fluct shrink Fluct1/Fluct0,

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
    T_R_lag1 lag T_R / Lag,
    T_R_mod mapdot Sato*yearly_mod ~> Months,
    T_R_lag mapdot T_R_mod * T_R_lag1,
    (   EQ_ON ->
        SY is (StartYear - 1880)*12+1,
        FY is (FitYear - 1880)*12 + 1,
	% Wi is FY - SY,
        RC range [SY,FY]/1,
	% offset
        T_lag00 offset T_lag - SY,
        T_R_lag00 offset T_R_lag - SY,
        T_lag0 shrink T_lag00 / RC,
        T_R_lag0 shrink T_R_lag00 / RC
    ;
        T_lag0 = T_lag,
        T_R_lag0 = T_R_lag
    ),
    % corrcoeff(T_lag0, T_R_lag0, R2C20),
    (	SignMetric ->
        excursion(T_lag0, T_R_lag0, R2C20)
    ;
        corrcoeff(T_lag0, T_R_lag0, R2C20)
    ),
    R2C2 is 100.0*R2C20,
    (
       Characteristic = model ->
    /*
          r_open_session,
          r_in( library(dcv) ),
          model <- T_lag,
          data <- T_R_lag,
          r_in('test.RE(Model, data)'),
          r_close,
     */
         T_lag_low mapdot T_lag .- Error_Bar,
         T_lag_high mapdot Error_Bar .+ T_lag,
         T_lags tuple T_lag_low + T_lag + T_lag_high,
         T_R_lags tuple T_R_lag + T_R_lag + T_R_lag,
         Corrections tuple Correction + Correction + Correction,
	 % Data group Year + T_lag + T_R_lag, % + Corrections,
	 Data tuple Year + T_lag + T_R_lag, % + Corrections,
         Header = [XLabel, DataSet, model], % , correction]
         RMS = 1.0,
	 show_periods([[Evection,C, SO],
		       [CWobble, TS, VC],
		       [K1, LO, AA],
	               [PeriodH, SW0, CW0],
	               [Period, SW, CW],
		       [Period2, SW2, CW2],
		       [Period3, SW3, CW3],
		       [Period4, SW4, CW4],
		       [Period5, SW5, CW5],
		       [Period6, SW6, CW6],
		       [Period7, SW7, CW7],
		       [Period8, SW8, CW8],
		       [Period9, SW9, CW9],
		       [PeriodA, SWA, CWA],
		       [PeriodB, SWB, CWB],
		       [PeriodC, SWC, CWC],
		       [PeriodD, SWD, CWD],
		       [PeriodE, SWE, CWE],
		       [PeriodF, SWF, CWF]
		        ], out(Periodic, Equation))

     ;
       Characteristic = fluctuation ->
         TCO2 mapdot Int .+ C .* LogCO2,
         TFluct mapdot T_lag - TCO2,
         FF lag Fluct / Lag,
         Data tuple Year + TFluct + FF,
         Header = [XLabel, signal,  variability]
    ;
         Characteristic = map ->
	 Data tuple T_R_lag + T_lag,
         Header = [model, DataSet]
     ;
       Characteristic = cross ->
         S0S2 mapdot SO .* S2,
         TSTSI_F mapdot TS .* TSI_F,
         VCV1 mapdot VC .* V1,
	 LOLOD_F mapdot LO .* LOD_F,
         AAM_D mapdot AA .* AAM,
	 Waves1 mapdot SW .* Sin + CW .* Cos,
	 Waves2 mapdot SW2 .* Sin2 + CW2 .* Cos2,
         Waves3 mapdot SW3 .* Sin3 + CW3 .* Cos3,
         Waves4 mapdot SW4 .* Sin4 + CW4 .* Cos4,
	 Waves5 mapdot SW5 .* Sin5 + CW5 .* Cos5,
         Waves6 mapdot SW6 .* Sin6 + CW6 .* Cos6,
	 Waves7 mapdot SW7 .* Sin7 + CW7 .* Cos7,
         Waves8 mapdot SW8 .* Sin8 + CW8 .* Cos8,
	 Waves9 mapdot SW9 .* Sin9 + CW9 .* Cos9,
	 WavesA mapdot SWA .* SinA + CWA .* CosA,
	 WavesB mapdot SWB .* SinB + CWB .* CosB,
	 WavesC mapdot SWC .* SinC + CWC .* CosC,
	 WavesD mapdot SWD .* SinD + CWD .* CosD,
	 WavesE mapdot SWE .* SinE + CWE .* CosE,
	 WavesF mapdot SWF .* SinF + CWF .* CosF,

	 CO2_Strength mapdot C .* LogCO2,
         Data tuple Year + CO2_Strength + S0S2 + VCV1 + LOLOD_F + TSTSI_F + AAM_D +
	            Waves1 + Waves2 + Waves3 + Waves4 + Waves5 + Waves6 + Waves7 + Waves8 + Waves9 +
		    WavesA + WavesB + WavesC + WavesD + WavesE + WavesF ,
	 Header = [XLabel, co2, soi,   aero,  lod, tsi,	aam, barys, baryc,
		   p_22_3, p_9, p_18_6, p_8_85, p_11_86, p_3_22, p_11_86_2, p_18_6_3, p_8_85_2,
		   p_5_30, p_4_20, p_22_6, p_22_2, p_22_1, p_3_35, p_27, p_2_46, p_2_00, p_9_3]

     ;

       Characteristic = cycles ->
	 Cycle1 mapdot SW .*  Sin +  CW .*  Cos +  SWA .* SinA + CWA .* CosA +
	               SWB .* SinB + CWB .* CosB + SWC .* SinC + CWC .* CosC +
		       SWD .* SinD + CWD .* CosD + SWE .* SinE + CWE .* CosE,
         Cycle2 mapdot SW2 .* Sin2 + CW2 .* Cos2 + SW6 .* Sin6 + CW6 .* Cos6,
	 Cycle3 mapdot SW3 .* Sin3 + CW3 .* Cos3 + SW8 .* Sin8 + CW8 .* Cos8,
	 Cycle4 mapdot SW4 .* Sin4 + CW4 .* Cos4 + SW9 .* Sin9 + CW9 .* Cos9,
         Cycle5 mapdot SW5 .* Sin5 + CW5 .* Cos5 + SW7 .* Sin7 + CW7 .* Cos7,
         Cycle6 mapdot SWF .* SinF + CWF .* CosF,
	 AllCycles mapdot Cycle1 + Cycle2 +Cycle3 +Cycle4 + Cycle5 + Cycle6,
         S0S2 mapdot SO .* S2,
         % TSTSI_F mapdot TS .* TSI_F,
         AAM_D mapdot AA .* AAM,
	 AllNatural mapdot AAM_D + S0S2,
	 Data tuple Year + AllCycles + AllNatural,
         % Header = [XLabel, '22',   '18.03', '18.6',  '8.85',  '11.86', '2']
         Header = [XLabel, 'cycles', 'natural'],
	 corrcoeff(AllCycles, AllNatural, CycleCC),
	 print(user_error, [cycleCC, CycleCC])

     ;
       Characteristic = residual ->
         (   FFT ->
	     R_FFT fft T_Diff,
	     Range ordinal R_FFT,
	     Data tuple Range + R_FFT
	 ;
             % T_Base mapdot Int .+ C .* LogCO2 + LO .* LOD_F ,
             % Noise_Level mapdot T - T_Base,
	     % RHV mapdot NI .* NAO,
	     % get_qbo(QBO),
	     S0S2 mapdot 4*SO .* S2,
	     % LM2 mapdot 4*LM .* LogMethane,
	     %corrcoeff(T_Diff, S0S2, R2C2),
	     Data tuple Year + T_Diff % + S0S2 +LM2   %  + Noise_Level
	 ),

         Header = [XLabel, residual, soi_t, soi_d] % , fluctuation]
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
         TSTSI_F mapdot 9 .+ TS .* TSI_F,
         S0S2 mapdot 8 .+ SO .* S2,
         VCV1 mapdot 7 .+ VC .* V1,
	 LOLOD_F mapdot 5 .+ LO .* LOD_F,
         % Noise_D mapdot NoiseA .* Noise2,
         AAM_D mapdot 4 .+ AA .* AAM,
         % ARCTIC_D mapdot ARC .* Arctic,
         % NAO_D mapdot NI .* NAO,
	 Diurnal mapdot 3 .+ SW3 .* Sin3 + CW3 .* Cos3 +
	                SW8 .* Sin8 + CW8 .* Cos8,
	 SemiDiurnal mapdot 2 .+ SW4 .* Sin4 + CW4 .* Cos4 +
	                    SW9 .* Sin9 + CW9 .* Cos9,
	 % HaleCycle mapdot 0.1 .+ SW .* Sin + CW .* Cos +
	 %               SWC .* SinC + CWC .* CosC +
	 %               SWD .* SinD + CWD .* CosD +
	 %               SWE .* SinE + CWE .* CosE,
	 HaleCycle mapdot 1 .+ SW .* Sin + CW .* Cos +
	                SWA .* SinA + CWA .* CosA +
	                SW5 .* Sin5 + CW5 .* Cos5,
	 % Jupiter mapdot 0.2 .+ SW5 .* Sin5 + CW5 .* Cos5 +
	 %                SW7 .* Sin7 + CW7 .* Cos7,
	 SunMoonEarth  mapdot SW2 .* Sin2 + CW2 .* Cos2,
			 % SWH .* SinH + CWH .* CosH,
	 Cycle_20_80 mapdot -1 .+ SWF .* SinF + CWF .* CosF +
	                SW6 .* Sin6 + CW6 .* Cos6 +
	                SWA .* SinA + CWA .* CosA +
	                SWB .* SinB + CWB .* CosB,

	 Waves mapdot SW .* Sin + CW .* Cos + SW2 .* Sin2 + CW2 .* Cos2
                                              + SW3 .* Sin3 + CW3 .* Cos3
                                              + SW4 .* Sin4 + CW4 .* Cos4
					      + SW5 .* Sin5 + CW5 .* Cos5
                                              + SW6 .* Sin6 + CW6 .* Cos6
					      + SW7 .* Sin7 + CW7 .* Cos7
                                              + SW8 .* Sin8 + CW8 .* Cos8
                                              + SW9 .* Sin9 + CW9 .* Cos9, % Check

	 CO2_Strength mapdot Int .+ C .* LogCO2,
	 % Meth mapdot -0.2 .+ LM .* LogMethane,
	 ResidNoise mapdot -0.3 .+ T_Diff,
	 show_rms([% [amo, NAO_D],
		   % [arctic, ARCTIC_D],
		   ['set of orbital cycles',Waves],
		   [tsi,TSTSI_F],
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
		       [Period6, SW6, CW6],
		       [Period7, SW7, CW7],
		       [Period8, SW8, CW8],
		       [Period9, SW9, CW9],
		       [PeriodA, SWA, CWA],
		       [PeriodB, SWB, CWB],
		       [PeriodC, SWC, CWC],
		       [PeriodD, SWD, CWD],
		       [PeriodE, SWE, CWE],
		       [PeriodF, SWF, CWF]
		        ], out(Periodic, Equation)),

     /*
         Angular mapdot AAM_D + LOLOD_F + Y_Lunar,
         Data tuple Year + S0S2 + VCV1 + TSTSI_F + ARCTIC_D + NAO_D + Angular,
	 Header = [XLabel, soi,   aero,  tsi,      arctic,    nao,    angular]
     */
	 % Double mapdot SemiDiurnal + SunMoonEarth,
         Data tuple Year + TSTSI_F + S0S2 + VCV1 + LOLOD_F + AAM_D +
	                   Diurnal %
			   +SemiDiurnal
			   % + Double
			   + HaleCycle % + Jupiter + Venus
			   + SunMoonEarth
			   + Cycle_20_80 + ResidNoise + CO2_Strength, % + Meth,
	 Header = [XLabel, tsi,  soi, aero,      lod,      aam,
		           diurnal, semi, hale, % jupiter, venus,
		           sme,
		           '20/80', residual,co2] % , methane]

    ),
    temp_data(NameData, DataSet),
    TCR is C*ln(2),
    PPP is Period7/12,
    print(user_error, ['random period ', PPP]),

    (	Kind = graph ->
    reply_html_page([title('GISS and SOI'),
                     \(con_text:style)],
                    [
		     table([tr([th('R=cc'), th('yearSin'),th('yearCos'),th('halfYearSin'),
				th('halfYearCos'),th('biennialSin'), th('biennialCos') %, th(bary) % , th(amo), th(arctic)
			       ]),
			    tr([td(b('~5f'-R2C2)),td('~3f'-C),td('~3f'-SO),td('~3f'-VC),
				td('~3f'-TS),td('~3f'-LO), td('~5f'-AA) %, td('~5f'-SC)
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
						       [NameData, FitYear, ' - ENSO:', Characteristic, ' 2nd:', WWII_Adjust ], Data, Show_Error_Bars))
		     % \(context_graphing:dygraph_native(LogLin, Header,
		%				       [XLabel,XUnits], [YLabel, YUnits],
		%				       [NameData, FitYear, ' - ENSO:', Characteristic], Data))
				  )
			       ])]),
		     br([]),
		     % br([]),
		     p(i('TCR = ~4f C for doubling of CO2'-TCR)),
		     \html_rms(RMS, Periodic, Equation)
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







