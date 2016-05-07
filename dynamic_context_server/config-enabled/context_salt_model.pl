:- module(context_salt_model, [
			       yearly_sin_period/5,
			       yearly_cos_period/5,
			       square_with_sign/2,
                               eureqa_sin/3,
                               eureqa_cos/3,
                               pdo/2,
                               fake_power_law/4,
                               eur/2,
			       cmss_fit/2,
			       scmss_fit/2,
			       sin_n/5,
			       planck_hale/6,
			       comb/2,
			       cfc_fit/2
			      ]).

:- use_module(context_math).
:- use_module(context_stats).

:- context:register(context_salt_model:navigate).
:- context:register(context_salt_model:plot).

% test_period(N,_,Period) :- Period is N*N/20*12+N*2.134.
test_period(_,P,P).

yearly_sin_period(0.0,N,M,_,0) :- N =< M.
yearly_sin_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is sin(2*pi*X/P).
yearly_sin_period(_,N,M,_,0.0) :- N > M.
yearly_cos_period(0.0,N,M,_,0) :- N =< M.
yearly_cos_period(Period,N,M,X,Y) :- N =< M, test_period(N,Period,P), Y is cos(2*pi*X/P).
yearly_cos_period(_,N,M,_,0.0) :- N > M.
/*
square_with_sign(X,Y) :- X > 0, Y is X + 0.01*X*X, !.
square_with_sign(X,X).
*/
square_with_sign(X,Y) :- Y is X^2 + 1*X.
fake_power_law(S,N,X,Y) :- Y is (X/S)^N.

sin_n(N,Period,Phase,X,Y) :- Y is (1-0.3*sin(2*pi*X/(2*Period) + Phase))*sin(2*pi*X/Period + Phase)^N, Y < -0.002.
sin_n(_,_,_,_,0.0).


comb(X,Y) :-
   Time is X/12+1880,
   Y is 0.00667970776152764*cos(1.00001166881362*Time + 0.000460124781386264*Time^2) - 0.00545092811737013*cos(2.20607832064059*Time).

/*
sin_n(_,_,_,X,Y) :-
   Time is X/12+1879.8,
Y is 0.0125377452661435*cos(-0.433245586798324*Time) + 0.00860055762569373*cos(1.55540829134865*Time) + 0.0125377452661435*cos(-0.433245586798324*Time)^2*cos(1.76661719483914*Time) - 0.00672101891764665*cos(2.3999071303648*Time) - 0.0188911524477231*cos(1.76661719483914*Time)*cos(0.436273798386119*Time).
*/

planck_hale(Period1,Period2,Phase1,Phase2,X,Y) :-
   Year is X/12,
   A is sin(2*pi*Year/Period1 + Phase1),
   B is sin(2*pi*Year/Period2 + Phase2),
   Y is abs((A-B)^8).

eureqa_sin(S, X, Y) :-
    Year is X/12,     % Y is 0.05169*sin(1.405*Year)*sin(5.779 + 0.04055*Year)*sin(5.013 - 0.04486*Year).
    Y is % 0.03369*sin(0.0534 + 0.02544*Year)*
         sin(S*(Year^2) - 2*Year).
eureqa_cos(S, X, Y) :-
    Year is X/12,    % Y is 0.05169*sin(1.405*Year)*sin(5.779 + 0.04055*Year)*sin(5.013 - 0.04486*Year).
    Y is % 0.03369*sin(0.0534 + 0.02544*Year)*
         cos(S*(Year^2) - 2*Year).

eur(X,Y) :-
   E1 = -0.09,
   E2 = 1.09,
   E3 = -2.3,
   E4 = 0.46,
   E5 = -0.32,
   C = 0.205,
   T is (X - 0.933*12)/12,
   Y is C*(0.06972*cos(-1.5*T+E1)+0.06418*sin(-1.863*T+E2)+0.06185*cos(-1.19*T+E3)
   -0.1132*sin(-0.3053*T+E4)*cos(-2.247*T+E5)).

pdo(X,Y) :-
    Year is (X-0)/12,
    Year < 96.85,
    Y is 0.0151*sin(5.774 - 1.915*Year) - 0.02839*sin(0.004966*(Year^2) - 2.027*Year), !.
pdo(X,Y) :-
    Year is (X-0)/12 - 96.85,
    Y is 0.04771*sin(5.756 + 2.554*Year)*sin(1.056 - 0.0182*(Year^2)).

cmss_fit(X,Y) :-
    Year is 1880.0 + (X-1)/12,
    Y is 0.05027*sin(0.03572 + 0.4539*Year) - 0.1037*sin(0.4901*Year) - 0.1972*sin(6.266 - 0.3148*Year).
  % Y is 0.05315*sin(0.0 + 0.4507*Year) - 0.10023*sin(0.00422+0.4901*Year) - 0.196*sin(0.0 - 0.3148*Year).
  % Y is 0.05358*sin(0.0 + 0.4507*Year) - 0.1005*sin(0.0 + 0.4901*Year) + 0.1949*sin(0.0 + 0.3148*Year).

scmss_fit(X,Y) :-
    Year is 1880.0 + (X-1)/12,
    Y is  0.06039*sin(0.02401 + 0.5287*Year) - 0.2699*cos(0.3156*Year) - 0.05436*sin(0.4713*Year)*sin(-0.01807*Year).
    %-0.06385*sin(6.277 + 0.5304*Year) - 0.2691*sin(0.9902 - 0.3153*Year).

cfc_fit(X,Y) :-
    Year is 1880.0 + (X-1)/12,
    Y is 0.147*exp(-((Year-2012.5)^3)/(-4000)-((Year-2012.5)^2)/38000).


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
temp_data('CET', cet).
temp_data('Sydney Tide', sydney_tide).
temp_data('Sydney Tide Raw', sydney_tide_raw).

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

dataset(ou_rw, L) :-
    Range range [1,1605]/1,
    PL mapdot fake_power_law(1650.0,3) ~> Range,
    context_random_walk:ou_random_walker(0.005,1,0.1,Range,RW),
    L mapdot PL + RW, !.

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
dataset(cet, L) :-
        cet(L0),
	L unbias L0.

dataset(sydney_tide, L) :-
	sydney_tide_raw(L0),
	L unbias L0.

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
			  % \(con_text:check_box(anthro, 'true', 'anthro aerosols')),
			  \(con_text:check_box(volc, 'true', 'GISS aerosol model')),
			  br([]),
			  \(con_text:radio_toggles(
					 'evaluate',
					 [
                                          ['Underlying CO2 signal', signal],
					  ['View the residual error', residual],
					  ['View staggered fluctuation components', all],
					  ['Match temperature with model', model],
					  ['Correlate CO2 with model', correlate],
					  ['Correlate temperature with model', map],
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
			  \(con_text:check_box(eq, 'true', 'EQ')),
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
						      [lod_lag,48.0,2],
						      [tsi_lag,6,2],
                                                      [aam_lag,6,2],
                                                      [bary_lag,6,2]
                                                      % [amo_win,-120,2],
						      % [arctic_win, -120,2]
						     ]))
				       ]
					)]),
				     p([' ...... ']),
				     div([style([ type('text/css'), scoped ],
					 '@import url("/html/css/context.css")'),
					  input([type('text'), size(2), name('wave'), value('30')]),
					  % \(con_text:check_box(wave, 'true', 'cycles On ')),
					  \(con_text:check_box(fit_res, 1, 'fit res', checked)),
					  \(con_text:check_box(high_res, 1, 'high res', checked)),
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
								  Dynamo,
			                                          Methane
								  ],
	[             C,   S,   T,   A,    L,   M, Z, N, V, W, P, Q,
		                                E, F, G, H, D, I, R, U,
		                                A1, B1, C1, D1, E1, F1,
		                                G1, H1, I1, J1, K1, L1,
		                                M1, N1, O1, P1, Q1, R1,
		                                S1, T1, U1, V1, W1, X1, J, K, DY,LM], Int, R2) :-
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
   y1 <- Methane,
   % pccr <- princomp('~y+c+s+a+l+t+m+z+j+k+n+v+w+p+q+e+f+g+h+d+i+r+u+a1+b1+c1+d1+e1+f1+g1+h1+i1+j1+k1+l1+m1+n1+o1+p1+q1+r1+s1+t1+u1+v1+w1+x1'),
   % r_print(summary(pccr)),
   % Test <- 'as.double(pccr$coefficients[1])',
   % print(user_error, Test),
   % r_print(loadings(pccr)),
   B is (EndDate - 1880)*12,
   O is (StartDate - 1880)*12 + 1,
   % O = 1, % 840,
   format(atom(Eq), 'y[~d:~d]~~c[~d:~d]+s[~d:~d]+a[~d:~d]+l[~d:~d]+t[~d:~d]+m[~d:~d]+veS[~d:~d]+veC[~d:~d]+h3S[~d:~d]+h3C[~d:~d]+s9S[~d:~d]+s9C[~d:~d]+diS[~d:~d]+diC[~d:~d]+t1S[~d:~d]+t1C[~d:~d]+j1S[~d:~d]+j1C[~d:~d]+h7S[~d:~d]+h7C[~d:~d]+j2S[~d:~d]+j2C[~d:~d]+d3S[~d:~d]+d3C[~d:~d]+t2S[~d:~d]+t2C[~d:~d]+h4S[~d:~d]+h4C[~d:~d]+h5S[~d:~d]+h5C[~d:~d]+h6S[~d:~d]+h6C[~d:~d]+h2S[~d:~d]+h2C[~d:~d]+h1S[~d:~d]+h1C[~d:~d]+q3S[~d:~d]+q3C[~d:~d]+loS[~d:~d]+loC[~d:~d]+v4S[~d:~d]+v4C[~d:~d]+s3S[~d:~d]+s3C[~d:~d]+bsc[~d:~d]+bcm[~d:~d]+dyn[~d:~d]+y1[~d:~d]',
	  [O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,
	   O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B,O,B]),
   fitxy <- lm(Eq),

	% fitxy <- lm('y~c+s+a+l+t+m+z+n+v+w+p+q+e+f+g+h+d+i+r+u+a1+b1+c1+d1+e1+f1+g1+h1+i1+j1+k1+l1+m1+n1+o1+p1+q1+r1+s1+t1+u1+v1+w1+x1+j+k'),
   %fitxy <- lm('y[1:1320]~c[1:1320]+s[1:1320]+a[1:1320]+l[1:1320]+t[1:1320]+m[1:1320]+z[1:1320]+n[1:1320]+v[1:1320]+w[1:1320]+p[1:1320]+q[1:1320]+e[1:1320]+f[1:1320]+g[1:1320]+h[1:1320]+d[1:1320]+i[1:1320]+r[1:1320]+u[1:1320]+a1[1:1320]+b1[1:1320]+c1[1:1320]+d1[1:1320]+e1[1:1320]+f1[1:1320]+g1[1:1320]+h1[1:1320]+i1[1:1320]+j1[1:1320]+k1[1:1320]+l1[1:1320]+m1[1:1320]+n1[1:1320]+o1[1:1320]+p1[1:1320]+q1[1:1320]+r1[1:1320]+s1[1:1320]+t1[1:1320]+u1[1:1320]+v1[1:1320]+w1[1:1320]+x1[1:1320]+j[1:1320]+k[1:1320]'),
   % fitxy <- lm('y[1:900]~c[1:900]+s[1:900]+a[1:900]+l[1:900]+t[1:900]+m[1:900]+z[1:900]+n[1:900]+v[1:900]+w[1:900]+p[1:900]+q[1:900]+e[1:900]+f[1:900]+g[1:900]+h[1:900]+d[1:900]+i[1:900]+r[1:900]+u[1:900]+a1[1:900]+b1[1:900]+c1[1:900]+d1[1:900]+e1[1:900]+f1[1:900]+g1[1:900]+h1[1:900]+i1[1:900]+j1[1:900]+k1[1:900]+l1[1:900]+m1[1:900]+n1[1:900]+o1[1:900]+p1[1:900]+q1[1:900]+r1[1:900]+s1[1:900]+t1[1:900]+u1[1:900]+v1[1:900]+w1[1:900]+x1[1:900]+j[1:900]+k[1:900]'),

   % fitxy <- lm('y[1:1601]~c[1:1601]+s[1:1601]+a[1:1601]+l[1:1601]+t[1:1601]+m[1:1601]+z[1:1601]+n[1:1601]+v[1:1601]+w[1:1601]+p[1:1601]+q[1:1601]+e[1:1601]+f[1:1601]+g[1:1601]+h[1:1601]+d[1:1601]+i[1:1601]+r[1:1601]+u[1:1601]+a1[1:1601]+b1[1:1601]+c1[1:1601]+d1[1:1601]+e1[1:1601]+f1[1:1601]+g1[1:1601]+h1[1:1601]+i1[1:1601]+j1[1:1601]+k1[1:1601]+l1[1:1601]+m1[1:1601]+n1[1:1601]+o1[1:1601]+p1[1:1601]+q1[1:1601]+r1[1:1601]+s1[1:1601]+t1[1:1601]+u1[1:1601]+v1[1:1601]+w1[1:1601]+x1[1:1601]+j[1:1601]+k[1:1601]'),

   %   Add the variables here !!! don't forget
   %REAL r_print(fitxy),
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
   LM <- 'as.double(fitxy$coefficients[49])',
   summary <- summary(fitxy),
   %REAL r_print(summary),
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
   Shift = -0.12, % -0.4
   Shift2 = -0.08,
   Plateau = -0.015,
   (   Correction ->
       Profile = [[1880,0],
		  [1938,0],
		  [1941,0],
		  [1942.5,Shift],
		  [1946,Shift],
		  [1947,Plateau],
                  [1952, Plateau],
                  [1952.5, Shift2],
                  [1953.5, Shift2],
                  [1954, Plateau],
		  [1960, Plateau],
		  [1960, 0.0],
		  [1967,0],
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

get_soi(Lag, SOI_F) :-
    soi(SOI),
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

get_eclipses(false, Zeros, Zeros) :- !.
get_eclipses(_, Zeros, Ecl_F) :-
    eclipses(E),
    sparse_list(Zeros, E, Ecl),
    Ecl_F lag Ecl/1.

get_rh(Lag, RH_F) :-
    rh(RH0),
    RH unbias RH0,
    (	Lag >= 0.0 ->
        RH_F lag RH / Lag
    ;
        RH_F mapdot 0 .* RH
    ).

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

get_pdo(Months, Lag, AMO_F) :-
    AMO mapdot pdo ~> Months,
    (	Lag >= 0 ->
        AMO_F = AMO
    ;
        AMO_F mapdot 0 .* AMO
    ).

get_eureka(Months, Lag, E_F) :-
    E mapdot eur ~> Months,
    (	Lag >= 0 ->
        E_F = E
    ;
        E_F mapdot 0 .* E
    ).

get_t(Months, Lag, AMO_F) :-
    AMO mapdot Months,
    (	Lag >= 0 ->
        AMO_F = AMO
    ;
        AMO_F mapdot 0 .* AMO
    ).

get_t2(Months, Lag, E_F) :-
    E mapdot Months * Months,
    (	Lag >= 0 ->
        E_F = E
    ;
        E_F mapdot 0 .* E
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

get_chandler_wobble(Years, Lag, CW_F) :-
    chandler_wobble(CW),
    interpolate(Years, CW, CW_I),
    (	Lag >= 0.0 ->
	CW_F delay CW_I / Lag
    ;
        CW_F mapdot 0 .* CW_I
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

get_qbo(T) :-
    qbo(T).

get_methane(Years, Lag, LogMethane) :-
    methane(Methane),
    interpolate(Years, Methane, Methane_I),
    (	Lag >= 0.0 ->
	Methane_Lag lag Methane_I / Lag,
        LogMethane mapdot ln ~> Methane_Lag
    ;
        LogMethane mapdot 0 .* Methane_I
    ).

get_cfc(Time, Lag, CFC) :-
    CFC_R mapdot cfc_fit ~> Time,
    (	Lag >= 0.0 ->
        CFC unbias CFC_R
	% CFC lag CFC_U / Lag
    ;
        CFC mapdot 0 .* CFC_R
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
    RMS is 1000*sqrt(S*S+C*C),
    format(atom(Value), '~1f', [RMS]),
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
			      volc(Sato, [boolean, default(false)]),
			      eq(EQ_ON, [boolean, default(false)]),
			      wwii_adjust(WWII_Adjust, [boolean, default(false)]),
			      % wave(WL, [boolean, default(false)]),
			      wave(WL, [integer, default(0)]),
			      fit_res(Others, [integer, default(0)]),
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
    % Chandler is 17.8 * 12,
    % 7.3 = http://tallbloke.wordpress.com/2013/02/07/short-term-forecasting-uah-lower-troposphere/

    /*

 % Keeling Whorf
    Period is 223 * Q,
    Period2 is 112 * Q,
    Period3 is 111 * Q,
    Period4 is 76.5 * Q,
    Period5 is 70 * Q,
    Period6 is 41*Q,
    Period7 is 35.5*Q,
    Period8 is 34.5*Q,
    Period9 is 28*Q,

*/

/*
    Period is 22 * 12,   % 22
    Period2 is 11 * 12,  % 11
    Period3 is 7.35 * 12,
    Period4 is 5.5 * 12,
    Period5 is 4.4 * 12,
    Period6 is 3.65 * 12,  % 3.65
*/


    Q is 1 , % 0.9333,

    Hale=21.98,

    % Scafetta
    Period is Hale/3 * 12 *Q,      % precession cycle with the time for Spring tides to realign with the same day each year
    % Period is 15*12*Q,
    Period2 is 9.015 * 12 *Q,   % 9.015 Sun-Moon-Earth tidal configuration 8.715
    % Period2 is 6*12*Q,
    Period3 is 18.613 * 12 *Q,  % Lunar precessional
    Period4 is Add*8.848 * 12 *Q,   % Lunar apsidal precession
    % Period4 is 3*12*Q,
    Period5 is Add*11.86* 12 *Q,   % 11.86 Tidal sidereal period of Jupiter
    % Period5 is 5*12*Q,
    % Period6 is Period2*2,       % Soros cycle tide
    % Period6 is 3.22 * 12 *Q,       % Soros cycle tide
    Period6 is Others*Hale/7 * 1.026* 12 *Q,       % Soros cycle tide -------
     % random(Random_F),
     % Period7 is Random_F*12*6,
    Period7 is Add*Period5/2,       % Period5/2 24*Q,
    % Period7 is Period2*2/3,       % Period5/2 24*Q,
    Period8 is Period3/3,       % 20.5
    % Period8 is 2*12*Q,
    Period9 is 8.848 * 12 *Q/2,
    %PeriodA is 2*pi*12/1.189, %Hale/4 *12 *Q,
    %PeriodB is 2*pi*12/1.499, %Hale/5 *12 *Q,
    %PeriodC is 2*pi*12/1.872, %Hale/6 *12 *Q,
    PeriodA is Add*Hale/4 *0.955*12 *Q,
    PeriodB is Others*Hale/5 *0.955*12 *Q, % -------
    PeriodC is Hale/6 *12 *Q,
    PeriodD is Add*Hale/2 *12 *Q,
    PeriodE is Add*Hale/1 *12 *Q,
    % random(Random_F),
    PeriodF is Others*3.35*12 *Q, % 3.344 Random_F*12*20, % 2*12 *Q, % ------
    % PeriodG is 1*12 *Q,
    % PeriodG is Random_F*12*50,  % Hale/7 *12 *Q,
    PeriodG is 9.015 * 12 *Q*3,  % 27
    % PeriodG is 4*12*Q,
    PeriodH is Add*7.944*12*Q, % 7.944 2.54  Venus 9.315*12*Q,  %
    % PeriodI is Add*PeriodH/4,  % 1.986
    % PeriodJ is 9.015 * 12 *Q/3,
    PeriodI is 12.0,  % 1.986
    PeriodJ is 6.0,

    Sin mapdot yearly_sin_period(Period,1,WL) ~> Months,
    Cos mapdot yearly_cos_period(Period,1,WL) ~> Months,
    Sin2 mapdot yearly_sin_period(Period2,2,WL) ~> Months,
    Cos2 mapdot yearly_cos_period(Period2,2,WL) ~> Months,
    Sin3 mapdot yearly_sin_period(Period3,8 ,WL) ~> Months,
    Cos3 mapdot yearly_cos_period(Period3,8 ,WL) ~> Months,
    Sin4 mapdot yearly_sin_period(Period4,6,WL) ~> Months,
    Cos4 mapdot yearly_cos_period(Period4,6,WL) ~> Months,
    Sin5 mapdot yearly_sin_period(Period5,3,WL) ~> Months,
    Cos5 mapdot yearly_cos_period(Period5,3,WL) ~> Months,
    Sin6 mapdot yearly_sin_period(Period6,14,WL) ~> Months,
    Cos6 mapdot yearly_cos_period(Period6,14,WL) ~> Months,
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
    %r4s(SinA),
    %r4c(CosA),
    SinB mapdot yearly_sin_period(PeriodB,12 ,WL) ~> Months,
    CosB mapdot yearly_cos_period(PeriodB,12 ,WL) ~> Months,
    %r7s(SinB),
    %r7c(CosB),
    SinC mapdot yearly_sin_period(PeriodC,11 ,WL) ~> Months,
    CosC mapdot yearly_cos_period(PeriodC,11 ,WL) ~> Months,
    SinD mapdot yearly_sin_period(PeriodD,17,WL) ~> Months,
    CosD mapdot yearly_cos_period(PeriodD,17,WL) ~> Months,
    SinE mapdot yearly_sin_period(PeriodE,19,WL) ~> Months,
    CosE mapdot yearly_cos_period(PeriodE,19,WL) ~> Months,
    %context_soim:soi_result(CosE0),
    %CosE1 shrink CosE0 / Months,
    %CosE lag CosE1 / SL,
    % CosE mapdot comb ~> Months,
    SinF mapdot yearly_sin_period(PeriodF,10 ,WL) ~> Months,
    CosF mapdot yearly_cos_period(PeriodF,10 ,WL) ~> Months,
    SinG mapdot yearly_sin_period(PeriodG,5,WL) ~> Months,
    CosG mapdot yearly_cos_period(PeriodG,5,WL) ~> Months,
    SinH mapdot yearly_sin_period(PeriodH,15,WL) ~> Months,
    CosH mapdot yearly_cos_period(PeriodH,15,WL) ~> Months,

    (	4 =< WL ->
       Dynamo mapdot sin_n(11,172.4,-1.1) ~> Months
    ;
       Dynamo = Zeros
    ),

    SinI mapdot yearly_sin_period(PeriodI,18,WL) ~> Months,
    CosI mapdot yearly_cos_period(PeriodI,18,WL) ~> Months,
    %r3s(SinI),
    %r3c(CosI),
    SinJ mapdot yearly_sin_period(PeriodJ,16,WL) ~> Months,
    CosJ mapdot yearly_cos_period(PeriodJ,16,WL) ~> Months,
    (	 EQ_ON ->
    get_scmss(EQ_ON, Months, OL, SCMSS),
    get_cmss(EQ_ON, Months, OL, CMSS)
    ;
    get_scmss(EQ_ON, Year, OL, SCMSS),
    get_cmss(EQ_ON, Year, OL, CMSS)
    ),


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

    % get_methane(Year, AL, LogMethane),
    get_cfc(Months, 1, LogMethane),

    % get_arctic(Year, AW, Arctic),
    % get_amo(NL, NAO),       % get_nao(Year, NL, NAO),
    % get_rh(-1, NAO),
    % get_chandler_wobble(Year, NL, NAO),
    % ? get_eureka(Months, NL, NAO),
    % ? get_pdo(Months, AW, Arctic),
    %get_t(Months, NL, NAO),
    %get_t2(Months, AW, Arctic),

    %EScale = 0.0046, % 676,
    %Arctic mapdot eureqa_sin(EScale) ~> Months,
    %NAO mapdot eureqa_cos(EScale) ~> Months,

    % %% Arctic mapdot yearly_sin_period(Chandler,WL) ~> Months,
    % %% NAO mapdot yearly_cos_period(Chandler,WL) ~> Months,

    % get_darwin(NL, NAO),
    % get_soi_peak(NL, NAO),

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
	                                                        SCMSS, CMSS, Dynamo,
				                                LogMethane],
	    Coefficients, Int, _R2C),
	    % [NoiseA, C, SO, TS, VC,   LO],

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
					                                    SC, CM, DY, LM]),

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
					   + DY .* Dynamo
					   + LM .* LogMethane,

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
	 Data group Year + T_lags + T_R_lags + Corrections,
         Header = [XLabel, DataSet, model, correction]

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
/*
       Characteristic = cross ->
         TSI_alone mapdot TS .* TSI_F,
	 Waves mapdot % TS .* TSI_F +
                      SW .* Sin + CW .* Cos
		    + SWA .* SinA + CWA .* CosA
                    + SWB .* SinB + CWB .* CosB
                    + SWC .* SinC + CWC .* CosC
                    + SWD .* SinD + CWD .* CosD
                    + SWE .* SinE + CWE .* CosE ,

         % AMO_C mapdot  NI .* NAO,
         % Orbit mapdot  SC .* SCMSS + CM .* CMSS,
         % Combo mapdot Waves + Orbit,
         % Combo mapdot Waves + NI .* NAO,

	 Data tuple Year +TSI_alone + Waves, % + Combo,
         Header = [XLabel, tsi, period7] %, harmonics]
*/
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
	 WavesG mapdot SWG .* SinG + CWG .* CosG,
	 WavesH mapdot SWH .* SinH + CWH .* CosH,
	 WavesI mapdot SWI .* SinI + CWI .* CosI,
	 WavesJ mapdot SWJ .* SinJ + CWJ .* CosJ,

         Bary mapdot  SC .* SCMSS + CM .* CMSS,
	 CO2_Strength mapdot C .* LogCO2,
         Data tuple Year + CO2_Strength + S0S2 + VCV1 + LOLOD_F + TSTSI_F + AAM_D + Bary +
	            Waves1 + Waves2 + Waves3 + Waves4 + Waves5 + Waves6 + Waves7 + Waves8 + Waves9 +
		    WavesA + WavesB + WavesC + WavesD + WavesE + WavesF + WavesG + WavesH + WavesI + WavesJ,
	 Header = [XLabel, co2, soi,   aero,  lod, tsi,	aam, bary,
		   p_22_3, p_9, p_18_6, p_8_85, p_11_86, p_3_22, p_11_86_2, p_18_6_3, p_8_85_2,
		   p_5_30, p_4_20, p_22_6, p_22_2, p_22_1, p_3_35, p_27, p_2_46, p_2_00, p_9_3]

     ;

       Characteristic = cycles ->
         Cycle0 mapdot SC .* SCMSS + CM .* CMSS,
	 Cycle1 mapdot SW .*  Sin +  CW .*  Cos +  SWA .* SinA + CWA .* CosA +
	               SWB .* SinB + CWB .* CosB + SWC .* SinC + CWC .* CosC +
		       SWD .* SinD + CWD .* CosD + SWE .* SinE + CWE .* CosE + SWG .* SinG + CWG .* CosG,
         Cycle2 mapdot SW2 .* Sin2 + CW2 .* Cos2 + SW6 .* Sin6 + CW6 .* Cos6,
	 Cycle3 mapdot SW3 .* Sin3 + CW3 .* Cos3 + SW8 .* Sin8 + CW8 .* Cos8,
	 Cycle4 mapdot SW4 .* Sin4 + CW4 .* Cos4 + SW9 .* Sin9 + CW9 .* Cos9,
         Cycle5 mapdot SW5 .* Sin5 + CW5 .* Cos5 + SW7 .* Sin7 + CW7 .* Cos7,
         Cycle6 mapdot SWF .* SinF + CWF .* CosF,
	 Cycle7 mapdot SWH .* SinH + CWH .* CosH + SWI .* SinI + CWI .* CosI + SWJ .* SinJ + CWJ .* CosJ,
         % Bary mapdot  SC .* SCMSS + CM .* CMSS,
	 AllCycles mapdot Cycle0 + Cycle1 + Cycle2 +Cycle3 +Cycle4 + Cycle5 + Cycle6 +Cycle7,
	 % Cycle7 mapdot SW7 .* Sin7 + CW7 .* Cos7,
         % Cycle8 mapdot SW8 .* Sin8 + CW8 .* Cos8,
         % Cycle9 mapdot SW9 .* Sin9 + CW9 .* Cos9,
         S0S2 mapdot SO .* S2,
         % TSTSI_F mapdot TS .* TSI_F,
         AAM_D mapdot AA .* AAM,
	 AllNatural mapdot AAM_D + S0S2,
	 Data tuple Year + AllCycles + AllNatural,
         % Header = [XLabel, '22',   '18.03', '18.6',  '8.85',  '11.86', '2']
         Header = [XLabel, 'cycles', 'natural'],
	 corrcoeff(AllCycles, AllNatural, CycleCC),
	 print(user_error, [cycleCC, CycleCC])
/*
    Period is 7.3 * 12,      % precession cycle with the time for Spring tides to realign with the same day each year
    Period2 is 9.015 * 12,   % Sun-Moon-Earth tidal configuration
    Period3 is 18.6 * 12,    % Lunar precessional
    Period4 is 8.85 * 12,    % Lunar apsidal precession
    Period5 is 11.86 * 12,   % Tidal sidereal period of Jupiter
    Period6 is Period2*2,    % Soros cycle tide
    Period7 is 24*Q,
    Period8 is Period3/3,    % 20.5
*/

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
	     Data tuple Year + T_Diff   %  + Noise_Level
	 ),

         Header = [XLabel, residual, qbo] % , fluctuation]
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
	 Dyno_F mapdot 1.0 .+ DY .* Dynamo,
         TSTSI_F mapdot 0.9 .+ TS .* TSI_F,
         S0S2 mapdot 0.8 .+ SO .* S2,
         VCV1 mapdot 0.7 .+ VC .* V1,
	 LOLOD_F mapdot 0.5 .+ LO .* LOD_F,
         % Noise_D mapdot NoiseA .* Noise2,
         AAM_D mapdot 0.4 .+ AA .* AAM,
         % ARCTIC_D mapdot ARC .* Arctic,
         % NAO_D mapdot NI .* NAO,
	 Diurnal mapdot 0.3 .+ SW3 .* Sin3 + CW3 .* Cos3 +
	                SW8 .* Sin8 + CW8 .* Cos8,
	 SemiDiurnal mapdot 0.2 .+ SW4 .* Sin4 + CW4 .* Cos4 +
	                    SW9 .* Sin9 + CW9 .* Cos9,
	 HaleCycle mapdot 0.1 .+ SW .* Sin + CW .* Cos +
	                SWC .* SinC + CWC .* CosC +
	                SWD .* SinD + CWD .* CosD +
	                SWE .* SinE + CWE .* CosE,
	 % Jupiter mapdot 0.2 .+ SW5 .* Sin5 + CW5 .* Cos5 +
	 %                SW7 .* Sin7 + CW7 .* Cos7,
	 SunMoonEarth  mapdot SWG .* SinG + CWG .* CosG +
	                SW2 .* Sin2 + CW2 .* Cos2 +
			SWJ .* SinJ + CWJ .* CosJ,
			 % SWH .* SinH + CWH .* CosH,
	 Cycle_20_80 mapdot -0.1 .+ SWF .* SinF + CWF .* CosF +
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

         Bary mapdot  -0.2 .+ SC .* SCMSS + CM .* CMSS,
	 CO2_Strength mapdot Int .+ C .* LogCO2,
	 % Meth mapdot 0.0 .+ LM .* LogMethane,
	 ResidNoise mapdot -0.3 .+ T_Diff,
	 show_rms([% [amo, NAO_D],
		   % [arctic, ARCTIC_D],
		   ['set of orbital cycles',Waves],
		   [tsi,TSTSI_F],
		   [bary,Bary],
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
		       [PeriodF, SWF, CWF],
		       [PeriodG, SWG, CWG],
		       [PeriodH, SWH, CWH],
		       [PeriodI, SWI, CWI],
		       [PeriodJ, SWJ, CWJ]
		        ], out(Periodic, Equation)),

     /*
         Angular mapdot AAM_D + LOLOD_F + Y_Lunar,
         Data tuple Year + S0S2 + VCV1 + TSTSI_F + ARCTIC_D + NAO_D + Angular,
	 Header = [XLabel, soi,   aero,  tsi,      arctic,    nao,    angular]
     */
	 % Double mapdot SemiDiurnal + SunMoonEarth,
         Data tuple Year + Dyno_F  + TSTSI_F + S0S2 + VCV1 + LOLOD_F + AAM_D +
	                   Diurnal %
			   +SemiDiurnal
			   % + Double
			   + HaleCycle % + Jupiter + Venus
			   + SunMoonEarth
			   + Cycle_20_80 + Bary + ResidNoise + CO2_Strength, % + Meth,
	 Header = [XLabel, dynamo, tsi,  soi, aero,      lod,      aam,
		           diurnal, semi, hale, % jupiter, venus,
		           sme,
		           '20/80', bary, residual,co2] % , methane]

    ),
    temp_data(NameData, DataSet),
    TCR is C*ln(2),
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







