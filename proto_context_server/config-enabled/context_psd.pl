:- module(context_psd, [two_level_random_walk/4,
		       two_level/3
		       % two_level_re/3,
		       % two_level_im/3
		       ]).

/** <module> PSD models
    * Two level semi-Markov model
    * Markov
*/

:- use_module(context_complex).
:- use_module(context_math).
:- use_module(context_random_walk).

:- context:register(context_psd:complex_psd).

delayed_exponent(L, Alpha, S, R) :-
    Theta is -L*S,
    Num isx 1.0*exp(i*Theta),
    Damp is Alpha*S,
    R isx Num / (1.0 & Damp).

two_level_model(L1, Alpha1, L2, Alpha2, Weight, S, R) :-
    delayed_exponent(L1, Alpha1, S, P),
    delayed_exponent(L2, Alpha2, S, Q),
    % WW is Weight^2,
    WW is Weight^2/sqrt(L1+Alpha1+L2+Alpha2),
    I isx 1.0 & 0.0,
    K isx S   & 0.0,
    W isx WW  & 0.0,
    R & _ isx W*(I-P)*(I-Q)/(I-P*Q)/K^2.

rdf_(corrugations, alpha1, 1.0).
rdf_(corrugations, l1, 5.0).
rdf_(corrugations, alpha2, 1.0).
rdf_(corrugations, l2, 27.5).
rdf_(corrugations, weight, 1.0).

/*
rdf_(gero_corrugations, alpha1, 0.01).
rdf_(gero_corrugations, l1, 0.11).
rdf_(gero_corrugations, alpha2, 0.01).
rdf_(gero_corrugations, l2, 0.605).
rdf_(gero_corrugations, weight, 0.01).
*/

rdf_(gero_corrugations, alpha1, 0.02).
rdf_(gero_corrugations, l1, 0.11).
rdf_(gero_corrugations, alpha2, 0.02).
rdf_(gero_corrugations, l2, 0.61).
rdf_(gero_corrugations, weight, 0.01).

rdf_(gero_corrugations_r, alpha1, 40.0).
rdf_(gero_corrugations_r, l1, 0.0).
rdf_(gero_corrugations_r, alpha2, 40.0).
rdf_(gero_corrugations_r, l2, 0.0).
rdf_(gero_corrugations_r, weight, 0.005).

rdf_(gero_corrugations_s, alpha1, 0.3).
rdf_(gero_corrugations_s, l1, 0.3).
rdf_(gero_corrugations_s, alpha2, 0.3).
rdf_(gero_corrugations_s, l2, 0.315).
rdf_(gero_corrugations_s, weight, 0.002).

rdf_(gero_corrugations_x, alpha1, 0.009).
rdf_(gero_corrugations_x, l1, 0.009).
rdf_(gero_corrugations_x, alpha2, 0.009).
rdf_(gero_corrugations_x, l2, 0.009).
rdf_(gero_corrugations_x, weight, 0.003).


rdf_(gero_belgian, alpha1, 0.0133).
rdf_(gero_belgian, l1, 0.13).
rdf_(gero_belgian, alpha2, 0.0133).
rdf_(gero_belgian, l2, 0.02).
rdf_(gero_belgian, weight, 0.006).

rdf_(gero_belgian_1, alpha1, 0.08).
rdf_(gero_belgian_1, l1, 0.133).
rdf_(gero_belgian_1, alpha2, 0.08).
rdf_(gero_belgian_1, l2, 0.133).
rdf_(gero_belgian_1, weight, 0.003).

rdf_(gero_belgian_2, alpha1, 0.54).
rdf_(gero_belgian_2, l1, 0.133).
rdf_(gero_belgian_2, alpha2, 0.54).
rdf_(gero_belgian_2, l2, 0.133).
rdf_(gero_belgian_2, weight, 0.005).

rdf_(gero_belgian_3, alpha1, 5.4).
rdf_(gero_belgian_3, l1, 0.133).
rdf_(gero_belgian_3, alpha2, 5.4).
rdf_(gero_belgian_3, l2, 0.133).
rdf_(gero_belgian_3, weight, 0.005).


rdf_(gero_fatigue, alpha1, 0.75).
rdf_(gero_fatigue, l1, 0.5).
rdf_(gero_fatigue, alpha2, 0.75).
rdf_(gero_fatigue, l2, 0.5).
rdf_(gero_fatigue, weight, 0.015). %

rdf_(gero_fatigue_s, alpha1, 1.4).
rdf_(gero_fatigue_s, l1, 2.0).  %2x
rdf_(gero_fatigue_s, alpha2, 0.04).
rdf_(gero_fatigue_s, l2, 0.2).
rdf_(gero_fatigue_s, weight, 0.012).

rdf_(gero_fatigue_p, alpha1, 0.04).
rdf_(gero_fatigue_p, l1, 0.2).
rdf_(gero_fatigue_p, alpha2, 1.4).
rdf_(gero_fatigue_p, l2, 2.0).
rdf_(gero_fatigue_p, weight, 0.012).

rdf_(gero_fatigue_x, alpha1, 0.006).
rdf_(gero_fatigue_x, l1, 0.008).
rdf_(gero_fatigue_x, alpha2, 0.006).
rdf_(gero_fatigue_x, l2, 0.008).
rdf_(gero_fatigue_x, weight, 0.01). %

rdf_(gero_fatigue_r, alpha1, 10.0).
rdf_(gero_fatigue_r, l1, 0.0).
rdf_(gero_fatigue_r, alpha2, 10.0).
rdf_(gero_fatigue_r, l2, 0.0).
rdf_(gero_fatigue_r, weight, 0.015).

/*
rdf_(gero_pothole, alpha1, 0.1). %1.5
rdf_(gero_pothole, l1, 0.9). %7.5
rdf_(gero_pothole, alpha2, 0.1). % 0.02
rdf_(gero_pothole, l2, 0.9).
rdf_(gero_pothole, weight, 0.06).
*/
rdf_(gero_pothole, alpha1, 1.5). %1.5
rdf_(gero_pothole, l1, 7.5). %7.5
rdf_(gero_pothole, alpha2, 0.03). % 0.02
rdf_(gero_pothole, l2, 0.57).  % 0.55
rdf_(gero_pothole, weight, 0.05).

rdf_(gero_pothole_r, alpha1, 40.0).
rdf_(gero_pothole_r, l1, 0.0).
rdf_(gero_pothole_r, alpha2, 40.0).
rdf_(gero_pothole_r, l2, 0.0).
rdf_(gero_pothole_r, weight, 0.01).

rdf_(gero_pothole_s, alpha1, 5.0).
rdf_(gero_pothole_s, l1, 1.0).
rdf_(gero_pothole_s, alpha2, 5.0).
rdf_(gero_pothole_s, l2, 1.0).
rdf_(gero_pothole_s, weight, 0.005).

rdf_(gero_pothole_x, alpha1, 0.5).
rdf_(gero_pothole_x, l1, 0.1).
rdf_(gero_pothole_x, alpha2, 0.5).
rdf_(gero_pothole_x, l2, 0.1).
rdf_(gero_pothole_x, weight, 0.002).


rdf_(mercedes_benz, alpha1, 0.0133).
rdf_(mercedes_benz, l1, 0.13).
rdf_(mercedes_benz, alpha2, 0.0133).
rdf_(mercedes_benz, l2, 0.02).
rdf_(mercedes_benz, weight, 0.002).

rdf_(mercedes_benz_2, alpha1, 0.54).
rdf_(mercedes_benz_2, l1, 0.133).
rdf_(mercedes_benz_2, alpha2, 0.54).
rdf_(mercedes_benz_2, l2, 0.133).
rdf_(mercedes_benz_2, weight, 0.002).

rdf_(mercedes_benz_3, alpha1, 20.0).
rdf_(mercedes_benz_3, l1, 0.133).
rdf_(mercedes_benz_3, alpha2, 20.0).
rdf_(mercedes_benz_3, l2, 0.133).
rdf_(mercedes_benz_3, weight, 0.015).




rdf_(mn_big_ravines, alpha1, 2000.0).
rdf_(mn_big_ravines, l1, 800.0).
rdf_(mn_big_ravines, alpha2, 80.0).
rdf_(mn_big_ravines, l2, 80.0).
rdf_(mn_big_ravines, weight, 8.0).

rdf_(mn_small_ravines, alpha1, 500.0).
rdf_(mn_small_ravines, l1, 200.0).
rdf_(mn_small_ravines, alpha2, 20.0).
rdf_(mn_small_ravines, l2, 20.0).
rdf_(mn_small_ravines, weight, 8.0).

rdf_(mn_roughness, alpha1, 30.0).
rdf_(mn_roughness, l1, 2.0).
rdf_(mn_roughness, alpha2, 30.0).
rdf_(mn_roughness, l2, 10.0).
rdf_(mn_roughness, weight, 0.5).


rdf_(ou_roughness, alpha1, 71.5).
rdf_(ou_roughness, l1, 0.0).
rdf_(ou_roughness, alpha2, 71.5).
rdf_(ou_roughness, l2, 0.0).
rdf_(ou_roughness, weight, 50.0).

rdf_(sm_roughness, alpha1, 71.5).
rdf_(sm_roughness, l1, 0.0).
rdf_(sm_roughness, alpha2, 71.5).
rdf_(sm_roughness, l2, 0.0).
rdf_(sm_roughness, weight, 1.0).


rdf_(rock_roughness, alpha1, 0.5).
rdf_(rock_roughness, l1, 0.0).
rdf_(rock_roughness, alpha2, 0.5).
rdf_(rock_roughness, l2, 0.0).
rdf_(rock_roughness, weight, 0.1).


/*
    semi_random_walker(X, 1.2, [18.0,3.0], [2.0,2.0], Z0, Z1),
    semi_random_walker(X, 0.6, [20.0,20.0], [12.0,12.0], Z1, Z2),
    semi_random_walker(X, 3.0, [20.0,20.0], [80.0,80.0], Z2, Z3),
    semi_random_walker(X, 1.0, [200.0,200.0], [800.0,800.0], Z3, Z4),
*/

two_level_random_walk(Course, X, Z, Result) :-
    rdf_(Course, alpha1, Alpha1),
    rdf_(Course, l1, L1),
    rdf_(Course, alpha2, Alpha2),
    rdf_(Course, l2, L2),
    rdf_(Course, weight, Weight),
    semi_random_walker(X, Weight, [L1,L2], [Alpha1,Alpha2], Z, Result).

two_level(Course, S, Result) :-
    rdf_(Course, alpha1, Alpha1),
    rdf_(Course, l1, L1),
    rdf_(Course, alpha2, Alpha2),
    rdf_(Course, l2, L2),
    rdf_(Course, weight, Weight),
    two_level_model(L1, Alpha1, L2, Alpha2, Weight, S, Result).
/*
two_level_re(Course, S, Result) :-
    two_level(Course, S, Result&_).
two_level_im(Course, S, Result) :-
    two_level(Course, S, _&Result).
*/

complex_psd(Request) :-
    http_parameters(Request, [course(Course, [])]),
    F range [0.002, 2.0]/0.002,
    Out mapdot two_level(Course) ~> F,
    R tuple F + Out,
    reply_html_page(% cliopatria(default),
                   [title('PSD'),
                   \(con_text:style_cliopatria)],
                   [
                     \(context_graphing:dygraph_native( true,
							['x', 'y'],
                                                      'wave number',
                                                      'PSD',
                                                      'PSD from two-level model',
                                                      R))
                   ]).











