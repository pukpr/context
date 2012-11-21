:- module(context_render, []).

/** <module> Rendering module
    * SVG of dual tracks
    *
*/

:- context:register(context_render:create).

:- use_module(context_math).

process_two_track(_, _, [_],[_],[_], Final, Final).
process_two_track(MX, MY, [X1,X2|XR],[Y1,Y2|YR],[Z1,Z2|ZR], Input, Final) :-
    % print(user_error, X1),
    X1a is X1/MX,
    X2a is X2/MX,
    Z1a is -(Z1/MY + 30),
    Z2a is -(Z2/MY + 30),
    X1b is X1/MX + 20,
    X2b is X2/MX + 20,
    Y1b is -Y1/MY,
    Y2b is -Y2/MY,
    process_two_track(
        MX, MY,
        [X2|XR], [Y2|YR], [Z2|ZR],
        [line([x1=X1b, y1=Y1b, x2=X2b, y2=Y2b, style='stroke:darkgreen'], []),
         line([x1=X2b, y1=Y2b, x2=X2a, y2=Z2a, style='stroke:url(#grad)'], []),
         line([x1=X2a, y1=Z2a, x2=X1a, y2=Z1a, style='stroke:darkblue'], []),
         line([x1=X1a, y1=Z1a, x2=X1b, y2=Y1b, style='stroke:url(#grad)'], [])| Input],
        Final).

process_xyz(_, _, [_],[_],[_], Final, Final).
process_xyz(MX, MY, [X1|XR],[Y1|YR],[Z1|ZR], Input, Final) :-
    % print(user_error, X1),
    X1a is (X1+Y1*2/pi)/MX,
    X1b is (X1+Y1*2/pi)/MX,
    Y1a is (Y1)/MY,
    Y2a is (Y1+Z1)/MY,
    process_xyz(
        MX, MY,
        XR, YR, ZR,
        [line([x1=X1a, y1=Y1a, x2=X1b, y2=Y2a, style='stroke:darkblue'], []) %,
         % line([x1=X2a, y1=Y1a, x2=X2a, y2=Y2a, style='stroke:url(#grad)'], []),
         % line([x1=X2a, y1=Y2a, x2=X1a, y2=Y2a, style='stroke:darkblue'], []),
         % line([x1=X1a, y1=Y2a, x2=X1a, y2=Y1a, style='stroke:url(#grad)'], [])
	 | Input],
        Final).

create(Request) :-
    http_parameters(Request, [uri(URI, [string])]),
    read_rdf(URI, _X_Units, _Z_Units, [DX,DY,DZ], _Header),
    context:max_min(DX, Max_X, Min_X),
    context:max_min(DY, Max_Y, Min_Y),
    context:max_min(DZ, Max_Z, Min_Z),
    Scale_Y is (max(Max_Y, Max_Z) - max(Min_Y, Min_Z))/100,
    Scale_X is (Max_X-Min_X)/1000,
    process_two_track(Scale_X,Scale_Y,DX,DY,DZ, [], Contents),
    render(URI, Contents).


surface(Request) :-
    http_parameters(Request, [uri(URI, [string])]),
    findall([X,Y,Z],
	    (	diffElev(URI,X,Y,Z),
	        Z > 0
	    ),
	    L),
    context_r_demo:xyz(L,[_|DX],[_|DY],[_|DZ]),
    ZZ mapdot log10 ~> DZ,
    context:max_min(DX, Max_X, Min_X),
    context:max_min(DY, Max_Y, Min_Y),
    %context:max_min(ZZ, Max_Z, Min_Z),
    Scale_X is (Max_X-Min_X)/500,
    Scale_Y is (Max_Y-Min_Y)/100,
    process_xyz(Scale_X,Scale_Y,DX,DY,ZZ, [], Contents),
    render(URI, Contents).


render(URI, Contents) :-
    File = 'html/test.svg',
    %Contents = [line([x1=3.1, y1=4.8, x2=112.4, y2=115.2], []),
    %		line([x1=4.1, y1=5.8, x2=102.4, y2=135.2], [])],
    XML = svg([version     = 1.1,
              'xmlns	   = "http://www.w3.org/2000/svg"',
              'xmlns:xlink = "http://www.w3.org/1999/xlink"',
               preserveAspectRatio = 'xMinYMin meet',
               viewBox     = '0 0 1050 280',      % 2*(100+40)
               % width       = 500,
               height      = 300
              ],
              [defs( linearGradient(
                        [id='grad',
                         x1='0%',
                         y1='0%',
                         x2='100%',
                         y2='100%'],
                        [stop([offset='0%',
                               style='stop-color:grey;stop-opacity:1'],
                              []),
                         stop([offset='100%',
                               style='stop-color:lightgrey;stop-opacity:1'],
                              [])
                        ]
                                  )
                  ),
               g([], text([fill='slategrey', x=0, y=15], URI)),
               g([transform = 'translate(0, 180)'],  % 100 * 2*40
                 g([fill = 'white', stroke = 'black'], Contents)
	       )]
	     ),
    tell(File),
    xml_write('',
              [doctype('svg'),
               public('-//W3C//DTD SVG 1.1 Tiny//EN'),
               system('http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-tiny.dtd')]),
    xml(XML),
    told,!,
    http_reply_file('./html/test.svg',[mime_type('image/svg+xml')], []).

