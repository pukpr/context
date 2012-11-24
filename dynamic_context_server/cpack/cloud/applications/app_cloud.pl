/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(app_cloud, []).
:- use_module(library(datacloud)).
:- use_module(library(settings)).
:- use_module(library(process)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- http_handler(root('datacloud.svg'), datacloud,  [spawn,time_limit(1800)]).
:- http_handler(root('datacloud'),     cloud_page, []).

:- setting(cloud:image_file, atom, 'datacloud.svg',
	   'File for serving the datacloud').
:- setting(cloud:format, atom, 'svg',
	   'File for serving the datacloud').
:- setting(cloud:renderer, oneof([dot,neato,fdp]), 'fdp',
	   'File for serving the datacloud').

/** <module> Dynamically serve a datacloud
*/

%%	cloud_page(+Request)
%
%	Provide a full HTML page with the datacloud

cloud_page(_Request) :-
	http_link_to_id(datacloud, [], Data),
	reply_html_page(cliopatria(default),
			title('Data cloud'),
			[ object([ data(Data),
				   type('image/svg+xml'),
				   % width('100%'),
				   height('100%')
				 ],
				 [])
			]).


%%	datacloud(+Request)
%
%	Serve an SVG holding the datacloud

datacloud(Request) :-
	setting(cloud:image_file, File),
	with_mutex(cloud_server,
		   make_cloud(File, [])),
	http_reply_file(File, [], Request).


make_cloud(File, _) :-
	access_file(File, read),
	size_file(File, Size),
	Size > 0, !.
make_cloud(File, Options) :-
	setup_call_cleanup(tmp_file_stream(utf8, TmpFile, Tmp),
			   write_cloud_graph(Tmp, Options),
			   close(Tmp)),
	call_cleanup(run_dot(TmpFile,File),
		     delete_file(TmpFile)).

run_dot(In, Out) :-
	setting(cloud:renderer, Dot),
	setting(cloud:format, Format),
	atom_concat('-T', Format, FmtArg),
	atom_concat('-o', Out, OutArg),
	process_create(path(Dot),
		       [FmtArg, OutArg, In],
		       [
		       ]).
