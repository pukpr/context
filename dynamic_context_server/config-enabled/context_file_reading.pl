:- module(context_file_reading, []).

/** <module> Utilities for reading data files
    * CSV = comma separated values
*/

:- use_module(context_math).
:- use_module(components(messages)).

:- context:register(context_file_reading:crawl).
:- context:register(context_file_reading:download).

get_context_dir(Path) :-
    (
        getenv('CONTEXT',Path)
    ;
        Path = '/svn/Models'
    ).


download(Request) :-
    http_parameters(Request, [uri(URI, [string]),
                              type(Type, [string])]),
    MIME=mime_type(Type),
    File = URI,
    http_reply_file(
        File,
        [MIME],
        []).


% CSV -------------------------------------------
read_tabbed_csv(FN, X, Y, Arity, Data) :-
    Arity = 2,
    csv_read_file(FN, [xy(X,Y)|Elements],
                      [functor(xy),
                       separator(9), % tab
                       arity(Arity)]),
    separate(Elements, Data), !.


read_tabbed_csv(FN, X, Y, Arity, Data) :-
    Arity = 3,
    csv_read_file(FN, [xyz(X,Y,Y)|Elements],
                      [functor(xyz),
                       separator(9), % tab
                       arity(Arity)]),
    separate(Elements, Data).

parse([], Xc, X, Yc, Y) :- reverse(Xc, X), reverse(Yc, Y).
parse([[X,Y|_]|Rest], Ix, Xc, Iy, Yc) :-
    parse(Rest, [X|Ix], Xc, [Y|Iy], Yc).


read_tabbed_csv(FN, X, Y, Arity, XV, YV) :-
    Arity = 2,
    csv_read_file(FN, [xy(X,Y)|Elements],
                      [functor(xy),
                       separator(9), % tab
                       arity(Arity)]),
    separate(Elements, Data),
    parse(Data, [], XV, [], YV).

% ---------------------------------------------------

%	 SVN crawler looks for models

excluded([
    '.',
    '..',
    '.svn',
    '.git',
    'Tools',
    'tools',
    'test' ]).


is_dir(Path, File, Dir) :-
    atomic_list_concat([Path, '/', File], Dir),
    exists_directory(Dir).

print_if_files_in_dir(File) :-
    directory_files(File, List),
    aggregate_all(count, ( member(F, List),is_dir(File, F, _) ), NS),
    length(List, NL),
    NL > NS,
    Total is NL - NS,
    format(atom(S), '~d files in ~w', [Total, File]),
    print_message(informational, format('~w', S)).
    % print(S), nl, !.
print_if_files_in_dir(_File).

if_file_found(Dir, File, FilePath) :-
    directory_files(Dir, List),
    member(File, List),
    % writeln([Dir, File]),
    atomic_list_concat([Dir,'/',File], FilePath),
    % writeln(FilePath),
    !.

% List all
traverse_dirs(_Path, []).
traverse_dirs(Path, [First|Rest]) :-
    excluded(Exclude),
    not(member(First, Exclude)),
    is_dir(Path, First, Dir),
    print_if_files_in_dir(Dir),
    directory_files(Dir, List),
    !,
    traverse_dirs(Dir, List),
    traverse_dirs(Path, Rest).
traverse_dirs(Path, [_|Rest]) :-
    !,
    traverse_dirs(Path, Rest).

% Find specific
traverse_dirs(_Path, [], _File, _FileName).
traverse_dirs(Path, [First|_Rest], File, FilePath) :-
    excluded(Exclude),
    not(member(First, Exclude)),
    is_dir(Path, First, Dir),
    if_file_found(Dir, File, FilePath), !.
traverse_dirs(Path, [First|Rest], File, FilePath) :-
    excluded(Exclude),
    not(member(First, Exclude)),
    is_dir(Path, First, Dir),
    directory_files(Dir, List),
    !,
    traverse_dirs(Dir, List, File, FilePath),
    traverse_dirs(Path, Rest, File, FilePath).
traverse_dirs(Path, [_|Rest], File, FilePath) :-
    !,
    traverse_dirs(Path, Rest, File, FilePath).


%
%
traverse(Root) :-
    exists_directory(Root),
    print([Root]), nl,
    directory_files(Root, List),
    traverse_dirs(Root,List), !.


find(File, FilePath) :-
    get_context_dir(Path),
    exists_directory(Path),
    directory_files(Path, List),
    traverse_dirs(Path, List, File, FilePath),
    nonvar(FilePath), !.
find(_File, 'not found').



crawl(_Request) :-
    get_context_dir(Path),
    call_showing_messages(
    (   context_file_reading:traverse(Path) ),
        []
                          ) .



