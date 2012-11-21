:- module(context_file_reading, []).

/** <module> Utilities for reading data files
    * CSV = comma separated values
*/

:- use_module(context_math).
:- use_module(components(messages)).

:- context:register(context_file_reading:crawl).

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
    atomic_list_concat([Path, '/',File], Dir),
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


traverse(Root) :-
    exists_directory(Root),
    print([Root]), nl,
    directory_files(Root, List),
    traverse_dirs(Root,List), !.


crawl(_Request) :-
    call_showing_messages(
    (   context_file_reading:traverse('/svn/Models') ),
        []
                          ) .

