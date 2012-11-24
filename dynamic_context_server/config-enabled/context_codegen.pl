:- module(context_codegen, []).

/** <module> Code Generation utilities
    * Symbolic to String
    * Skeleton processing
    * C, Python, Modelica
*/


symbolic_to_string(Symbolic, String) :-
    write_to_chars(Symbolic,Chars),
    string_to_list(String,Chars).


generate_from_symbolic_skeleton(Begin, Inner, End, Code) :-
    symbolic_to_string(Inner, Code_String),
    atomic_list_concat([Begin,Code_String,End], Code).

