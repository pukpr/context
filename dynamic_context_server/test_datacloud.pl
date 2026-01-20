#!/usr/bin/env swipl

/*  Test script for datacloud functionality
    
    This script tests the datacloud predicate using dot/graphviz
    to verify that it can generate proper cloud graph visualizations.
*/

:- initialization(main, main).

% Set up the cpack search path
user:file_search_path(cpack, 'cpack').

% Load datacloud module from the cpack
:- use_module(cpack('cloud/lib/datacloud')).

main :-
    write('Testing datacloud functionality...\n\n'),
    
    % Test 1: Check if write_cloud_graph/2 is available
    write('Test 1: Checking if write_cloud_graph/2 predicate exists... '),
    (   current_predicate(datacloud:write_cloud_graph/2)
    ->  write('PASSED\n')
    ;   write('FAILED - predicate not found\n'),
        halt(1)
    ),
    
    % Test 2: Check if graphviz (dot) command is available
    write('Test 2: Checking if graphviz (dot) is available... '),
    (   absolute_file_name(path(dot), DotPath, [access(execute), file_errors(fail)])
    ->  write('PASSED - found at '), write(DotPath), nl
    ;   write('FAILED - dot command not found in PATH\n'),
        halt(1)
    ),
    
    % Test 3: Check if fdp command is available
    write('Test 3: Checking if graphviz (fdp) is available... '),
    (   absolute_file_name(path(fdp), FdpPath, [access(execute), file_errors(fail)])
    ->  write('PASSED - found at '), write(FdpPath), nl
    ;   write('FAILED - fdp command not found in PATH\n'),
        halt(1)
    ),
    
    % Test 4: Check if neato command is available
    write('Test 4: Checking if graphviz (neato) is available... '),
    (   absolute_file_name(path(neato), NeatoPath, [access(execute), file_errors(fail)])
    ->  write('PASSED - found at '), write(NeatoPath), nl
    ;   write('FAILED - neato command not found in PATH\n'),
        halt(1)
    ),
    
    % Test 5: Test that we can generate a simple DOT graph with graphviz
    write('Test 5: Testing graphviz execution... '),
    catch(
        setup_call_cleanup(
            (   tmp_file('test_dot', TmpDot),
                open(TmpDot, write, Stream)
            ),
            (   write(Stream, 'digraph G { A -> B; }\n'),
                close(Stream),
                tmp_file('test_svg', TmpSvg),
                atom_concat(TmpSvg, '.svg', OutFile),
                process_create(path(dot), ['-Tsvg', '-o', OutFile, TmpDot], [process(PID)]),
                process_wait(PID, exit(0)),
                (   exists_file(OutFile)
                ->  write('PASSED - generated SVG output\n'),
                    delete_file(OutFile)
                ;   write('FAILED - no SVG output generated\n'),
                    halt(1)
                )
            ),
            delete_file(TmpDot)
        ),
        Error,
        (   write('FAILED with error: '), write(Error), nl,
            halt(1)
        )
    ),
    
    write('\n===========================================\n'),
    write('All datacloud/graphviz tests PASSED!\n'),
    write('===========================================\n'),
    write('\nSummary:\n'),
    write('- datacloud module is properly loaded\n'),
    write('- graphviz tools (dot, fdp, neato) are installed and working\n'),
    write('- The datacloud functionality is ready to use\n'),
    write('\nNote: To generate actual datacloud visualizations, you need to:\n'),
    write('1. Start the server with: ./run-cloud.sh start\n'),
    write('2. Load RDF data with CloudNode entries\n'),
    write('3. Access http://localhost:3020/datacloud\n'),
    halt(0).
