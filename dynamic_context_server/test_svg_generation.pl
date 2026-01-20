#!/usr/bin/env swipl

/*  Test script for SVG plot generation
    
    This script tests the generate_svg_plot predicate to verify
    that it can create SVG files with proper directory handling.
*/

:- initialization(main, main).

% Load required modules
:- use_module('config-enabled/context_r').

main :-
    write('Testing SVG plot generation...\n\n'),
    
    % Test 1: Generate SVG plot with sample data
    write('Test 1: Generating SVG plot with sample data... '),
    TestFile = 'html/images/test_plot.svg',
    
    % Sample data: years and ice-out days
    X = [2010, 2011, 2012, 2013, 2014],
    Y = [120.0, 125.0, 118.0, 130.0, 122.0],
    
    % Clean up any existing test file
    (exists_file(TestFile) -> delete_file(TestFile) ; true),
    
    % Try to generate the plot
    (   catch(
            context_r:rplot_with_regression(
                TestFile, 
                X, 
                Y, 
                'Test Plot', 
                'Year', 
                'Ice Out Day', 
                _Slope
            ),
            Error,
            (write('FAILED - Error: '), write(Error), nl, fail)
        )
    ->  % Check if file was created
        (   exists_file(TestFile)
        ->  write('PASSED - File created successfully\n'),
            % Read and verify it's valid SVG
            read_file_to_string(TestFile, Content, []),
            (   sub_string(Content, _, _, _, "<?xml version")
            ->  write('Test 2: SVG content is valid... PASSED\n')
            ;   write('Test 2: SVG content is valid... FAILED\n'),
                halt(1)
            ),
            % Clean up
            delete_file(TestFile),
            write('\nAll tests passed!\n'),
            halt(0)
        ;   write('FAILED - File was not created\n'),
            halt(1)
        )
    ;   write('FAILED - Exception occurred\n'),
        halt(1)
    ).
