% Import other modules (aka other prolog files)
:- use_module(input_output,[readLines/2,printErrorAndClose/2]).
:- use_module(labels,[checkLabels/1]).
:- use_module(hardconstraints,[tooNear/4]).
:- use_module(softconstraints1,[createAllMatches/2,filterValidMatches/5]).


% Main functor
main:-
    % Get console argument values
    current_prolog_flag(argv, [_,InputFileName,OutputFileName|_]),

    % Open file and get lines of text
    open(InputFileName,read,Str), 
    readLines(Str,LinesOfFile),

    % Check file text for comments or label errors (Scott)
    checkLabels(LinesOfFile),

    % Parse lines of text to get values (Oliver)

    % Check parsed values for errors (Khalid)

    % Check hard constraints (Fungai, Jacky)
    % tooNear(ListOfMatches,ListOfMatches,TooNear,ValidMatches),

    % Check soft constraints
    createAllMatches(["A","B","C","D","E","F","G","H"],AllMatches),
    filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches),

    % Close file and write output
    close(Str), 
    write(LinesOfFile,OutputFileName),  nl.
