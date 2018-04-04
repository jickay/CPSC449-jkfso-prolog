% Import other modules (aka other prolog files)
:- use_module(input_output,[readLines/2,printErrorAndClose/2]).
:- use_module(hardconstraints,[hasTooNear/3]).


% Main functor
main:-
    % Get console argument values
    current_prolog_flag(argv, [_,InputFileName,OutputFileName|_]),

    % Open file and get lines of text
    open(InputFileName,read,Str), 
    readLines(Str,LinesOfFile),

    % Check file text for comments or label errors (Scott)

    % Parse lines of text to get values (Oliver)

    % Check parsed values for errors (Khalid)

    % Check hard constraints (Fungai, Jacky)
    hasTooNear(Matches,TooNear,1) -> printErrorAndClose("No valid solution possible!",OutputFileName);

    % Check soft constraints

    % Close file and write output
    close(Str), 
    write(LinesOfFile,OutputFileName),  nl.
