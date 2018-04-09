% Import other modules (aka other prolog files)
:- use_module(input_output,[readLines/2,createSolution/3,printErrorAndClose/2]).
:- use_module(labels,[checkLabels/2,get_forced_partial/2,get_forbidden_machine/2,get_toonear_tasks/2,get_machine_penalties/2,get_toonear_penalties/2]).
:- use_module(hardconstraints,[tooNear/4]).
:- use_module(softconstraints1,[createAllMatches/2,filterValidMatches/5]).
:- use_module(softconstraints2,[getBestMatches/6]).


% Main functor
main:-
    % Get console argument values
    current_prolog_flag(argv, [_,InputFileName,OutputFileName|_]),

    % Open file and get lines of text
    open(InputFileName,read,Str), 
    readLines(Str,LinesOfFile),
    close(Str), 

    split_string(LinesOfFile,"\n","",ListOfLines),

    % Check file text for comments or label errors (Scott)
    checkLabels(ListOfLines,OutputFileName),

    get_forced_partial(ListOfLines, ForcedPartial),
    get_forbidden_machine(ListOfLines, ForbiddenMachine),
    get_toonear_tasks(ListOfLines, TooNearTasks),
    get_machine_penalties(ListOfLines, MachinePenalties),
    get_toonear_penalties(ListOfLines, TooNearPenalties),

    % Parse lines of text to get values (Oliver)

    % Check parsed values for errors (Khalid)

    % Check hard constraints (Fungai, Jacky)
    % tooNear(ListOfMatches,ListOfMatches,TooNear,ValidMatches),

    % Check soft constraints
    createAllMatches(["A","B","C","D","E","F","G","H"],AllMatches),
    filterValidMatches(AllMatches,Forced,Forbid,TooNear,ValidMatches),
    getBestMatches(ValidMatches,ValidMatches,Grid,TooNear,Total,BestMatches),

    % Close file and write output
    createSolution(BestMatches,Total,Solution),
    write(Solution,OutputFileName),  nl.
