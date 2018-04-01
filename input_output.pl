% Define module
:- module(input_output,[readLines/2]).

% Read all lines in file
readLines(InStream,W):- 
    get_code(InStream,Char), 
    checkCharAndReadRest(Char,Chars,InStream), 
    atom_codes(W,Chars). 

% if char is newline
% checkCharAndReadRest(10,[],_):-  !. 

% if char is blank space
% checkCharAndReadRest(32,[],_):-  !. 

% if at end of stream
checkCharAndReadRest(-1,[],_):-  !. 

% if at end of file
checkCharAndReadRest(end_of_file,[],_):-  !. 

% otherwise keep reading
checkCharAndReadRest(Char,[Char|Chars],InStream):- 
    get_code(InStream,NextChar), 
    checkCharAndReadRest(NextChar,Chars,InStream).

% Output if error message produced and close program
printErrorAndClose(ErrorMsg,OutputFileName):-
    write(ErrorMsg,OutputFileName),
    halt.