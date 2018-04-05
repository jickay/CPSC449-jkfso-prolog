% Define module
:- module(input_output,[readLines/2,printErrorAndClose/2]).

% Read all lines in file
readLines(InStream,W):- 
    get_code(InStream,Char), 
    checkCharAndReadRest(Char,Chars,InStream), 
    atom_codes(W,Chars). 

% if char is newline
% checkCharAndReadRest(10,[],_):-  !. 

% if char is blank space
% checkCharAndReadRest(32,[],_):-  !. 

% if char is % symbol for comment
checkCharAndReadRest(37,[],_):-  
    open('ErrorWhileParsingInputFile.txt',read,ErrorStream),
    readLines(ErrorStream,ErrorTerm),
    open('output.txt',write,OutputStream),%,[create([write])]),
    printErrorAndClose(OutputStream,ErrorTerm). 

% if char is # symbol for comment
checkCharAndReadRest(35,[],_):-  
        open('ErrorWhileParsingInputFile.txt',read,ErrorStream),
        readLines(ErrorStream,ErrorTerm),
        open('output.txt',write,OutputStream),%,[create([write])]),
        printErrorAndClose(OutputStream,ErrorTerm). 

% if at end of stream
checkCharAndReadRest(-1,[],_):-  !. 

% if at end of file
checkCharAndReadRest(end_of_file,[],_):-  !. 

% otherwise keep reading
checkCharAndReadRest(Char,[Char|Chars],InStream):- 
    get_code(InStream,NextChar), 
    checkCharAndReadRest(NextChar,Chars,InStream).

% Output if error message produced and close program
% ErrorMsg is a stream
printErrorAndClose(FileName,ErrorMsg):-
    open(FileName,write,OutputFileStream),
    write(OutputFileStream,ErrorMsg), nl(OutputFileStream),
    close(OutputFileStream).
    %halt, %Closes SWI-Prolog, but probably needed for final version