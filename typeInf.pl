:- dynamic gvar/2, lvar/1.

typeExp(X, int) :-
    integer(X).

typeExp(X, float) :-
    float(X).

typeExp(X, bool) :-
    typeBoolExp(X).

/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(
        Fct, 
        Fname, 
        _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(
        Args, 
        [T], 
        FType), /* make it loook like a function signature */
    functionType(
        Fname,
         TArgs), /* get type of arguments from definition */
    typeExpList(
        FType, 
        TArgs). /* recurisvely match types */


/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(
        Hin, 
        Hout), /* type infer the head */
    typeExpList(
        Tin, 
        Tout). /* recurse */

hasComparison(int).
hasComparison(float).
hasComparison(string).

hasBool(bool).

/*
Addition
*/
hasAdd(int).
hasAdd(float).

/*
Subtraction
*/
hasSub(int).
hasSub(float).
/*
Division
*/
hasDiv(int).
hasDiv(float).
/*
Multiplication
*/
hasMul(int).
hasMul(float).

/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false). 

/* 
Less than
 */
typeBoolExp( X < Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).

/* 
Greater than
 */
typeBoolExp( X > Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).

/* 
Greater than or equal to 
 */
typeBoolExp( X =< Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).
 
/* 
Greater than or equal to 
 */
typeBoolExp( X =< Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).
 
/* 
Equal to
 */
typeBoolExp( X == Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).

/* 
Not equal to
 */
typeBoolExp( X \== Y) :- 
    typeExp(
        X, 
        T),
    typeExp(
        Y, 
        T),
    hasComparison(T).

/* TODO: add statements types and their type checking */

typeStatement(X, T) :-
    typeExp(
        X,
         T).

/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */


/*
The only difference between them is that you don't assert in local
*/

typeStatement(getVar(Name,T),unit):-
    gvar(
        Name,
        T).

typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(
        Code, 
        T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(
        gvar(
            Name, 
            T)). /* add definition to database */

/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(
        TrueB, 
        T),
    typeCode(
        FalseB, 
        T).

typeStatement(localLet(Name, T, Code, S),unit):-    
    atom(Name), 
    typeExp(Code,
         T), 
    bType(T),
    assert(gvar(Name,
        T)),
    typeCode(S,
        unit),
    retract(gvar(Name,
        T)).


typeStatement(for(Name, T, Cond, List)) :-   
    integer(Cond),    
    atom(Name),
    asserta(
        gvar(
            Name,
            T)),
        typeCode(List,
        T),
    retract(
        gvar(
            Name,
            T)).



/*
for statement 
*/

/*
Finding type of code Block
*/
codeBlock(codeB,T):-
    is_list(codeB),
    typeCode(
        codeB,
        T).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
typeCode([S], T):-
    typeStatement(
    S, 
    T).
typeCode([S, S2|Code], T):-
    typeStatement(
        S,
        _T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(
        Code,
         T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- 
    bType(H).
bType([H|T]):- 
    bType(
        H), 
        bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

/*

iplus :: int -> int -> int

*/


/*
stuff give to me
*/
fType(
    iplus,
    [int,int,int]).
fType((+), [T, T, T]) :- 
    hasAdd(T).
fType(
    fplus, 
    [float, float, float]).
fType(
    fsub, 
    [float, float, float]).
fType(
    fToInt,
    [float,int]).
fType(
    iToFloat, 
    [int,float]).
fType(
    print, 
    [_X, unit]). /* simple print */

/*
ones I added
*/

/*
iExpresions match to haskell type
*/
fType(
    isub, 
    [int,int,int]).
fType(
    idiv, 
    [int,int,int]).
fType(
    imul, 
    [int,int,int]).

/*
float expressions
*/
fType(
    fmul, 
    [float,float,float]).
fType(
    fdiv, 
    [float,float,float]).
/* 
operations
*/

fType((-), [T, T, T]) :- 
    hasSub(T).
fType((*), [T, T, T]) :- 
    hasMul(T).
fType((/), [T, T, T]) :- 
    hasDiv(T).

/* 
comparisons
*/

fType(
    (>), 
    [T, T, bool]).
fType(
    (>), 
    [T, T, bool]).
fType(
    (>=),
    [T, T, bool]).
fType(
    (<=), 
    [T, T, bool]).
fType(
    (==), 
    [T, T, bool]).



/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(
        Name, 
        Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(
        Name, 
        Args),
        !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().