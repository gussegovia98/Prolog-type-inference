:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

%this test should pass
test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

test(typeExp_isub_T, [true(T == int)]) :-
    typeExp(isub(int, int), T).

test(typeExp_isub_F, [fail]) :-
    typeExp(isub(int, int), float).

test(typeExp_imul_T, [true(T == int)]) :-
    typeExp(imul(int, int), T).

test(typeExp_imul_F, [fail]) :-
    typeExp(imul(int, int), float).

test(typeExp_idiv_T, [true(T == int)]) :-
    typeExp(idiv(int, int), T).

test(typeExp_idiv_F, [fail]) :-
    typeExp(idiv(int, int), float).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

test(typeExp_fplus_F, [fail]) :-
    typeExp(fplus(int, int), int).

test(typeExp_fsub_T, [true(T == float)]) :-
    typeExp(fsub(float, float), T).

test(typeExp_fsub_F, [fail]) :-
    typeExp(fsub(int, int), int).

test(typeExp_fmul_T, [true(T == float)]) :-
    typeExp(fmul(float, float), T).

test(typeExp_fmul_F, [fail]) :-
    typeExp(fmul(int, int), int).

test(typeExp_fdiv_T, [true(T == float)]) :-
    typeExp(fdiv(float, float), T).

test(typeExp_fdiv_F, [fail]) :-
    typeExp(fdiv(int, int), int).



% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct


test(boolTest, [nondet]):-
    typeStatement(if(3=<7,[3],[7]),T),
    assertion(T==int).


test(simple_if, [nondet]) :-
    typeStatement( if(true, 
        [5.2], 
        [6.3]),
         T),
    assertion(T==float).

test(simple_for, [nondet]) :-   
    typeStatement(for(v, T, 7,
         [if(2 < 7, 
             [2.0], 
             [7.0])])),    
    assertion(T==float).

test(code_block, [nondet]) :-  
      typeCode([if(13 < 22, [13], [22]) 
      ,if(7 > 1, [7.0], [1.0])], 
      T),    
      assertion(T==float).

/*
Let in
*/
test(letIn, [nondet]) :-
    deleteGVars(),
    infer([localLet(a, int, idiv(int,int), [localLet(b, float, fmul(float,float), [getVar(a,A),getVar(b,B)])])], unit),
    assertion(A==int),
    assertion(B==float).

:-end_tests(typeInf).
