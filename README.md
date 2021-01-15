P4 Prolog
============
How To run
============
Mac:p4 gussegovia$ swipl

?- ["typeInf.plt"].

true.

?- run_tests.

% PL-Unit: typeInf ........................ done

% All 25 tests passed

true.





Stuff implemented/example test
============
1.Arithmetic/Comparison
~~~
test(typeExp_imul_F, [fail]) :- typeExp(imul(int, int), float).

test(boolTest, [nondet]):-
    typeStatement(if(3=<7,[3],[7]),T),
    assertion(T==int).
~~~
2.FunctionCalls
~~~
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct
~~~
3.Let In
~~~
test(letIn, [nondet]) :-
    deleteGVars(),
    infer([localLet(a, int, idiv(int,int), [localLet(b, float, fmul(float,float), [getVar(a,A),getVar(b,B)])])], unit),
    assertion(A==int),
    assertion(B==float).
~~~

4.If
~~~
test(simple_if, [nondet]) :-
    typeStatement( if(true, 
        [5.2], 
        [6.3]),
         T),
    assertion(T==float).
~~~

5.For
~~~
test(simple_for, [nondet]) :-   
    typeStatement(for(v, T, 7,
         [if(2 < 7, 
             [2.0], 
             [7.0])])),    
    assertion(T==float).
~~~

6.Code Blocks
~~~
test(code_block, [nondet]) :-  
      typeCode([
        if(13 < 22, [13], [22]) ,
        if(7 > 1, [7.0], [1.0])], 
        T),    
      assertion(T==float).
~~~

