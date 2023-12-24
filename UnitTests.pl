:- consult("../TendasEArvores").

% Helper predicate to count the number of variables in a nested list
count_vars(List, Count) :-
    flatten(List, FlatList),
    include(var, FlatList, Vars),
    length(Vars, Count).

% Test for vizinhanca/2
:- begin_tests(vizinhanca).
test(vizinhanca_1) :-
    vizinhanca((3, 4), L),
    assertion(maplist(nonvar, L)),
    assertion(L == [(2,4),(3,3),(3,5),(4,4)]).

test(vizinhanca_2) :-
    vizinhanca((3, 1), L),
    assertion(maplist(nonvar, L)),
    assertion(L == [(2,1),(3,0),(3,2),(4,1)]).

:- end_tests(vizinhanca).

% Test for vizinhancaAlargada/2
:- begin_tests(vizinhancaAlargada).
test(vizinhancaAlargada_1) :-
    vizinhancaAlargada((3, 4), L),
    assertion(maplist(nonvar, L)),
    assertion(L == [(2,3),(2,4),(2,5),(3,3),(3,5),(4,3),(4,4),(4,5)]).

:- end_tests(vizinhancaAlargada).

% Test for todasCelulas/2
:- begin_tests(todasCelulas).
test(todasCelulas_1) :-
    puzzle(6-13, (T, _, _)),
    todasCelulas(T, TodasCelulas),
    assertion(maplist(nonvar, TodasCelulas)),
    assertion(TodasCelulas == [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
                    (2,1),(2,2),(2,3),(2,4),(2,5),(2,6),
                    (3,1),(3,2),(3,3),(3,4),(3,5),(3,6),
                    (4,1),(4,2),(4,3),(4,4),(4,5),(4,6),
                    (5,1),(5,2),(5,3),(5,4),(5,5),(5,6),
                    (6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]).

:- end_tests(todasCelulas).

% Test for todasCelulas/3
:- begin_tests(todasCelulas_with_object).
test(todasCelulas_with_object_1) :-
    puzzle(6-13, (T, _, _)),
    todasCelulas(T, TodasCelulas, a),
    assertion(maplist(nonvar, TodasCelulas)),
    assertion(TodasCelulas == [(1,5),(2,1),(2,6),(3,4),(4,5),(5,3),(6,3)]).

:- end_tests(todasCelulas_with_object).

% Test for calculaObjectosTabuleiro/4
:- begin_tests(calculaObjectosTabuleiro).
test(calculaObjectosTabuleiro_1) :-
    puzzle(6-13, (T, _, _)),
    calculaObjectosTabuleiro(T, CLinhas, CColunas, a),
    assertion(maplist(nonvar, CLinhas)),
    assertion(maplist(nonvar, CColunas)),
    assertion(CLinhas == [1,2,1,1,1,1]),
    assertion(CColunas == [1,0,2,1,2,1]).

test(calculaObjectosTabuleiro_2) :-
    puzzle(6-13, (T, _, _)),
    calculaObjectosTabuleiro(T, CLinhas, CColunas, _),
    assertion(maplist(nonvar, CLinhas)),
    assertion(maplist(nonvar, CColunas)),
    assertion(CLinhas == [5,4,5,5,5,5]),
    assertion(CColunas == [5,6,4,5,4,5]).

:- end_tests(calculaObjectosTabuleiro).

% Test for celulaVazia/2
:- begin_tests(celulaVazia).
test(celulaVazia_1) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (1, 2))).

test(celulaVazia_2) :-
    puzzle(6-13, (T, _, _)),
    assertion(\+ celulaVazia(T, (1, 5))). % Negate the function call to ensure the test fails.

test(celulaVazia_3) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (0, 5))).

test(celulaVazia_4) :-
    puzzle(6-13, (T, _, _)),
    assertion(celulaVazia(T, (1, 7))).

:- end_tests(celulaVazia).

% Test for insereObjectoCelula/3
:- begin_tests(insereObjectoCelula).
test(insereObjectoCelula_1) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoCelula(T, r, (1,1)),
    count_vars(T, CountAfter),
    Expected = [[r, X, a, Y], [Z, W, V, U], [a, a, a, a], [S, R, a, Q]],
    count_vars(Expected, ExpectedCount),
    assertion(T = Expected),
    assertion(maplist(var, [X, Y, Z, W, V, U, S, R, Q])),
    assertion(CountAfter =:= ExpectedCount).

test(insereObjectoCelula_2) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoCelula(T, r, (1,3)),
    Expected = [[X, Y, a, Z], [W, V, U, S], [a, a, a, a], [R, J, a, O]],
    count_vars(Expected, ExpectedCount),
    count_vars(T, CountAfter),
    assertion(T = Expected),
    assertion(maplist(var, [X, Y, Z, W, V, U, S, R, J, O])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(insereObjectoCelula).

% Test for insereObjectoEntrePosicoes/4
:- begin_tests(insereObjectoEntrePosicoes).
test(insereObjectoEntrePosicoes_1) :-
    T = [[_, _, a, _], [_, _, _, _], [a, a, a, a], [_, _, a, _]],
    insereObjectoEntrePosicoes(T, r, (1,1), (1,4)),
    count_vars(T, CountAfter),
    Expected = [[r, r, a, r], [X, Y, Z, W], [a, a, a, a], [V, U, a, S]],
    count_vars(Expected, ExpectedCount),
    assertion(T = Expected),
    assertion(maplist(var, [X, Y, Z, W, V, U, S])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(insereObjectoEntrePosicoes).

% Test for relva/1
:- begin_tests(relva).
test(relva_1) :-
    puzzle(6-14, P),
    relva(P),
    P = (Board, _, _),
    count_vars(Board, CountAfter),
    Expected = (
            [
                [X, a, Y, a, Z, r],
                [a, r, r, r, r, r],
                [A, B, C, D, E, r],
                [F, G, a, a, H, r],
                [I, J, K, L, M, r],
                [N, a, O, U, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    Expected = (ExpectedBoard, _, _),
    count_vars(ExpectedBoard, ExpectedCount),
    assertion(P = Expected),
    assertion(maplist(var, [X, Y, Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, U])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(relva).

% Test for inacessiveis/1
:- begin_tests(inacessiveis).
test(inacessiveis_1) :-
    puzzle(6-14, (T, _, _)),
    inacessiveis(T),
    count_vars(T, CountAfter),
    Expected = [
            [X, a, Y, a, Z, r],
            [a, A, r, B, r, r],
            [C, r, D, E, r, r],
            [r, F, a, a, G, r],
            [r, H, I, J, K, r],
            [L, a, M, N, a, O]
        ],
    count_vars(Expected, ExpectedCount),
    assertion(T = Expected),
    assertion(maplist(var, [X, Y, Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(inacessiveis).

% Test for aproveita/1
:- begin_tests(aproveita).
test(aproveita_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    P = (Board, _, _),
    count_vars(Board, CountAfter),
    Expected = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [X, Y, Z, W, V, r],
                [U, T, a, a, S, r],
                [R, Q, E, O, N, r],
                [M, a, L, K, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    Expected = (ExpectedBoard, _, _),
    count_vars(ExpectedBoard, ExpectedCount),
    assertion(P = Expected),
    assertion(maplist(var, [X, Y, Z, W, V, U, T, S, R, Q, E, O, N, M, L, K])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(aproveita).

% Test for unicaHipotese/1
:- begin_tests(unicaHipotese).
test(unicaHipotese_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    relva(P),
    unicaHipotese(P),
    P = (Board, _, _),
    count_vars(Board, CountAfter),
    Expected = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [X, Y, r, Z, W, r],
                [V, t, a, a, U, r],
                [F, S, r, R, Q, r],
                [L, a, r, O, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    Expected = (ExpectedBoard, _, _),
    count_vars(ExpectedBoard, ExpectedCount),
    assertion(P = Expected),
    assertion(maplist(var, [X, Y, Z, W, V, U, F, S, R, Q, L, O])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(unicaHipotese).

% Test for limpaVizinhancas/1
:- begin_tests(limpaVizinhancas).
test(limpaVizinhancas_1) :-
    puzzle(6-14, P),
    relva(P),
    aproveita(P),
    relva(P),
    unicaHipotese(P),
    limpaVizinhancas(P),
    P = (Board, _, _),
    count_vars(Board, CountAfter),
    Expected = (
            [
                [t, a, t, a, t, r],
                [a, r, r, r, r, r],
                [r, r, r, X, Y, r],
                [r, t, a, a, Z, r],
                [r, r, r, A, B, r],
                [C, a, r, D, a, r]
            ],
            [3, 0, 1, 1, 1, 1],
            [2, 1, 1, 1, 2, 0]
        ),
    Expected = (ExpectedBoard, _, _),
    count_vars(ExpectedBoard, ExpectedCount),
    assertion(P = Expected),
    assertion(maplist(var, [X, Y, Z, A, B, C, D])),
    assertion(CountAfter =:= ExpectedCount).

:- end_tests(limpaVizinhancas).

% Test for valida/2
:- begin_tests(valida).
test(valida_1) :-
    Trees = [(1,2),(1,4),(2,1),(4,3),(4,4),(6,2),(6,5)],
    Tents = [(1,1),(1,3),(1,5),(3,4),(4,2),(5,5),(6,1)],
    assertion(valida(Trees, Tents)).

test(valida_2) :-
    Trees = [(1,1),(1,3)],
    Tents = [(1,2),(1,4)],
    assertion(valida(Trees, Tents)).

test(valida_3) :-
    Trees = [(1,2),(1,4),(2,1),(3,4),(4,1),(4,3)],
    Tents = [(1,1),(1,3),(2,4),(3,1),(4,2),(4,4)],
    assertion(valida(Trees, Tents)).

test(valida_4) :-
    Trees = [
                (1,2),(1,4),(1,6),
                (2,1),(2,7),
                (3,4),
                (4,1),(4,3),(4,5),(4,7),
                (5,4),
                (6,1),(6,7),
                (7,2),(7,4),(7,6)
            ],
    Tents = [
                (1,1),(1,3),(1,5),(1,7),

                (3,1),(3,3),(3,5),(3,7),

                (5,1),(5,3),(5,5),(5,7),

                (7,1),(7,3),(7,5),(7,7)
            ],
    assertion(valida(Trees, Tents)).

test(valida_5) :-
    Trees = [
                (1,2),(1,6),
                (2,1),(2,3),(2,5),(2,7),
                (3,2),(3,6),
                
                (5,2),(5,4),(5,6),
                (6,1),(6,7),
                (7,2),(7,4),(7,6)
            ],
    Tents = [
                (1,1),(1,3),(1,5),(1,7),
                
                (3,1),(3,3),(3,5),(3,7),

                (5,1),(5,3),(5,5),(5,7),

                (7,1),(7,3),(7,5),(7,7)
            ],
    assertion(valida(Trees, Tents)).

test(valida_6) :-
    Trees = [(1,1),(1,3)],
    Tents = [(1,1),(1,3)],
    assertion(\+ valida(Trees, Tents)).

test(valida_7) :-
    Trees = [(1,1),(1,4)],
    Tents = [(1,2),(1,4)],
    assertion(\+ valida(Trees, Tents)).

test(valida_8) :-
    Trees = [(1,2),(2,1),(2,3),(3,2)],
    Tents = [(1,1),(1,3),(3,1),(3,3)],
    assertion(valida(Trees, Tents)).

test(valida_9) :-
    assertion(\+ valida([(1, 1), (1, 2)], [(1, 1), (1, 2)])).

test(valida_10) :-
    assertion(\+ valida([(1,1),(1,3),(3,2)],[(1,2),(3,1),(3,3)])).

:- end_tests(valida).

:- begin_tests(puzzle).
test(puzzle_6_13) :-
    puzzle(6-13, P),
    resolve(P),
    sol(6-13, P).

test(puzzle_6_14) :-
    puzzle(6-14, P),
    resolve(P),
    sol(6-14, P).

test(puzzle_8_1) :-
    puzzle(8-1, P),
    resolve(P),
    sol(8-1, P).

% Extra tests

puzzleextra(8-222, 
([
[_,_,a,_,_,_,_,_],
[a,_,_,_,_,_,_,a],
[_,a,_,_,_,a,_,_],
[a,_,_,_,_,_,a,_],
[_,a,_,_,a,_,_,a],
[_,_,_,_,_,_,_,_],
[_,_,_,a,_,_,a,_],
[_,a,_,_,_,_,_,_]
], [2, 1, 2, 1, 3, 1, 1, 2], [2, 2, 2, 1, 0, 2, 1, 3])).

solextra(8-222, ([
[r,t,a,r,r,r,r,t],
[a,r,r,r,r,t,r,a],
[t,a,t,r,r,a,r,r],
[a,r,r,r,r,r,a,t],
[t,a,t,r,a,t,r,a],
[r,r,r,r,r,r,r,t],
[r,t,r,a,r,r,a,r],
[r,a,r,t,r,r,t,r]], 
[2, 1, 2, 1, 3, 1, 1, 2], [2, 2, 2, 1, 0, 2, 1, 3])
).

test(puzzle_extra_8_222) :-
    puzzleextra(8-222, P),
    resolve(P),
    solextra(8-222, P).

:- end_tests(puzzle).