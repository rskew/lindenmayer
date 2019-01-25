#!/usr/bin/env swipl

:- use_module(postscript).
:- initialization(main, main).

/*
  Quadtree shape grammar that splits a square into four quadrants,
  assigning shades.
  The script outputs postscipt via stdout.
*/

main([]) :-
    main(['5']).
main([DepthArg|_RestArgv]) :-
    atom_number(DepthArg, Depth),
    apply_n_times(rule, [square(black)], Depth, [Model]),
    render(Model, bounding_box(500, 500), PostScript),
    write(PostScript),
    format(user_error, 'Press enter to exit~n', []),
    read_line_to_codes(user_input,_).

rule(quadtree(square(black),
              square(black),
              square(black),
              square(white))) -->
    [square(black)].
rule(quadtree(square(white),
              square(black),
              square(black),
              square(black))) -->
    [square(white)].
% This rule makes things funky
rule(quadtree(quadtree(A, B, C, square(black)),
              TL,
              quadtree(D, square(white), F, E),
              BR)) -->
    [quadtree(TL,
              quadtree(A, B, C, square(white)),
              quadtree(square(black), D, E, F),
              BR)].
rule(quadtree(NextTL, NextTR, NextBL, NextBR)) -->
    [quadtree(TL, TR, BL, BR)],
    { phrase(rule(NextTL), [TL]),
      phrase(rule(NextTR), [TR]),
      phrase(rule(NextBL), [BL]),
      phrase(rule(NextBR), [BR]) }.

/*
  quadtree_ps relates a symbolic quadtree with postscript that describes
  a rendering of it. Recursively describes sub-quadtrees, partitioning
  the bounding box.
*/
quadtree_ps(square(white), quadrant(_Bottom, _Left, _Length), []).
quadtree_ps(square(black), quadrant(Left, Bottom, Length), Box) :-
    phrase((postscript:box_path(point(Left, Bottom), Length, Length),
            [fill, '\n']),
           Box).
quadtree_ps(quadtree(TopLeft, TopRight, BottomLeft, BottomRight),
            quadrant(Left, Bottom, Length),
            Ps) :-
    HalfLength is Length / 2,
    MiddleX is Left + HalfLength,
    MiddleY is Bottom + HalfLength,
    quadtree_ps(TopLeft, quadrant(Left, MiddleY, HalfLength), TopLeftPs),
    quadtree_ps(TopRight, quadrant(MiddleX, MiddleY, HalfLength), TopRightPs),
    quadtree_ps(BottomLeft, quadrant(Left, Bottom, HalfLength), BottomLeftPs),
    quadtree_ps(BottomRight, quadrant(MiddleX, Bottom, HalfLength), BottomRightPs),
    append([TopLeftPs, TopRightPs, BottomLeftPs, BottomRightPs], Ps).


/*
  Creates the formatted postscript document.
*/
render(Model, bounding_box(Width, Height), PostScript) :-
    MinDimension is min(Width, Height),
    MinDimensionLessBorder is MinDimension - 10,
    quadtree_ps(Model,
                quadrant(10, 10, MinDimensionLessBorder),
                PostScriptModel),
    postscript:header(PsHeader),
    append([PsHeader, [0.2, setgray, '\n'], PostScriptModel], PostScriptList),
    atomic_list_concat(PostScriptList,
                       ' ',
                       PostScript).


apply_n_times(_Rule, Model, 0, Model) :- format(user_error, '~w~n', [Model]).
apply_n_times(Rule, InputModel, N, Output) :-
    N > 0,
    N1 is N - 1,
    phrase(production(Rule, ProcessedModel), InputModel),
    apply_n_times(Rule, ProcessedModel, N1, Output).


/*
  Run the production rules over the entire Model.
*/
production(_, []) --> [].
production(Rule, [ProcessedModel|ProcessedRest]) -->
    call(Rule, ProcessedModel),
    production(Rule, ProcessedRest).
