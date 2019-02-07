:- module(quadtree, [rule//1,
                     quadtree_graphictree/2
                    ]).

/*
  Quadtree shape grammar that splits a square into four quadrants,
  assigning shades.
*/

% Wrapping/unwrapped with lists is just to make it work with the
% current system of modelling L-system models as lists. Not for long!
rule([quadtree(square(black),
              square(black),
              square(black),
              square(white))]) -->
    [square(black)].
rule([quadtree(square(white),
              square(black),
              square(black),
              square(black))]) -->
    [square(white)].
% This rule makes things funky
rule([quadtree(quadtree(A, B, C, square(black)),
              TL,
              quadtree(D, square(white), F, E),
              BR)]) -->
    [quadtree(TL,
              quadtree(A, B, C, square(white)),
              quadtree(square(black), D, E, F),
              BR)].
rule([quadtree(NextTL, NextTR, NextBL, NextBR)]) -->
    [quadtree(TL, TR, BL, BR)],
    { phrase(rule([NextTL]), [TL]),
      phrase(rule([NextTR]), [TR]),
      phrase(rule([NextBL]), [BL]),
      phrase(rule([NextBR]), [BR]) }.


quadtree_graphictree(Model, GraphicTree) :-
    quadtree_graphictree(Model, quadrant(0, 0, 1), GraphicTree).

/*
  quadtree_graphictree relates a symbolic quadtree with a graphic tree
  intermediate representation. Recursively describes sub-quadtrees, partitioning
  the bounding box.
*/
quadtree_graphictree(square(white), quadrant(_Bottom, _Left, _Length), []).
quadtree_graphictree(square(black), quadrant(Left, Bottom, Length), [box(Left, Bottom, Length, Length, [fill('#000000')])]).
quadtree_graphictree(quadtree(TopLeft, TopRight, BottomLeft, BottomRight),
                     quadrant(Left, Bottom, Length),
                     GraphicTrees) :-
    HalfLength is Length / 2,
    MiddleX is Left + HalfLength,
    MiddleY is Bottom + HalfLength,
    quadtree_graphictree(TopLeft, quadrant(Left, MiddleY, HalfLength), TopLeftGraphicTrees),
    quadtree_graphictree(TopRight, quadrant(MiddleX, MiddleY, HalfLength), TopRightGraphicTrees),
    quadtree_graphictree(BottomLeft, quadrant(Left, Bottom, HalfLength), BottomLeftGraphicTrees),
    quadtree_graphictree(BottomRight, quadrant(MiddleX, Bottom, HalfLength), BottomRightGraphicTrees),
    append([TopLeftGraphicTrees, TopRightGraphicTrees, BottomLeftGraphicTrees, BottomRightGraphicTrees], GraphicTrees).
