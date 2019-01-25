:- module(postscript, [ellipse_path//5,
                       box_path//4
                       ]).
/*
  Generate graphics in postscript.

  This module contains the core abstraction for generating postscript.
  See test.pl for machinery to pipe generated postscript to ghostscript
  for interactive generation of graphics.
 */

header(['%!', '\n']).

ellipse_path(CenterX, CenterY, Radius, ElongationFactor, Angle) -->
    [newpath],
    [CenterX, CenterY, translate],
    [Angle, rotate],
    [ElongationFactor, 1, scale],
    [0, 0, Radius, 0, 360, arc].


box_path(Left, Bottom, Width, Height) -->
    { Top is Bottom + Height,
      Right is Left + Width },
    [newpath],
    [Left, Bottom, moveto],
    [Left, Top, lineto],
    [Right, Top, lineto],
    [Right, Bottom, lineto],
    [closepath].
