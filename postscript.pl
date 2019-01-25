:- module(postscript, [ellipse_path//5,
                       box_path//4,
                       triangle_path//6
                       ]).
/*
  Generate graphics in postscript.

  TODO: design an interface that stops postscript from leaking through.
 */

header(['%!', '\n']).

ellipse_path(point(CenterX, CenterY), Radius, ElongationFactor, Angle) -->
    [newpath],
    [CenterX, CenterY, translate],
    [Angle, rotate],
    [ElongationFactor, 1, scale],
    [0, 0, Radius, 0, 360, arc].


box_path(point(Left, Bottom), Width, Height) -->
    { Top is Bottom + Height,
      Right is Left + Width },
    [newpath],
    [Left, Bottom, moveto],
    [Left, Top, lineto],
    [Right, Top, lineto],
    [Right, Bottom, lineto],
    [closepath].


triangle_path(point(AX, AY),
              point(BX, BY),
              point(CX, CY)) -->
    [newpath,
     AX, AY, moveto,
     BX, BY, lineto,
     CX, CY, lineto,
     closepath].
