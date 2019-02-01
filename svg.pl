:- module(svg, [graphictree_width_height_viewbox_svg/5
               ]).

/*
  Construct an SVG image.

  The main interface predicate 'graphictree_height_width_svg' matches a
  graphictree object represented as a tree of primitive graphic objects
  sich as 'box', 'triangle' etc with its SVG rendering.

  The SVG uses the same abstract XML representation as the XML library:
  - http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/sgml.html%27)
*/
:- use_module(library(pprint)).
:- use_module(library(pairs)).

/*
  Prototypical box:
    <rect id="rect1" x="100" y="100" width="200" height="200"
     stroke="green" stroke-width="3" fill="red"/>
*/

graphictree_width_height_viewbox_svg(GraphicTree,
                                     Width,
                                     Height,
                                     viewbox(VbX, VbY, VbWidth, VbHeight),
                                     [element(svg,
                                              [width=Width,
                                               height=Height,
                                               viewbox=Viewbox],
                                              SvgElements)]) :-
    format(atom(Viewbox), '~w ~w ~w ~w', [VbX, VbY, VbWidth, VbHeight]),
    graphictree_svgelements(GraphicTree, SvgElements).

% Plural!
graphictree_svgelements(GraphicTree,
                        SvgElements) :-
    maplist(graphictree_svgelement, GraphicTree, SvgElements).

% Case: box
graphictree_svgelement(
    box(X, Y, Width, Height, Styles),
    element(rect,
            [x=X, y=Y, width=Width, height=Height | StyleAttributes],
            [])) :-
    maplist(style_attributes, Styles, StyleAttributesLists),
    append(StyleAttributesLists, StyleAttributes).

% Case: translate transformation
graphictree_svgelement(
    translate(X, Y, GraphicTree),
    element(g,
            [transform=TranslateAtom],
            GraphicTreeSvg)) :-
    format(atom(TranslateAtom), 'translate(~w, ~w)', [X, Y]),
    graphictree_svgelements(GraphicTree, GraphicTreeSvg).

% Case: scale transformation
graphictree_svgelement(
    scale(X, Y, GraphicTree),
    element(g,
            [transform=ScaleAtom],
            GraphicTreeSvg)) :-
    format(atom(ScaleAtom), 'scale(~w, ~w)', [X, Y]),
    graphictree_svgelements(GraphicTree, GraphicTreeSvg).

% Case: rotate transformation
graphictree_svgelement(
    rotate(Degrees, GraphicTree),
    element(g,
            [transform=RotateAtom],
            GraphicTreeSvg)) :-
    format(atom(RotateAtom), 'rotate(~w)', [Degrees]),
    graphictree_svgelements(GraphicTree, GraphicTreeSvg).


style_attributes(fill(Color), [fill=Color]).
style_attributes(border(Color, Width), [stroke=Color, stroke-width=Width]).
