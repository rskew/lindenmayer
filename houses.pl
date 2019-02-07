:- module(houses, [rule//1,
                  add_windows/2,
                  house_graphictree/2
                 ]).

/*
  Procedural trees :-)
*/

/*
  Constants.
  The functors serve to make them globally accessibly under a certain name,
  as well as allow live updating of constants.
*/
window_size(bounding_box(600, 600)).
new_house_spawn_probability(0.12).
new_house_growth_factor(0.7).
split_variability(0.4).
split_growth(1.0, 1.5).
house_width_threshold(0.00).
window_width(0.3).
window_border_multiplier(0.5, 0.5).
y_offset(0.5).


/*
  Production rules
*/
% Sometimes add a new house
rule([house(XPos, Width, Height), house(NewXPos, NewWidth, NewHeight)]) -->
    [house(XPos, Width, Height)],
    { new_house_spawn_probability(SpawnProbability),
      maybe(SpawnProbability),
      new_house_growth_factor(GrowthFactor),
      NewWidth is Width / 2,
      NewHeight is Height * GrowthFactor,
      NewXPos is XPos + Width * 1 / 9 }.

% Don't let houses get below a certain size
rule([house(XPos, Width, Height)]) -->
    [house(XPos, Width, Height)],
    { house_width_threshold(Threshold),
      Width < Threshold }.

% Split house into two smaller, narrower houses
rule([house(LeftXPos, LeftWidth, LeftHeight), house(RightXPos, RightWidth, RightHeight)]) -->
    [house(XPos, Width, Height)],
    { random_split(Split),
      split_growth(WidthGrowth, HeightGrowth),
      LeftWidth is Width * Split * WidthGrowth,
      RightWidth is Width * (1 - Split) * WidthGrowth,
      LeftHeight is Height * Split * HeightGrowth,
      RightHeight is Height * (1 - Split) * HeightGrowth,
      XShift is Width * (WidthGrowth - 1) / 2,
      LeftXPos is XPos - XShift,
      RightXPos is XPos + LeftWidth + 2 * XShift }.


/*
  Narrow the random number with which to split the house so it's
  closer to 50-50.
*/
random_split(Split) :-
    random(WideSplit),
    split_variability(Width),
    Split is (1 - Width) / 2 + Width * WideSplit.


/*
  Windows can be added to houses once they're already placed in the model.
*/
add_windows(house(XPos, Width, Height), house(XPos, Width, Height, Windows)) :-
    house_windows(house(XPos, Width, Height), Windows).


% The taller the building is, the more windows it should have.
house_windows(house(_XPos, Width, Height), Windows) :-
    window_width(WindowWidth),
    NRows is floor(2 * Height / Width),
    WindowHeight is 2 * WindowWidth / NRows,
    window_border_multiplier(_XBorderMult, YBorderMult),
    findall(WindowPair,
            (between(1, NRows, Row),
             YPos is YBorderMult * WindowWidth + (Row - 1) * (WindowHeight * 4 / 3),
             window_row(YPos, WindowHeight, WindowPair)),
            WindowPairs),
    append(WindowPairs, Windows).


window_row(YPos, Height, [window(LeftXPos, YPos, Width, Height),
                          window(RightXPos, YPos, Width, Height)]) :-
    window_border_multiplier(XBorderMult, _YBorderMult),
    window_width(Width),
    RightXPos is 1 - (XBorderMult * Width + Width),
    LeftXPos is XBorderMult * Width.


/*
  Convert house model with windows into a tree of abstract shapes
  and operations as an intermediate representation.
*/
house_graphictree(
    house(XPos, Width, Height, Windows),
    [translate(
         XPos, 0,
         [box(0, 0, Width, Height, [fill('#000000')]),
          scale(Width, Height, WindowGraphicTrees)])]) :-
    maplist(window_graphictree, Windows, WindowGraphicTreesList),
    append(WindowGraphicTreesList, WindowGraphicTrees).


window_graphictree(window(XPos, YPos, Width, Height),
                   [box(XPos, YPos, Width, Height, [fill('#ffffff')])]).
