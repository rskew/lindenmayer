#!/usr/bin/env swipl

:- use_module(postscript).
:- initialization(main, main).

/*
  Fill a landscape with tiny houses, becoming a cityscape of tall towers
  as the rules are applied.
*/


/*
  Constants.
  The functors serve to make them globally accessibly under a certain name,
  as well as allow live updating of constants.
*/
window_size(bounding_box(600, 600)).
new_house_spawn_probability(0.15).
new_house_growth_factor(1.1).
split_variability(0.4).
split_growth(1.0, 1.5).
house_width_threshold(0.03).
window_width(0.3).
window_border_multiplier(0.5, 0.5).
y_offset(0.5).


main :-
    main([house(0, 1, 1)]).
main(Model) :-
    % Process the Model one further iteration
    once(phrase(production(rule, Houses), Model)),
    % Add windows to houses
    maplist(add_windows, Houses, HousesWindows),
    format(user_error, '~w~n', [HousesWindows]),
    % Compute postscript representation of houses
    window_size(BoundingBox),
    render_to_postscript(HousesWindows, BoundingBox, PostScript),
    % Write postscript to stdout
    format(user_output, 'gsave ~w grestore~n', [PostScript]),
    % User interation: q stops the iteration, anything else steps though again
    % one level deeper.
    format(user_error, 'Press enter to go deeper, q to quit~n', []),
    read_line_to_codes(user_input, Codes),
    format(user_error, '~w~n', [Codes]),
    char_code(q, QCode),
    ( Codes = [QCode|_]
    -> fail
    ; true),
    format(user_error, '~w~n', [Codes]),
    write('erasepage\n'),
    main(Houses).

/*
  Production rules
*/
% Sometimes add a new house
rule([house(NewXPos, NewWidth, NewHeight), house(XPos, Width, Height)]) -->
    [house(XPos, Width, Height)],
    { new_house_spawn_probability(SpawnProbability),
      maybe(SpawnProbability),
      new_house_growth_factor(GrowthFactor),
      NewWidth is Width * GrowthFactor,
      NewHeight is Height * GrowthFactor,
      NewXPos is XPos - Width / 2 }.

% Don't let houses get below a certain size
rule([house(XPos, Width, Height)]) -->
    [house(XPos, Width, Height)],
    { house_width_threshold(Threshold),
      Width < Threshold }.

% Split house into two smaller, narrower  houses
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
  Defines how a house is visually represented.
*/
house_ps(house(XPos, Width, Height, Windows), HousePs) :-
    maplist(window_ps, Windows, WindowsPsList),
    append(WindowsPsList, WindowsPs),
    y_offset(YOffset),
    phrase(([gsave, 0.0, setgray, XPos, YOffset, translate],
            [Width, Height, scale],
            postscript:box_path(point(0, 0), 1, 1),
            [fill],
            WindowsPs,
            roof_ps(Width, Height),
            [grestore, '\n']),
           HousePs).


window_ps(window(XPos, YPos, Width, Height), WindowPs) :-
    phrase(([1, setgray],
            postscript:box_path(point(XPos, YPos), Width, Height),
            [fill, '\n']),
           WindowPs).


roof_ps(Width, Height) -->
    { Extent is 1 + 0.25 * Width / Height },
    postscript:triangle_path(point(0, 1),
                             point(1, 1),
                             point(0.5, Extent)),
    [0.5, setgray, fill].

/*
  Creates the formatted postscript document.
*/
render_to_postscript(Houses, bounding_box(Width, Height), PostScript) :-
    maplist(house_ps, Houses, HousesPsList),
    flatten(HousesPsList, HousesPs),
    postscript:header(PsHeader),
    append([PsHeader,
            [Width, Height, scale],
            [0.2, setgray, '\n'],
            HousesPs],
           PostScriptList),
    atomic_list_concat(PostScriptList,
                       ' ',
                       PostScript).


/*
  Run the production rules over the entire Model.
*/
production(_, []) --> [].
production(Rule, Output) -->
    call(Rule, ProcessedModel),
    { append(ProcessedModel, ProcessedRest, Output) },
    production(Rule, ProcessedRest).
