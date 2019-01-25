#!/usr/bin/env swipl

:- use_module(postscript).
:- initialization(main, main).

/*
  Fill a landscape with tiny houses.
*/

main([]) :-
    phrase(production(rule, Houses), [house(100, 50)]),
    render(Houses, bounding_box(500, 500), PostScript),
    write(PostScript),
    format(user_error, 'Press enter to exit~n', []),
    read_line_to_codes(user_input,_).

rule([house(LeftHeight), house(RightHeight)]) -->
    [house(Height)],
    { random(Split),
      LeftHeight is Height * Split,
      RightHeight is Height * (1 - Split) }.

% Make this DCG, consumes list of houses, creates list of postscript
houses_ps(_XPos, [], []).
houses_ps(XPos,
          [house(Height)|RestHouses],
          [HousePsShiftScale|RestHousesPs]) :-
    house_ps(HousePs),
    append([XPos, translate, Height, scale], HousePs, HousePsShiftScale),
    NextX is XPos + Height,
    houses_ps(NextX, RestHouses, RestHousesPs).

house_ps(HousePs) :-
    phrase((postscript:box_path(0, 0, 1, 1),
            [fill, '\n']),
           HousePs).

/*
  Creates the formatted postscript document.
*/
render(Houses, bounding_box(Width, Height), PostScript) :-
    MinDimension is min(Width, Height),
    MinDimensionLessBorder is MinDimension - 10,
    houses_ps(0, Houses, HousesPs),
    postscript:header(PsHeader),
    append(HousesPs, PostScriptModel),
    append([[MinDimensionLessBorder, scale],
            PsHeader,
            [0.2, setgray, '\n'],
            PostScriptModel],
           PostScriptList),
    atomic_list_concat(PostScriptList,
                       ' ',
                       PostScript).


/*
  Run the production rules over the entire Model.
*/
production(_, []) --> [].
production(Rule, ProcessedAll) -->
    call(Rule, ProcessedModel),
    { append(ProcessedModel, ProcessedRest, ProcessedAll) },
    production(Rule, ProcessedRest).
