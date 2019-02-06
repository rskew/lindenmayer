:- module(app, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).

:- use_module(houses).
:- use_module(quadtree).
:- use_module(svg).


:- http_handler('/houses/', fresh_page(house_svg), []).
:- http_handler('/houses/update', update_page(house_svg), []).
:- http_handler('/quadtree/', fresh_page(quadtree_svg), []).
:- http_handler('/quadtree/update', update_page(quadtree_svg), []).

start_server :-
    start_server(9987).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).


fresh_page(CreateSvg, Request) :-
    call(CreateSvg, 5, Svg),

    member(protocol(Protocol), Request),
    member(host(Host), Request),
    member(port(Port), Request),
    member(path(Path), Request),
    format(atom(UpdateRoute), '~w://~w:~w~wupdate', [Protocol, Host, Port, Path]),

    % Send to server wrapped in html tags
    app_htmlheader(Header, UpdateRoute),
    app_htmlbody_svg(Html, Svg),
    reply_html_page(Header, Html).


update_page(CreateSvg, Request) :-
    member(search([iterations=IterationsAtom]), Request),
    atom_number(IterationsAtom, Iterations),
    call(CreateSvg, Iterations, Svg),
    phrase(html(Svg), Html),
    format('Content-type: text/html~n~n', []),
    print_html(Html).


house_svg(NIterations, Svg) :-
    % Generate scene by iterating L-system rules over the initial model
    InitialModel = [house(0, 1, 1)],
    iterate_l_sys(houses:rule, InitialModel, NIterations, Houses),
    % Add windows to houses
    maplist(houses:add_windows, Houses, HousesWindows),
    % Convert to graphic-tree intermediate representation
    maplist(houses:house_graphictree, HousesWindows, GraphicTreeList),
    append(GraphicTreeList, GraphicTree),
    % Convert graphic-tree to SVG, doing a funky rotation to make
    % the coordinates orientation the familiar Cartesian one
    svg:graphictree_width_height_viewbox_svg(
            [rotate(180, GraphicTree)],
            2000, 1000,
            viewbox(-0.75, -0.75, 1, 1),
            Svg).


quadtree_svg(NIterations, Svg) :-
    % Generate scene by iterating L-system rules over the initial model
    InitialModel = square(black),
    iterate_l_sys(quadtree:rule, InitialModel, NIterations, Squares),
    % Convert to graphic-tree intermediate representation
    maplist(quadtree:quadtree_graphictree, Squares, GraphicTreeList),
    append(GraphicTreeList, GraphicTree),
    % Convert graphic-tree to SVG
    svg:graphictree_width_height_viewbox_svg(
            [rotate(180, GraphicTree)],
            2000, 1000,
            viewbox(-0.75, -0.75, 1, 1),
            Svg).


app_htmlheader([title(':-)'), element(script, [], HTTP_HEADER)], UpdateRoute) :-
    format(atom(HTTP_HEADER),
    'function regenerate_image() {
         const request = new XMLHttpRequest();
         const iterations = document.getElementById("iterations").value;
         const url = "~w?iterations=" + iterations;
         console.log(url);
         request.open("GET", url);
         request.send();
         request.onreadystatechange=(e)=>{
             var old_svg = document.getElementsByClassName("svg")[0];
             var new_svg_wrapped = document.createElement("div");
             new_svg_wrapped.innerHTML = request.responseText.trim();
             var new_svg = new_svg_wrapped.getElementsByClassName("svg")[0];
             old_svg.parentNode.insertBefore(new_svg, old_svg);
             old_svg.parentNode.removeChild(old_svg);
         };
    };', [UpdateRoute]).


app_htmlbody_svg(
    [element(div, [style='display:flex;flex-direction:row;'], [
                 element(div, [class=sliders, style='display:flex;flex-direction:column;'],
                         ['Iterations',
                          element(input,
                                  [type=range,
                                   min=1,
                                   max=7,
                                   value=50,
                                   id=iterations,
                                   oninput='regenerate_image();'],
                                  [])]),
                 Svg
             ])],
    [Svg]).


iterate_l_sys(_Rule, Model, 0, Model).
iterate_l_sys(Rule, Model, N, ProcessedModel) :-
    NNext is N - 1,
    once(phrase(production(Rule, [NextModel]), [Model])),
    iterate_l_sys(Rule, NextModel, NNext, ProcessedModel).


/*
  Run the production rules over the entire Model.
*/
production(_, []) --> [].
production(Rule, Output) -->
    call(Rule, ProcessedModel),
    %{ append(ProcessedModel, ProcessedRest, Output) },
    production(Rule, ProcessedRest).
