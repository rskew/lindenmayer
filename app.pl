:- module(app, []).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).

:- use_module(houses).
:- use_module(svg).


:- http_handler('/', fresh_page, []).
:- http_handler('/update', update_page, []).

start_server :-
    start_server(9987).
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).


fresh_page(_Request) :-
    create_svg(5, Svg),
    % Send SVG to server wrapped in html tags
    app_htmlheader(Header),
    app_htmlbody_svg(Html, Svg),
    reply_html_page(Header, Html).


update_page(Request) :-
    member(search([iterations=IterationsAtom]), Request),
    atom_number(IterationsAtom, Iterations),
    create_svg(Iterations, Svg),
    phrase(html(Svg), Html),
    format('Content-type: text/html~n~n', []),
    print_html(Html).


create_svg(NIterations, Svg) :-
    % Generate scene by iterating L-system rules over the initial model
    InitialModel = [house(0, 1, 1)],
    iterate_l_sys(InitialModel, NIterations, Houses),
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


app_htmlheader([title(':-)'),
                element(script, [], [
    'function regenerate_image() {
         const request = new XMLHttpRequest();
         const iterations = document.getElementById("iterations").value;
         const url = "http://localhost:9987/update?iterations=" + iterations;
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
    };'
                ])
               ]).


app_htmlbody_svg(
    [element(div, [style='display:flex;flex-direction:row;'], [
                 element(div, [class=sliders, style='display:flex;flex-direction:column;'],
                         ['Iterations',
                          element(input,
                                  [type=range,
                                   min=1,
                                   max=10,
                                   value=50,
                                   id=iterations,
                                   oninput='regenerate_image();'],
                                  [])]),
                 Svg
             ])],
    [Svg]).


iterate_l_sys(Model, 0, Model).
iterate_l_sys(Model, N, ProcessedModel) :-
    NNext is N - 1,
    once(phrase(production(houses:rule, NextModel), Model)),
    iterate_l_sys(NextModel, NNext, ProcessedModel).


/*
  Run the production rules over the entire Model.
*/
production(_, []) --> [].
production(Rule, Output) -->
    call(Rule, ProcessedModel),
    { append(ProcessedModel, ProcessedRest, Output) },
    production(Rule, ProcessedRest).
