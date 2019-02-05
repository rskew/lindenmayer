Example usage: `./quadtree.pl 13 | gs`

You'll need a postscript viewer for the quadtree example.

![Quadtree with some funky rules](https://github.com/rskew/lindenmayer/raw/master/quadtree.png)

The houses example runs an HTTP server and serves an SVG to your browser. Run the swipl top level, enter `?- start_server.` (optionally pass the port number as `start_server(PortNumber)`), then open `localhost:9987` in your browser (or whatever port number you passed in).

![Cityscape](https://github.com/rskew/lindenmayer/raw/master/houses.png)

![Deepscape](https://github.com/rskew/lindenmayer/raw/master/many_houses.png)
