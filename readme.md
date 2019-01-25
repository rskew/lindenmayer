Shape grammars!

The `lindenmayer.pl` script computes a given number of passes using the rules in `production_rules.pl`, which also defines a way to render the generated model to a postscript file. The postscript is output via stdout, allowing visualisation via Ghostscript:
`example_quadtree.pl 13 | gs`

You'll need SWI-Prolog and a postscript viewer.

![Default Output](https://github.com/rskew//example.eps)
