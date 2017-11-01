# mandelbrot

FIXME: description

## Installation

Download from http://example.com/FIXME.

## Usage

    $ java -jar mandelbrot-0.1.0-standalone.jar [args]

Where `args` is either nothing, or coordinates defining the location to start at.

The starting location is given as start-real, end-real, start-imaginary, end-imaginary coordinates.
 - start-r(eal) is the left hand area bound
 - end-r is the right hand area bound
 - start-i(maginary) is the upper area bound
 - end-i is the lower area bound

 To start with a zoomed out view of the lower right corner, use:

    $ java -jar mandelbrot-0.1.0-standalone.jar 0 1 0 1