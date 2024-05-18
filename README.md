# mandelbrot

![Sample](https://github.com/carcigenicate/mandelbrot/blob/master/mandelSample.png?raw=true)

## Installation

To make a jar, you'll need Leiningen.

Create a jar using:

    lein uberjar

In the directory containing `project.clj`.

Or, to just run the code without creating a jar:

    lein run

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

Once the UI opens, you can zoom and move to where ever you want.

The slider accross the bottom decides the width of the saved image. The height will be 2/3 of the chosen width.

Images are saved to a `saves` directory in the current working directory.

## Performance

This version was done quite naively in retrospect. I'm using large structures to represent each pixel, which almost certainly hurts performance. The size of the window also affects the resolution of the image being drawn each click, so make the window smaller when "moving around" to make it load faster.

I'm currently on-and-off attempting to [re-write this in Rust](https://github.com/carcigenicate/mandelbrot_explorer). I have image generation working great, but I'm finding Rust's UI libraries to be lacking at the moment, so I'm stuck until I either get good enough at Rust to use existing libraries, or better libraries come out.

## Location Information

The "location" and color-scheme used by the image is stored in the file-name, so you can't rename the image without losing that information.