# The Ptera Keyboard
This is a "fork" of the [Dactyl-Manuform](https://github.com/abstracthat/dactyl-manuform), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

The main goals are to make it a bit smoother, easier to fit to your hand, and also as a learning project

## Assembly

### Generating a Design

**Setting up the Clojure environment**
* [Install the Clojure runtime](https://clojure.org)
* [Install the Leiningen project manager](http://leiningen.org/)
* [Install OpenSCAD](http://www.openscad.org/)

**Generating the design**
* Run `lein repl`
* Load the file `(load-file "src/ptera.clj")`
* This will regenerate the `things/*.scad` files
* Use OpenSCAD to open a `.scad` file.
* Make changes to design, repeat `load-file`, OpenSCAD will watch for changes and rerender.
* When done, use OpenSCAD to export STL files

## Assembly
For tips on actually building this thing, please refer back to [the original page](https://github.com/abstracthat/dactyl-manuform), as anything I say here would just be copied anyway

## License
Copyright © 2024 lightningund

This entire project is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE). Have fun!
