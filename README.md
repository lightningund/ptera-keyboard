# The Ptera Keyboard
This is a "fork" of the [Dactyl-Manuform](https://github.com/abstracthat/dactyl-manuform), a parameterized, split-hand, concave, columnar, ergonomic keyboard.

I have ported the entire thing over from clojure into raw SCAD language, to make it way easier to use.

The main goals are to make it a bit smoother, easier to fit to your hand, and also as a learning project

## Assembly
For tips on actually building this thing, please refer back to [the original page](https://github.com/abstracthat/dactyl-manuform) for the bulk of it, but here are some tips I have found to make assembly a bit smoother:

- Connect the rows and columns using strips of copper tape instead of wires or diodes. This makes it way cleaner underneath
- The scad file has a module called "switch_holder" which is designed to go under every single keyswitch. This keeps them from popping out when you pull of a cap

## License
Copyright © 2024 lightningund

This entire project is distributed under the [GNU AFFERO GENERAL PUBLIC LICENSE Version 3](LICENSE). Have fun!
