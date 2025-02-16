# scala-parser-library
A small Scala library that can be easily used to implement parsers for custom languages.
It provides a Parser[T] parametric class, which can be used to create Parser objects that convert a String into a T type object (or a Parse Error). Using monad operations such as filter or flatmap, and combinator functions, the user can easily create parsers as combinations of smaller parsers.
For example, in the "Musiquita" file, we use the library to implement a parser for a language that represents musical notes, chords, and melodies. A note is either A, or B, etc. A "Sharp" note" is a note followed by a # symbol. We keep on going until we get to a full "Musical Piece" parser.
This project was made as part of the "Advanced Programming Techniques" course at Universidad Tecnológica Nacional, with the goal of combining functional and object-oriented programming techniques in the same codebase, in a clean and coherent way.
