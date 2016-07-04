package misc

import scala.annotation.Annotation

/**
  * Informal (and unchecked) effect annotation: pure function.
  * Meaning: the function has no side effects, and each time it is called
  * with the same arguments, it will give the same result.
  * (The result may not be referentially equal though.)
  */
case class Pure() extends Annotation