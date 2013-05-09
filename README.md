shapeless-serialization
=======================

shapeless-serialization is a [Scala](http://www.scala-lang.org) library which
takes advantage of the uber-awesome powers of [Miles Sabin's](https://github.com/milessabin)
[shapeless](https://github.com/milessabin/shapeless) library to create 
serializable (picklable) case classes using only a tad of boilerplate.

The isomorphism between a case class and an HList, and generous use of implicits
are the driving forces behind shapeless-serialization.

This library is one part in a trilogy of shapeless-based libraries that I
([William Harvey](http://www.cse.ohio-state.edu/~harveywi)) recently cooked up
to both deepen my understanding of Scala and to scratch some technical itches.
I hope you find it useful and interesting!

Example
--------------------------------

```scala
import com.github.harveywi.pickling._
import java.io._

object Temp extends App {

  // Define a case class
  case class TestString(s: String)

  // Its companion object must extend "IsoPicklableNoBP[...]"
  object TestString extends IsoPicklableNoBP[TestString] {
    // Obligatory boilerplate since the compiler doesn't know the types of
    // the 'apply' and 'unapply' methods of the case class a priori
    val isoContainer = createIsoContainer(apply _, unapply _)
  }

  // Create an instance of the class here
  val testString = TestString("Hello")

  // pickle it as a byte string
  val pickled = {
    val baos = new ByteArrayOutputStream
    val daos = new DataOutputStream(baos)
    testString.pickle(daos)
    baos.toByteArray()
  }

  // unpickle the byte string as an instance of TestString
  val unpickled = {
    val bais = new ByteArrayInputStream(pickled)
    val dais = new DataInputStream(bais)
    TestString.unpickle(dais)
  }
  
  // Yessssssss!
  assert(testString == unpickled, "Noooooooo!")
}
```

For more examples, see the test specifications [here](https://github.com/harveywi/shapeless-serialization/tree/master/src/test/com/github/harveywi/pickling).

Prerequisites
--------------------------------
This library requires Scala 2.10 and shapeless 1.2.3.

### Questions?  Comments?  Bugs?
Feel free to contact me (harveywi at cse dot ohio-state dot edu).  Thanks!

