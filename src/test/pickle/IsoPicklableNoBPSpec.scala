/*
 *   shapeless-serialization
 *   (c) William Harvey 2013
 *   harveywi@cse.ohio-state.edu
 *   
 *   This file is part of "shapeless-serialization".
 *
 *   shapeless-serialization is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   shapeless-serialization is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with shapeless-serialization.  If not, see <http://www.gnu.org/licenses/>.
 */

package pickle

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import shapeless._
import java.io._
import scala.util.matching.Regex

object IsoPicklableNoBPSpec {
  case class TestInt(x: Int)
  object TestInt extends IsoPicklableNoBP[TestInt] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testInt = TestInt(42)

  case class TestString(s: String)
  object TestString extends IsoPicklableNoBP[TestString] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testString = TestString("Hello")

  case class TestBoolean(b: Boolean)
  object TestBoolean extends IsoPicklableNoBP[TestBoolean] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testBooleanTrue = TestBoolean(true)
  val testBooleanFalse = TestBoolean(false)

  case class TestFloat(f: Float)
  object TestFloat extends IsoPicklableNoBP[TestFloat] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testFloat = TestFloat(123.456f)

  case class TestDouble(d: Double)
  object TestDouble extends IsoPicklableNoBP[TestDouble] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testDouble = TestDouble(789.123)
  
  case class TestShort(s: Short)
  object TestShort extends IsoPicklableNoBP[TestShort] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testShort = TestShort(100.toShort)
  
  case class TestLong(l: Long)
  object TestLong extends IsoPicklableNoBP[TestLong] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testLong = TestLong(42L)

  case class TestRegex(r: Regex)
  object TestRegex extends IsoPicklableNoBP[TestRegex] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testRegex = TestRegex("\\s+".r)

  case class TestChar(c: Char)
  object TestChar extends IsoPicklableNoBP[TestChar] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testChar = TestChar('x')

  case class TestFile(f: File)
  object TestFile extends IsoPicklableNoBP[TestFile] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testFile = TestFile(new File("/dev/null"))

  case class TestOption(x: Option[Int])
  object TestOption extends IsoPicklableNoBP[TestOption] {
    val isoContainer = createIsoContainer.apply1(apply _, unapply _)
  }
  val testOptionSome = TestOption(Some(42))
  val testOptionNone = TestOption(None)

  case class TestTuple2(t: (Int, String))
  object TestTuple2 extends IsoPicklableNoBP[TestTuple2] {
    val isoContainer = createIsoContainer.apply1(apply _, unapply _)
  }
  val testTuple2 = TestTuple2((42, "Hello"))
  
  case class TestArray(arr: Array[String])
  object TestArray extends IsoPicklableNoBP[TestArray] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testArray = TestArray(Array("Hello", "World"))
  
  case class TestSeq(seq: Seq[Long])
  object TestSeq extends IsoPicklableNoBP[TestSeq] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testSeq = TestSeq(Seq(1L, 2L, 3L))
  
  case class TestMap(map: Map[String, Double])
  object TestMap extends IsoPicklableNoBP[TestMap] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testMap = TestMap(Map("Hello" -> 42.0, "World" -> 100.0))
  
  // Here's some more complicated test cases
  case class TestLotsOfParameters(xInt: Int, xString: String, xLong: Long, xOptionFloat: Option[Float], xSeqDouble: Seq[Double])
  object TestLotsOfParameters extends IsoPicklableNoBP[TestLotsOfParameters] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testLotsOfParameters = TestLotsOfParameters(42, "Hello", 100L, Some(12f), Seq(1.0, 2.0, 3.0, 4.0, 5.0))
  
  case class TestNestedCaseClasses1(testInt: TestInt, testDouble: TestDouble)
  object TestNestedCaseClasses1 extends IsoPicklableNoBP[TestNestedCaseClasses1] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testNestedCaseClasses1 = TestNestedCaseClasses1(TestInt(42), TestDouble(100.0))
  
  case class TestNestedCaseClasses2(testNestedCaseClasses1: TestNestedCaseClasses1)
  object TestNestedCaseClasses2 extends IsoPicklableNoBP[TestNestedCaseClasses2] {
    val isoContainer = createIsoContainer.apply1(apply _, unapply _)
  }
  val testNestedCaseClasses2 = TestNestedCaseClasses2(testNestedCaseClasses1)
  
  case class TestLotsOfParameters2(testOption: TestOption, testTuple2: TestTuple2)
  object TestLotsOfParameters2 extends IsoPicklableNoBP[TestLotsOfParameters2] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }
  val testLotsOfParameters2 = TestLotsOfParameters2(TestOption(Some(100)), TestTuple2((100, "asdf")))
  
  case class TestNestedCaseClasses3(testLotsOfParameters2: TestLotsOfParameters2)
  object TestNestedCaseClasses3 extends IsoPicklableNoBP[TestNestedCaseClasses3] {
    val isoContainer = createIsoContainer.apply1(apply _, unapply _)
  }
  val testNestedCaseClasses3 = TestNestedCaseClasses3(TestLotsOfParameters2(TestOption(Some(42)), TestTuple2((42, "Hello"))))

  def pickleThenUnpickle[T](t: T, unpickle: DataInputStream => T)(implicit u: T => Picklable): T = {
    val pickled = {
      val baos = new ByteArrayOutputStream
      val daos = new DataOutputStream(baos)
      t.pickle(daos)
      baos.toByteArray()
    }

    val unpickled = {
      val bais = new ByteArrayInputStream(pickled)
      val dais = new DataInputStream(bais)
      unpickle(dais)
    }

    unpickled
  }

}

class IsoPicklableNoBPSpec extends FlatSpec with ShouldMatchers {
  import IsoPicklableNoBPSpec._
  
  def makeTest[T](t: T, msg: String, unpickler: DataInputStream => T)(implicit u: T => Picklable) = {
    s"$msg ($t)" should "pickle and unpickle properly" in {
      t match {
        case TestRegex(regex) => t.toString should equal(pickleThenUnpickle(t, unpickler).toString)
        case TestArray(array: Array[String]) => 
          val unpickled = pickleThenUnpickle(t, unpickler).asInstanceOf[TestArray]
          println(array.mkString(","))
          println(unpickled.arr.mkString(","))
          for ((x, y) <- array.iterator.zip(unpickled.arr.iterator)) {
            x should equal(y)
          }
        case _ => t should equal(pickleThenUnpickle(t, unpickler))
      }
    }
  }
  
  def makeSimpleTest[T](t: T, unpickler: DataInputStream => T)(implicit u: T => Picklable) = makeTest(t, "A case class with a single parameter", unpickler)
  def makeTrickyTest[T](t: T, unpickler: DataInputStream => T)(implicit u: T => Picklable) = makeTest(t, "A complicated case class with many/nested parameters", unpickler)
  
  makeSimpleTest(testInt, TestInt.unpickle _)
  makeSimpleTest(testString, TestString.unpickle _)
  makeSimpleTest(testBooleanTrue, TestBoolean.unpickle _)
  makeSimpleTest(testBooleanFalse, TestBoolean.unpickle _)
  makeSimpleTest(testFloat, TestFloat.unpickle _)
  makeSimpleTest(testDouble, TestDouble.unpickle _)
  makeSimpleTest(testShort, TestShort.unpickle _)
  makeSimpleTest(testLong, TestLong.unpickle _)
  makeSimpleTest(testRegex, TestRegex.unpickle _)
  makeSimpleTest(testChar, TestChar.unpickle _)
  makeSimpleTest(testFile, TestFile.unpickle _)
  makeSimpleTest(testOptionSome, TestOption.unpickle _)
  makeSimpleTest(testOptionNone, TestOption.unpickle _)
  makeSimpleTest(testTuple2, TestTuple2.unpickle _)
  makeSimpleTest(testArray, TestArray.unpickle _)
  makeSimpleTest(testSeq, TestSeq.unpickle _)
  makeSimpleTest(testMap, TestMap.unpickle _)
  
  makeTrickyTest(testLotsOfParameters, TestLotsOfParameters.unpickle _)
  makeTrickyTest(testLotsOfParameters2, TestLotsOfParameters2.unpickle _)
  makeTrickyTest(testNestedCaseClasses1, TestNestedCaseClasses1.unpickle _)
  makeTrickyTest(testNestedCaseClasses2, TestNestedCaseClasses2.unpickle _)
  makeTrickyTest(testNestedCaseClasses3, TestNestedCaseClasses3.unpickle _)

}