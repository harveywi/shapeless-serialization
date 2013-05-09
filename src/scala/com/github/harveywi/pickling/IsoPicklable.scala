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

package com.github.harveywi.pickling

import java.io._
import shapeless._
import shapeless.TypeOperators._
import language.implicitConversions

/**
 * Mix this in to the companion object of a case class `C` to
 * enable moderate-boilerplate (un)pickling of `C`.  Note that the `HList` type
 * signature must be provided as a parameter; hence [[com.github.harveywi.pickling.IsoPicklableNoBP]]
 * seems to be more convenient to use.  I've kept it around for historical reasons ("Don't be fooled by the
 * rocks that I got \\ I'm still Jenny from the block!" and so on and so forth...).
 * 
 * @example {{{
 * 
 * // Define a case class
 * case class TestLotsOfParameters(xInt: Int, xString: String, xLong: Long, xOptionFloat: Option[Float], xSeqDouble: Seq[Double])
 * 
 * // Mix in the IsoPicklable trait.  Note that hyoooge HList type signature at the end!
 * object TestLotsOfParameters extends IsoPicklable[TestLotsOfParameters, Int :: String :: Long :: Option[Float] :: Seq[Double] :: HNil] {
 *   // Define the isomorphism between the case class and a corresponding HList
 *   implicit val iso = IsoPicklable.createIso(apply _, unapply _)
 * }
 * 
 * // Create instances of the case class as usual
 * val testLotsOfParameters = TestLotsOfParameters(42, "Hello", 100L, Some(12f), Seq(1.0, 2.0, 3.0, 4.0, 5.0))
 * 
 * // The case class instance can now pickle itself into a DataOutputStream.  Here, it pickles itself into
 * // a byte array
 * val pickled = {
 *   val baos = new ByteArrayOutputStream
 *   val daos = new DataOutputStream(baos)
 *   testLotsOfParameters.pickle(daos)
 *   baos.toByteArray()
 * }
 *
 * // unpickle the byte array as an instance of your case class
 * val unpickled = {
 *   val bais = new ByteArrayInputStream(pickled)
 *   val dais = new DataInputStream(bais)
 *   TestLotsOfParameters.unpickle(dais)
 * }
 *
 * // Yessssssss!
 * assert(testLotsOfParameters == unpickled, "Noooooooo!")
 *  }}}
 *  
 * @author William Harvey
 */
trait IsoPicklable[CC, L <: HList] {
  /**
   * An isomorphism between case classes of type `CC` and an HList.  Objects mixing in `IsoPicklable` must manually define this isomorphism
   * as shown in the above example.
   */
  def iso: SoupedUpIso[CC, L]
  
  private[this] def pickler = IsoPicklable.pickler[CC](iso)

  /**
   * Endows instances of a case class of type `CC` with the `pickle` method. 
   */
  implicit class PicklizableCaseClass(cc: CC) extends Picklable {
    def pickle(daos: DataOutputStream): Unit = pickler.pickle(cc, daos)
  }

  /**
   * Implicitly unpickles data into instances of type `CC`.
   */
  implicit val unpickler: Unpickler[CC] = new Unpickler[CC] {
    def unpickle(dais: DataInputStream): CC = {
      val hlist = iso.unpickler.unpickle(dais)
      iso.from(hlist)
    }
  }

  /**
   * Unpickles an instance of type `CC` from the provided `DataInputStream`.  Note that this method
   * will blow up if the stream provides data which is not a pickled instance of `CC`!  Obrashayte
   * vnimanie, cuidado, peligro, ostorozhno, watch out, and be careful please.
   * @param dais data input stream about to provide a pickled case class of type `CC`
   * @return an instance of the pickled case class
   */
  def unpickle(dais: DataInputStream): CC = unpickler.unpickle(dais)
}

/**
 * Provides some basic machinery for instances of `IsoPicklable` to generate their pickling machinery
 * (as instances of `Pickler[CC]`), and provides the `createIso` method which companion objects
 * can use to establish the necessary isomorphism between a case class and its HList manifestation. 
 * 
 * @author William Harvey
 */
object IsoPicklable {
  
  /**
   * Generates a pickler for case classes of type `CC`.
   */
  def pickler[CC] = new MkPickler[CC]

  protected class MkPickler[CC] {
    def apply[L <: HList](iso: SoupedUpIso[CC, L]): Pickler[CC] = new Pickler[CC] {
      def pickle(cc: CC, daos: DataOutputStream): Unit = {
        val hlist = iso.to(cc)
        val g = iso.hlistToPicklable(hlist)
        g.foreach(picklable => picklable.pickle(daos))
      }
    }
  }

  /**
   * Creates an isomorphism between an HList and a single-parameter case class.  For this to work, all of the constructor parameters of the
   * case class must be implicitly viewable as `Picklable`.
   * 
   * @note The scala compiler can get confused when you have a single-parameter case class, but that parameter is a subtype of `Product` (e.g.
   *       `Option`, tuples, etc.).  In this case, the compiler is unsure which definition of createIso to try to use.  To get around this problem
   *        you can just call the `createIso1` method instead to extinguish the ambiguity.
   * 
   * @param apply the apply method of the case class
   * @param the unapply method of the case class
   * @return an object capturing the isomorphism between the case class and an HList
   */
  def createIso[CC, T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
    unpicklerIn: Unpickler[T :: HNil]) =
    new SoupedUpIso[CC, T :: HNil] {
      import Functions._
      def to(t: CC): T :: HNil = unapply(t).get :: HNil
      def from(l: T :: HNil): CC = apply.hlisted(l)

      val hlistToPicklable = hlistToPicklableIn
      val unpickler = unpicklerIn
    }

  /**
   * Creates an isomorphims between an HList and a multiple-parameter case class.  For this to work, all of the constructor parameters of the
   * case class must be implicitly viewable as `Picklable`.
   * 
   * @param apply the apply method of the case class
   * @param unapply the unapply method of the case class 
   * @return an object capturing the isomorphism between the case class and an HList
   */
  def createIso[CC, C, T <: Product, L <: HList](apply: C, unapply: CC => Option[T])(
    implicit fhl: FnHListerAux[C, L => CC], hl: HListerAux[T, L],
    hlistToPicklableIn: ToListWithImplicitConversion[L, Picklable], unpicklerIn: Unpickler[L]) =
    new SoupedUpIso[CC, L] {
      import Functions._
      def to(t: CC): L = hl(unapply(t).get)
      def from(l: L): CC = apply.hlisted(l)

      val hlistToPicklable = hlistToPicklableIn
      val unpickler = unpicklerIn
    }
  
  /**
   * Creates an isomorphism between an HList and a single-parameter case class.  Use this instead of `createIso` when the parameter is a subtype of `Product` and the
   * scala compiler gets confused.  For this to work, all of the constructor parameters of the case class must be implicitly viewable as `Picklable`.
   * 
   * @param apply the apply method of the case class
   * @param unapply the unapply method of the case class 
   * @return an object capturing the isomorphism between the case class and an HList
   */
  def createIso1[CC, T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
    unpicklerIn: Unpickler[T :: HNil]) = createIso(apply, unapply)
}
