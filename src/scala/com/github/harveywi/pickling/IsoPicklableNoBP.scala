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

/**
 * Mix this in to the companion object of a case class `C` to enable low-boilerplate (un)pickling of `C`.
 * 
 * @example {{{
 * 
 * // Define a case class
 * case class TestLotsOfParameters(xInt: Int, xString: String, xLong: Long, xOptionFloat: Option[Float], xSeqDouble: Seq[Double])
 * 
 * // Mix in the IsoPicklableNoBP trait.
 * object TestLotsOfParameters extends IsoPicklableNoBP[TestLotsOfParameters] {
 *   // Create a container for the obligatory HList isomorphism
 *   val isoContainer = createIsoContainer(apply _, unapply _)
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
trait IsoPicklableNoBP[CC] extends IsoPicklableNoBPHelper[CC] {
  
  /**
   * Creates a container instance capturing the isomorphism between `CC` and an HList.
   */
  def createIsoContainer = new Applier
  
  /**
   * Exists only to help with the type inference of `createIsoContainer`.  Using this trick,
   * the HList type signature can be captures without having to manually specify it.  Yay!
   */
  class Applier {
    def apply[T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
      unpicklerIn: Unpickler[T :: HNil]) = new SoupedUpIsoContainer[CC] {
      type L = T :: HNil
      val iso = IsoPicklable.createIso(apply, unapply)
    }

    def apply[C, T <: Product, L1 <: HList](apply: C, unapply: CC => Option[T])(
      implicit fhl: FnHListerAux[C, L1 => CC], hl: HListerAux[T, L1],
      hlistToPicklableIn: ToListWithImplicitConversion[L1, Picklable], unpicklerIn: Unpickler[L1]) = new SoupedUpIsoContainer[CC] {
      type L = L1
      val iso = IsoPicklable.createIso(apply, unapply)
    }

    def apply1[T](app: T => CC, unapp: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
      unpicklerIn: Unpickler[T :: HNil]): SoupedUpIsoContainer[CC] = apply[T](app, unapp)
  }
}

/**
 * Necessary to relieve `IsoPicklableNoBP` from having to ask for the HList type signature.
 *    
 * @author William Harvey
 */
trait IsoPicklableNoBPHelper[CC] {
  /**
  * Captures the isomorphism between case classes of type `CC` and an HList.  Objects mixing in `IsoPicklableNoBP` must manually define this isomorphism
  * as shown in the `IsoPicklableNoBP` usage example.
  */
  val isoContainer: SoupedUpIsoContainer[CC]
  
  implicit def pickler = IsoPicklable.pickler[CC](isoContainer.iso)
  
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
      val hlist = isoContainer.iso.unpickler.unpickle(dais)
      isoContainer.iso.from(hlist)
    }
  }

  /**
   * Unpickles an instance of type `CC` from the provided `DataInputStream`.  Note that this method
   * will blow up if the stream provides data which is not a pickled instance of `CC`!  Obrashayte
   * vnimanie, cuidado, peligro, ostorozhno, watch out, and be careful please.
   * @param dais data input stream about to provide a pickled case class of type `CC`
   * @return an instance of the pickled case class
   */
  def unpickle(dais: DataInputStream) = unpickler.unpickle(dais)
}