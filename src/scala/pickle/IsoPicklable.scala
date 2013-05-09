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

import java.io._
import shapeless._
import shapeless.TypeOperators._
import language.implicitConversions

abstract class IsoPicklable[CC, L <: HList] {
  def iso: SoupedUpIso[CC, L]
  def pickler = IsoPicklable.pickler[CC](iso)

  implicit class PicklizableCaseClass(cc: CC) extends Picklable {
    def pickle(daos: DataOutputStream): Unit = pickler.pickle(cc, daos)
  }

  implicit val unpickler: Unpickler[CC] = new Unpickler[CC] {
    def unpickle(dais: DataInputStream): CC = {
      val hlist = iso.unpickler.unpickle(dais)
      iso.from(hlist)
    }
  }

  def unpickle(dais: DataInputStream) = unpickler.unpickle(dais)
}

object IsoPicklable {
  def pickler[CC] = new MkPickler[CC]

  class MkPickler[CC] {
    def apply[L <: HList](iso: SoupedUpIso[CC, L]): Pickler[CC] = new Pickler[CC] {
      def pickle(cc: CC, daos: DataOutputStream): Unit = {
        val hlist = iso.to(cc)
        val g = iso.hlistToPicklable(hlist)
        g.foreach(picklable => picklable.pickle(daos))
      }
    }
  }

  def createIso[CC, T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
    unpicklerIn: Unpickler[T :: HNil]) =
    new SoupedUpIso[CC, T :: HNil] {
      import Functions._
      def to(t: CC): T :: HNil = unapply(t).get :: HNil
      def from(l: T :: HNil): CC = apply.hlisted(l)

      val hlistToPicklable = hlistToPicklableIn
      val unpickler = unpicklerIn
    }

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
  
  def createIso1[CC, T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
    unpicklerIn: Unpickler[T :: HNil]) = createIso(apply, unapply)
}



