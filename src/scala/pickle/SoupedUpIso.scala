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

import shapeless._

trait SoupedUpIso[CC, L <: HList] extends Iso[CC, L] {
  def hlistToPicklable: ToListWithImplicitConversion[L, Picklable]
  def unpickler: Unpickler[L]
}

trait SoupedUpIsoContainer[CC] {
  type L <: HList
  def iso: SoupedUpIso[CC, L]
}

//object SoupedUpIsoContainer {
//  class Applier[CC] {
//    def apply[L1 <: HList](iso1: SoupedUpIso[CC, L1]) = new SoupedUpIsoContainer[CC] {
//      type L = L1
//      def iso = iso1
//    }
//
//    def apply[T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
//      unpicklerIn: Unpickler[T :: HNil]) = new SoupedUpIsoContainer[CC] {
//      type L = T :: HNil
//      val iso = IsoPicklable.createIso(apply, unapply)
//    }
//
//    def apply[C, T <: Product, L1 <: HList](apply: C, unapply: CC => Option[T])(
//      implicit fhl: FnHListerAux[C, L1 => CC], hl: HListerAux[T, L1],
//      hlistToPicklableIn: ToListWithImplicitConversion[L1, Picklable], unpicklerIn: Unpickler[L1]) = new SoupedUpIsoContainer[CC] {
//      type L = L1
//      val iso = IsoPicklable.createIso(apply, unapply)
//    }
//    
//    def apply1[T](app: T => CC, unapp: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
//      unpicklerIn: Unpickler[T :: HNil]): SoupedUpIsoContainer[CC] = apply[T](app, unapp)
//  }
//  
//  def apply[CC] = new Applier[CC]
//}