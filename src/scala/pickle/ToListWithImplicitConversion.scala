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

trait ToListWithImplicitConversion[L <: HList, B] {
  def apply(l: L): List[B]
}

object ToListWithImplicitConversion {
  implicit def hnilToListWithConversion[B]: ToListWithImplicitConversion[HNil, B] = new ToListWithImplicitConversion[HNil, B] {
    def apply(l: HNil) = Nil
  }

  implicit def hlistToListWithConversion[H, T <: HList, B](implicit u: H => B, hlistToPicklable: ToListWithImplicitConversion[T, B]) =
    new ToListWithImplicitConversion[H :: T, B] {
      def apply(l: H :: T) = u(l.head) :: hlistToPicklable(l.tail)
    }
}