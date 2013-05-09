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

import shapeless._

/**
 * An `Iso` which ensures that all constituent elements of an HList are implicitly `Picklable`.  As a byproduct,
 * it can (un)pickle the HList and, thanks to the case class isomorphism, it can (un)pickle the isomorphic
 * case class as well. 
 * 
 * @author William Harvey
 */
trait SoupedUpIso[CC, L <: HList] extends Iso[CC, L] {
  def hlistToPicklable: ToListWithImplicitConversion[L, Picklable]
  def unpickler: Unpickler[L]
}

/**
 * A container for a `SoupedUpIso`.  This is sort of a hack which helps us to avoid specifying the
 * type signature of the HList, which can become unruly in practice.
 * 
 * @author William Harvey
 */
trait SoupedUpIsoContainer[CC] {
  type L <: HList
  def iso: SoupedUpIso[CC, L]
}
