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

/**
 * A trait for types which can pickle instances of type `T`.
 * 
 * @author William Harvey
 */
trait Pickler[T] {
  /**
   * Pickles an instance of type `T`.
   * @param t instance to pickle
   * @param daos stream in which t will be pickled
   */
	def pickle(t: T, daos: DataOutputStream): Unit
}