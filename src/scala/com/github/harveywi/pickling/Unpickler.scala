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
 * Types mixing in this trait advertise that they can unpickle instances of type `T`. 
 */
trait Unpickler[T] {
  /**
   * Unpickles an instance of type `T` from the specified stream.
   * 
   * @param dais stream providing an instance of type `T`
   * @return unpickled instance of type `T`
   */
  def unpickle(dais: DataInputStream): T
}

object Unpickler extends DefaultUnpicklers 