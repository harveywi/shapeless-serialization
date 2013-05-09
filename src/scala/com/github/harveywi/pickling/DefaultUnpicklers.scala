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
import shapeless.TypeOperators._
import java.io.DataInputStream
import scala.util.matching.Regex
import java.io.File
import scala.reflect.ClassTag

trait DefaultUnpicklers {
  implicit def hNilUnpickler = new Unpickler[HNil] {
    def unpickle(dais: DataInputStream): HNil = HNil
  }

  implicit def hlistUnpickler[H, T <: HList](implicit f: Unpickler[H], ubob: Unpickler[T]) = new Unpickler[H :: T] {
    def unpickle(dais: DataInputStream): H :: T = {
      f.unpickle(dais) :: ubob.unpickle(dais)
    }
  }

  implicit def intUnpickler: Unpickler[Int] = new Unpickler[Int] { def unpickle(dais: DataInputStream) = dais.readInt }
  implicit def stringUnpickler: Unpickler[String] = new Unpickler[String] { def unpickle(dais: DataInputStream) = dais.readUTF }
  implicit def boolUnpickler: Unpickler[Boolean] = new Unpickler[Boolean] { def unpickle(dais: DataInputStream) = dais.readBoolean }
  implicit def floatUnpickler = new Unpickler[Float] { def unpickle(dais: DataInputStream) = dais.readFloat }
  implicit def doubleUnpickler = new Unpickler[Double] { def unpickle(dais: DataInputStream) = dais.readDouble }
  implicit def shortUnpickler = new Unpickler[Short] { def unpickle(dais: DataInputStream) = dais.readShort }
  implicit def longUnpickler = new Unpickler[Long] { def unpickle(dais: DataInputStream) = dais.readLong }
  implicit def regexUnpickler = new Unpickler[Regex] { def unpickle(dais: DataInputStream) = new Regex(dais.readUTF) }
  implicit def fileUnpickler = new Unpickler[File] { def unpickle(dais: DataInputStream) = new File(dais.readUTF) }
  implicit def charUnpickler = new Unpickler[Char] { def unpickle(dais: DataInputStream) = dais.readChar }

  implicit def optionUnpickler[T](implicit unpickler: Unpickler[T]): Unpickler[Option[T]] = new Unpickler[Option[T]] {
    def unpickle(dais: DataInputStream) = {
      if (dais.readUTF == "Some") {
        Some(unpickler.unpickle(dais))
      } else {
        None
      }
    }
  }

  implicit def seqUnpickler[T](implicit elementUnpickler: Unpickler[T]): Unpickler[Seq[T]] = new Unpickler[Seq[T]] {
    def unpickle(dais: DataInputStream) = {
      val n = dais.readInt
      (for (_ <- 1 to n) yield elementUnpickler.unpickle(dais)).toSeq
    }
  }

  implicit def mapUnpickler[K, V](implicit keyUnpickler: Unpickler[K], valUnpickler: Unpickler[V]): Unpickler[Map[K, V]] = new Unpickler[Map[K, V]] {
    def unpickle(dais: DataInputStream) = {
      seqUnpickler(tuple2Unpickler(keyUnpickler, valUnpickler)).unpickle(dais).toMap
    }
  }

  implicit def tuple2Unpickler[T1, T2](implicit unpickler1: Unpickler[T1], unpickler2: Unpickler[T2]): Unpickler[(T1, T2)] = new Unpickler[(T1, T2)] {
    def unpickle(dais: DataInputStream) = {
      (unpickler1.unpickle(dais), unpickler2.unpickle(dais))
    }
  }

  implicit def arrayUnpickler[T](implicit classTag: ClassTag[T], elementUnpickler: Unpickler[T]): Unpickler[Array[T]] = new Unpickler[Array[T]] {
    def unpickle(dais: DataInputStream) = {
      val n = dais.readInt
      (for (_ <- 1 to n) yield elementUnpickler.unpickle(dais)).toArray
    }
  }

}