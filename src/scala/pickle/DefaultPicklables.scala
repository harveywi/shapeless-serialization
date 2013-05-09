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

import java.io.{ DataOutputStream, File }
import scala.util.matching.Regex
import scala.reflect.ClassTag

trait DefaultPicklables {
  implicit class IntPicklable(x: Int) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeInt(x) }
  implicit class StringPicklable(x: String) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeUTF(x) }
  implicit class BooleanPicklable(b: Boolean) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeBoolean(b) }
  implicit class FloatPicklable(f: Float) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeFloat(f) }
  implicit class DoublePicklable(d: Double) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeDouble(d) }
  implicit class ShortPicklable(s: Short) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeShort(s) }
  implicit class LongPicklable(l: Long) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeLong(l) }
  implicit class RegexPicklable(r: Regex) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeUTF(r.toString) }
  implicit class CharPicklable(c: Char) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeChar(c) }
  implicit class FilePicklable(f: File) extends Picklable { def pickle(daos: DataOutputStream) = daos.writeUTF(f.getAbsolutePath) }

  implicit class OptionPicklable[T](tOpt: Option[T])(implicit u: T => Picklable) extends Picklable {
    def pickle(daos: DataOutputStream) = tOpt match {
      case Some(t) =>
        daos.writeUTF("Some")
        t.pickle(daos)
      case None =>
        daos.writeUTF("None")
    }
  }

  implicit class Tuple2Picklable[T1, T2](t: (T1, T2))(implicit p1: T1 => Picklable, p2: T2 => Picklable) extends Picklable {
    def pickle(daos: DataOutputStream) = {
      t._1.pickle(daos)
      t._2.pickle(daos)
    }
  }

  implicit class ArrayPicklable[T: ClassTag](arr: Array[T])(implicit u: T => Picklable) extends Picklable {
    def pickle(daos: DataOutputStream) = {
      daos.writeInt(arr.length)
      arr.foreach(_.pickle(daos))
    }
  }

  implicit class SeqPicklable[T](seq: Seq[T])(implicit u: T => Picklable) extends Picklable {
    def pickle(daos: DataOutputStream) = {
      daos.writeInt(seq.size)
      seq.foreach(_.pickle(daos))
    }
  }

  implicit class MapPicklable[K, V](m: Map[K, V])(implicit k: K => Picklable, v: V => Picklable) extends Picklable {
    def pickle(daos: DataOutputStream) = {
      m.toSeq.pickle(daos)
    }
  }
}