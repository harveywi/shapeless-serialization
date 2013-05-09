package pickle

import java.io._
import shapeless._

trait Unpickler[T] {
  def unpickle(dais: DataInputStream): T
}

object Unpickler extends DefaultUnpicklers 