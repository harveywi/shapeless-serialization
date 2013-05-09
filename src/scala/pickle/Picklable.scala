package pickle

import java.io.DataOutputStream

trait Picklable {
  def pickle(daos: DataOutputStream): Unit
}

object Picklable extends DefaultPicklables