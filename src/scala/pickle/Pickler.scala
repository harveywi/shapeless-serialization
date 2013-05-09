package pickle

import java.io._

trait Pickler[T] {
	def pickle(t: T, daos: DataOutputStream): Unit
}