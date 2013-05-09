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