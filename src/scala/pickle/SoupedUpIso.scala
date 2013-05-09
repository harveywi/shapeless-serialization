package pickle

import shapeless._

trait SoupedUpIso[CC, L <: HList] extends Iso[CC, L] {
  def hlistToPicklable: ToListWithImplicitConversion[L, Picklable]
  def unpickler: Unpickler[L]
}

trait SoupedUpIsoContainer[CC] {
  type L <: HList
  def iso: SoupedUpIso[CC, L]
}

//object SoupedUpIsoContainer {
//  class Applier[CC] {
//    def apply[L1 <: HList](iso1: SoupedUpIso[CC, L1]) = new SoupedUpIsoContainer[CC] {
//      type L = L1
//      def iso = iso1
//    }
//
//    def apply[T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
//      unpicklerIn: Unpickler[T :: HNil]) = new SoupedUpIsoContainer[CC] {
//      type L = T :: HNil
//      val iso = IsoPicklable.createIso(apply, unapply)
//    }
//
//    def apply[C, T <: Product, L1 <: HList](apply: C, unapply: CC => Option[T])(
//      implicit fhl: FnHListerAux[C, L1 => CC], hl: HListerAux[T, L1],
//      hlistToPicklableIn: ToListWithImplicitConversion[L1, Picklable], unpicklerIn: Unpickler[L1]) = new SoupedUpIsoContainer[CC] {
//      type L = L1
//      val iso = IsoPicklable.createIso(apply, unapply)
//    }
//    
//    def apply1[T](app: T => CC, unapp: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
//      unpicklerIn: Unpickler[T :: HNil]): SoupedUpIsoContainer[CC] = apply[T](app, unapp)
//  }
//  
//  def apply[CC] = new Applier[CC]
//}