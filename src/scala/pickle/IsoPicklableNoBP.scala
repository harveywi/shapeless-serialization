package pickle

import java.io._
import shapeless._

trait IsoPicklableNoBPHelper[CC] {
  val isoContainer: SoupedUpIsoContainer[CC]
  implicit def pickler = IsoPicklable.pickler[CC](isoContainer.iso)
  implicit class PicklizableCaseClass(cc: CC) extends Picklable {
    def pickle(daos: DataOutputStream): Unit = pickler.pickle(cc, daos)
  }

  implicit val unpickler: Unpickler[CC] = new Unpickler[CC] {
    def unpickle(dais: DataInputStream): CC = {
      val hlist = isoContainer.iso.unpickler.unpickle(dais)
      isoContainer.iso.from(hlist)
    }
  }

  def unpickle(dais: DataInputStream) = unpickler.unpickle(dais)
}

trait IsoPicklableNoBP[CC] extends IsoPicklableNoBPHelper[CC] {
  
  def createIsoContainer = new Applier
  
  class Applier {
//    def apply[L1 <: HList](iso1: SoupedUpIso[CC, L1]) = new SoupedUpIsoContainer[CC] {
//      type L = L1
//      def iso = iso1
//    }

    def apply[T](apply: T => CC, unapply: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
      unpicklerIn: Unpickler[T :: HNil]) = new SoupedUpIsoContainer[CC] {
      type L = T :: HNil
      val iso = IsoPicklable.createIso(apply, unapply)
    }

    def apply[C, T <: Product, L1 <: HList](apply: C, unapply: CC => Option[T])(
      implicit fhl: FnHListerAux[C, L1 => CC], hl: HListerAux[T, L1],
      hlistToPicklableIn: ToListWithImplicitConversion[L1, Picklable], unpicklerIn: Unpickler[L1]) = new SoupedUpIsoContainer[CC] {
      type L = L1
      val iso = IsoPicklable.createIso(apply, unapply)
    }

    def apply1[T](app: T => CC, unapp: CC => Option[T])(implicit hlistToPicklableIn: ToListWithImplicitConversion[T :: HNil, Picklable],
      unpicklerIn: Unpickler[T :: HNil]): SoupedUpIsoContainer[CC] = apply[T](app, unapp)
  }
}