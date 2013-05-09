package pickle

import shapeless._
import java.io._
import language.reflectiveCalls

object Nugget {
  //  implicit class Blargh[CC, L <: HList](x: { val iso: SoupedUpIso[CC, L] }) {
  //    def pickler = IsoPicklable.pickler[CC](x.iso)
  //    implicit class PicklizableCaseClass(cc: CC) extends Picklable {
  //      def pickle(daos: DataOutputStream): Unit = pickler.pickle(cc, daos)
  //
  //      implicit val unpickler: Unpickler[CC] = new Unpickler[CC] {
  //        def unpickle(dais: DataInputStream): CC = {
  //          val hlist = x.iso.unpickler.unpickle(dais)
  //          x.iso.from(hlist)
  //        }
  //      }
  //
  //      def unpickle(dais: DataInputStream) = unpickler.unpickle(dais)
  //    }
  //
  //    def poodoo = "42"
  //  }

  case class Test1(x: Int, y: Option[String])
  object Test1 extends IsoPicklableNoBP[Test1] {
    val isoContainer = createIsoContainer(apply _, unapply _)
  }

  def main(args: Array[String]): Unit = {

    val t = Test1(42, Some("Hello"))

    val pickled = {
      val baos = new ByteArrayOutputStream
      val daos = new DataOutputStream(baos)
      t.pickle(daos)
      baos.toByteArray()
    }

    val unpickled = {
      val bais = new ByteArrayInputStream(pickled)
      val dais = new DataInputStream(bais)
      Test1.unpickle(dais)
    }
    println(t)
    println(unpickled)
  }

}