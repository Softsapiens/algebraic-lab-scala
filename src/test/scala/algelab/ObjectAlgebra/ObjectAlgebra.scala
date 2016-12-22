package algelab.ObjectAlgebra

import org.scalatest.FlatSpec

import org.scalatest._

/**
  * Created by dpm on 14/11/16.
  */
class ObjectAlgebras extends FlatSpec with Matchers {

  object OABasic {

    trait ExpAlg[T] {
      def lit(n: Int): T

      def add(x: T, y: T): T
    }

    def e1[T](f: ExpAlg[T]): T = {
      // (1 + (2 + 3))
      f.add(
        f.lit(1),
        f.add(
          f.lit(2),
          f.lit(3)))
    }

    trait Eval {
      def eval(): Int
    }

    class EvalExp extends ExpAlg[Eval] {
      def lit(n: Int): Eval = new Eval() {
        def eval(): Int = n
      }

      def add(x: Eval, y: Eval): Eval = new Eval() {
        def eval(): Int = x.eval() + y.eval()
      }
    }

    val v1 = e1(new EvalExp()).eval()

    v1 should be(6)

    trait MulAlg[T] extends ExpAlg[T] {
      def mul(x: T, y: T): T
    }

    def e2[T](f: MulAlg[T]): T = {
      // (4 * (5 + 6))
      f.mul(
        f.lit(4),
        f.add(
          f.lit(5),
          f.lit(6)))
    }

    class EvalMul extends EvalExp with MulAlg[Eval] {
      def mul(x: Eval, y: Eval) = new Eval() {
        def eval(): Int = x.eval() * y.eval()
      }
    }

    val v2 = e2(new EvalMul()).eval()

    v2 should be(44)

    trait View {
      def view(): String
    }

    class ViewExp extends ExpAlg[View] {
      def lit(n: Int): View = new View() {
        def view(): String = Integer.toString(n)
      }

      def add(x: View, y: View): View = new View() {
        def view(): String = "(" + x.view() + " + " + y.view() + ")"
      }
    }

    val s1 = e1(new ViewExp()).view()

    s1 should be("(1 + (2 + 3))")
  }

  object OAHK {

    trait ExpAlg[T[_], A] {
      def lit(a: Int): T[A]
      def add(x: T[A], y: T[A]): T[A]
    }

    def e1[T[_], A](f: ExpAlg[T, A]): T[A] = {
      // (1 + (2 + 3))
      f.add(
        f.lit(1),
        f.add(
          f.lit(2),
          f.lit(3)))
    }

    trait Eval[A] {
      def eval(): A
    }

    class EvalExp extends ExpAlg[Eval, Int] {
      def lit(a: Int): Eval[Int] = new Eval[Int]() {
        def eval(): Int = a
      }
      def add(x: Eval[Int], y: Eval[Int]): Eval[Int] = new Eval[Int]() {
        def eval(): Int = x.eval() + y.eval()
      }
    }

    val v1 = e1(new EvalExp()).eval()

    trait MulAlg[T[_], A] extends ExpAlg[T, A] {
      def mul(x: T[A], y: T[A]): T[A]
    }

    def e2[T[_]](f: MulAlg[T, Int]): T[Int] = {
      // (4 * (5 + 6))
      f.mul(
        f.lit(4),
        f.add(
          f.lit(5),
          f.lit(6)))
    }

    class EvalMul extends EvalExp with MulAlg[Eval, Int] {
      def mul(x: Eval[Int], y: Eval[Int]) = new Eval[Int]() {
        def eval(): Int = x.eval() * y.eval()
      }
    }

    val v2 = e2(new EvalMul()).eval()

    trait View[A] {
      def view(): A
    }

    class ViewExp extends ExpAlg[View, String] {
      def lit(n: Int): View[String] = new View[String]() {
        def view(): String = Integer.toString(n)
      }

      def add(x: View[String], y: View[String]): View[String] = new View[String]() {
        def view(): String = "(" + x.view() + " + " + y.view() + ")"
      }
    }

      val s1 = e1(new ViewExp()).view()
  }

  OAHK.v1 should be(6)
  OAHK.v2 should be(44)
  OAHK.s1 should be("(1 + (2 + 3))")
}
