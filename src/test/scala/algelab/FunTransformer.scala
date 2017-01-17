package algelab

import algelab.FunTransformer._
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{Monad, Id}

/**
  * Created by dani on 08/01/2017.
  */
object FunTransformer {

  trait MyIO[F[_]] {
    def put[A](v: A): F[Unit]

    def get(): F[String] // TODO: what if get is polymorphic¿?
  }

  import scalaz.Id

  type Id[A] = scalaz.Id.Id[A]

  trait Fu[F[_]] {
    def fmap[A, B](fa: F[A])(f: A=>B): F[B]
  }
  trait Mo[F[_]] {
    def bind[A, B](fa: F[A])(f: (A=>F[B])): F[B]
  }
}

class FunTransformerSpec extends FlatSpec with Matchers {

  import FunTransformer._

  "MyIO must accept for-comprehentions" should "work with scalaz.Monad" in {
      import scalaz.Id.Id, scalaz.syntax.monad._

      val _mio = new MyIO[Id] {
        override def get() = "hola"

        override def put[A](v: A) = println(v)
      }

      def _program[F[_] : MyIO : Monad](): F[String] = {
        for {
          v <- implicitly[MyIO[F]].get()
          _ <- implicitly[MyIO[F]].put(v)
        } yield (v)
      }

      _program()(_mio, Monad[Id])
    }

      "MyIO must accpet for-comprehentions" should "work with custom monad" in {
        import scala.language.reflectiveCalls // TODO: find about its meaning...

        implicit val _mio = new MyIO[Id] {
          self =>
          override def get() = "hola"

          override def put[A](v: A) = println(v)
        }

        implicit def mid[A](ia: Id[A]) = new Mo[Id] with Fu[Id] {
          override def fmap[B, C](fa: Id[B])(f: (B) => C) = f(fa)

          def map[B](f: A => B): Id[B] = fmap(ia)(f)

          override def bind[B, C](fa: Id[B])(f: (B) => Id[C]): Id[C] = f(fa)

          def flatMap[B](f: (A) => Id[B]): Id[B] = bind(ia)(f)
        }

        def _program[F[_] : MyIO](): F[String] = {
          for {
            v <- implicitly[MyIO[F]].get()
            _ <- implicitly[MyIO[F]].put(v)
          } yield (v)
        }

        _program()
      }
}
