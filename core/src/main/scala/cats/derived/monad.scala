package cats.derived

import cats._, Eval.now
import alleycats.{ConsK, EmptyK, Pure}
import export.{ exports, imports, reexports }
import shapeless._


@reexports[MkMonad]
object monad {
  @imports[Monad]
  object legacy
}

trait MkMonad[F[_]] extends Monad[F]

@exports
object MkMonad extends MkMonad0 {
  def apply[F[_]](implicit mmf: MkMonad[F]): MkMonad[F] = mmf
}

trait MkMonad0 extends MkMonad1 {
  implicit def withConsK[F[_]](
    implicit
    P: Pure[F],
    C: ConsK[F],
    E: EmptyK[F],
    F: Foldable[F]
  ): MkMonad[F] = new MkMonad[F] {
    def pure[A](x: A): F[A] = P.pure(x)

    def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
      F.foldRight[A, F[B]](fa, now(E.empty[B])) { (a, l) =>
        val fb = f(a)
        F.foldRight(fb, l)((b, memo) => now(C.cons(b, memo.value)))
      }.value
    }
  }
}


trait MkMonad1 {
  implicit def withoutConsK[F[_]](
    implicit
    P: Pure[F],
    E: EmptyK[F],
    F: Foldable[F]
  ): MkMonad[F] = new MkMonad[F] {
    def pure[A](x: A): F[A] = P.pure(x)

    def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = {
      F.foldLeft[A, F[B]](fa, E.empty[B])((_, a) => f(a))
    }
  }
}
