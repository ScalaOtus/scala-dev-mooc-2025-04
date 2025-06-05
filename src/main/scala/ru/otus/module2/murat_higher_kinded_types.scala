package ru.otus.module2

object murat_higher_kinded_types {

  trait Tupler[F[_]] {
    def tupleF[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }


  object Tupler {

    implicit val optTupler: Tupler[Option] = new Tupler[Option] {
      override def tupleF[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _ => None
        }
      }
    }

    // суммонер
    def apply[F[_]](implicit T: Tupler[F[_]]): Tupler[F[_]] = T

    //метод
    def tupleF[F[_]: Tupler, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Tupler[F].tupleF(fa,fb)

  }

}
