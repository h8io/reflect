package h8io.reflect

import h8io.reflect.internal.Macros

trait TypeMacros {
  implicit inline def materialize[T]: Type[T] = ${ Macros.tpe[T] }
}

trait Kind1Macros {
  implicit inline def materialize[F[_]]: Kind1[F] = ${ Macros.kind1[F] }
}

trait Kind2Macros {
  implicit inline def materialize[F[_, _]]: Kind2[F] = ${ Macros.kind2[F] }
}

trait Kind3Macros {
  implicit inline def materialize[F[_, _, _]]: Kind3[F] = ${ Macros.kind3[F] }
}
