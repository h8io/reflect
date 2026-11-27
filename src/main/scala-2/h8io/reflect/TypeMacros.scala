package h8io.reflect

import h8io.reflect.internal.Macros

trait TypeMacros {
  implicit def materialize[T]: Type[T] = macro Macros.tpe[T]
}

trait Kind1Macros {
  implicit def materialize[F[_]]: Kind1[F] = macro Macros.kind1[F]
}

trait Kind2Macros {
  implicit def materialize[F[_, _]]: Kind2[F] = macro Macros.kind2[F]
}

trait Kind3Macros {
  implicit def materialize[F[_, _, _]]: Kind3[F] = macro Macros.kind3[F]
}
