package h8io.reflect.test

import scala.annotation.StaticAnnotation

/** An annotation that stays in the type, unlike `@unchecked`. */
final class TypeAnnotation extends StaticAnnotation
