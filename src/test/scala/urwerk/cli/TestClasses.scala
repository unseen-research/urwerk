package urwerk.cli

import scala.quoted.*

inline def describe[A]: String = ${describeImpl[A]}

def describeImpl[T: Type](using Quotes): Expr[String] = {
  import quotes.reflect.*
  Literal(StringConstant(TypeRepr.of[T].dealias.show)).asExprOf[String]
}
