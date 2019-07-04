import cats.data.NonEmptyList

package object protobluff {

  sealed trait Syntax
  object Syntax {
    case object Proto3 extends Syntax
  }

  final case class OptionValue(name: String, value: String)

  final case class Field(
      name: String,
      tpe: Type,
      position: Int,
      options: List[OptionValue],
      isRepeated: Boolean,
      isMapField: Boolean
  )

  sealed trait Type {
    def conceal: Type
  }
  object Type {
    final object TNull                                                 extends Type
    final object TDouble                                               extends Type
    final object TFloat                                                extends Type
    final object TInt32                                                extends Type
    final object TInt64                                                extends Type
    final object TUint32                                               extends Type
    final object TUint64                                               extends Type
    final object TSint32                                               extends Type
    final object TSint64                                               extends Type
    final object TFixed32                                              extends Type
    final object TFixed64                                              extends Type
    final object TSfixed32                                             extends Type
    final object TSfixed64                                             extends Type
    final object TBool                                                 extends Type
    final object TString                                               extends Type
    final object TBytes                                                extends Type
    final case class TNamedType(name: String)                          extends Type
    final case class TRepeated(value: Type)                            extends Type
    final case class TOneOf(name: String, fields: NonEmptyList[Field]) extends Type
    final case class TMap(keyTpe: Type, value: Type)                   extends Type
    final case class TEnum(
        name: String,
        symbols: List[(String, Int)],
        options: List[OptionValue],
        aliases: List[(String, Int)]
    ) extends Type
    final case class TMessage(name: String, fields: List[Field], reserved: List[List[String]]) extends Type
  }

  final case class Rpc(
    name: String,
    messageType: Type,
    responseType: Type
  )

  final case class Service(
    name: String,
    options: List[OptionValue],
    rpcs: List[Rpc]
  )

  final case class Package(name: String)

  final case class Import(pkg: String, tpe: Option[Import.Type])
  object Import {
    sealed trait Type
    object Type {
      case object Weak extends Type
      case object Public extends Type
    }
  }

  final case class Protofile(
    syntax: Syntax,
    pkg: Option[Package],
    imports: List[Import],
    types: List[Type], // TEnum \/ TMessage
    services: List[Service]
  )

}
