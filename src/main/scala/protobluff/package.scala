import cats.data.NonEmptyList

package object protobluff {

  sealed trait Syntax
  object Syntax {
    case object Proto3 extends Syntax
  }

  final case class OptionValue(name: String, value: String)

  sealed trait Field {
    def widen: Field = this
  }

  object Field {
    final case class Normal(
      name: String,
      tpe: Type,
      fieldNumber: Int,
      options: List[OptionValue],
      isRepeated: Boolean
    ) extends Field

    final case class OneOf(
      name: String,
      fields: List[OneOf.OneOfField]
    ) extends Field

    object OneOf {
      final case class OneOfField(
        name: String,
        tpe: Type,
        fieldNumber: Int,
        options: List[OptionValue]
      )
    }

    final case class Map(
      name: String,
      tpe: Type.TMap,
      fieldNumber: Int,
      options: List[OptionValue]
    ) extends Field
  }


  sealed trait Type {
    def widen: Type = this
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
    final case class TMap(keyTpe: Type, value: Type)                   extends Type

  }

  final case class Range(start: String /*intlit*/, end: Option[String])

  sealed trait TopLevelDefinition

  object TopLevelDefinition {

    final case class TEnum(
      name: String,
      symbols: List[(String, Int)],
      options: List[OptionValue],
      aliases: List[(String, Int)]
    ) extends TopLevelDefinition

    final case class TMessage(name: String, fields: List[Field], reserved: List[List[String]]) extends TopLevelDefinition

    final case class Service(
      name: String,
      options: List[OptionValue],
      rpcs: List[Service.Rpc]
    ) extends TopLevelDefinition

    object Service {
      final case class Rpc(
        name: String,
        messageType: Type,
        responseType: Type
      )
    }
  }

  final case class Package(name: String)

  final case class Import(tpe: Option[Import.Type], pkg: String)
  object Import {
    sealed trait Type
    object Type {
      case object Weak extends Type
      case object Public extends Type
      val weak: Type = Weak
      val public: Type = Public
    }
  }

  final case class Protofile(
    syntax: Syntax,
    pkg: Option[Package],
    imports: List[Import],
    definitions: List[TopLevelDefinition]
  )

}
