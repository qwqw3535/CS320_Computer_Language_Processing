package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._
import javax.print.attribute.EnumSyntax
import scala.language.experimental

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program]
                 with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))
  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  implicit def delimiter(string: String): Syntax[Token] = elem(DelimiterKind(string))

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module_comb) ~<~ eof).map(ms => Program(ms.flatten.toList).setPos(ms.head.head))

  // A module (i.e., a collection of definitions and an initializer expression)
  // lazy val module: Syntax[ModuleDef] = (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map {
  //   case obj ~ id ~ defs ~ body ~ _ ~ id1 => 
  //     if id == id1 then 
  //       ModuleDef(id, defs.toList, body).setPos(obj)
  //     else 
  //       throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + id1)
  // }

  lazy val module_comb: Syntax[ModuleDef] = (kw("object") ~ identifier ~ modulecont).map{
    case _ ~ id ~ mod => 
      if (mod(2) == "__")||(mod(2) == id) then
        ModuleDef(id, mod(0).toList, mod(1))
      else
        throw new AmycFatalError("Begin and end module names do not match: " + id + " and " + mod(2))
  }
  lazy val modulecont: Syntax[Tuple3[Seq[ClassOrFunDef],Option[Expr],String]] = module_old | module_new
  lazy val module_old: Syntax[Tuple3[Seq[ClassOrFunDef],Option[Expr],String]] = (many(definition) ~ opt(expr) ~ kw("end") ~ identifier).map{
    case defs ~ body ~ _ ~ id1 => Tuple3(defs,body,id1)
  }
  lazy val module_new: Syntax[Tuple3[Seq[ClassOrFunDef],Option[Expr],String]] = (delimiter("{") ~ many(definition) ~ opt(expr) ~ delimiter("}")).map{
    case _ ~ defs ~ body ~ _ => Tuple3(defs,body,"__")
  }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id@IdentifierToken(name) => (name, id.position)
  }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] = fndef | absdef | casedef

  lazy val fndef: Syntax[ClassOrFunDef] = (kw("fn") ~ identifier ~ delimiter("(") ~ parameters ~ delimiter(")") 
    ~ delimiter(":") ~ typeTree ~ delimiter("=") ~ delimiter("{") ~ exprPri1 ~ delimiter("}")).map{
      case fn ~ id ~ open ~ para ~ close ~ colon ~ prim ~ equal ~ open2 ~ expr ~ close2 =>
       FunDef(id, para, prim, expr).setPos(fn)
    }
  lazy val absdef: Syntax[ClassOrFunDef] =  (kw("abstract") ~ kw("class") ~ identifier).map{
      case abs ~ _ ~ ids =>
      AbstractClassDef(ids).setPos(abs)
  }
  lazy val casedef: Syntax[ClassOrFunDef] = (kw("case") ~ kw("class") ~ identifier ~ delimiter("(") ~ typeInParameters  ~ delimiter(")")
    ~ kw("extends") ~ identifier).map{
      case ca ~ _ ~ idd ~ open ~ params ~ close ~ ex ~ parent =>
      CaseClassDef(idd, params, parent).setPos(ca)
    }
      
  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] = repsep(parameter, ",").map(_.toList)

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] = (identifier ~ delimiter(":") ~ typeTree).map{
    case id ~ _ ~ typeT =>
      ParamDef(id, typeT)

    }
  lazy val typeInParameters: Syntax[List[TypeTree]] = repsep(typeInParameter, ",").map(_.toList)
  lazy val typeInParameter: Syntax[TypeTree] = (identifier ~ delimiter(":") ~ typeTree).map{
    case id ~ _ ~ typeT =>
      typeT
  }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
   lazy val primitiveType: Syntax[TypeTree] = (accept(PrimTypeKind) {
    case tk@PrimTypeToken(name) => TypeTree(name match {
      case "Unit" => UnitType
      case "Boolean" => BooleanType
      case "Int" => IntType
      case "String" => StringType
      case _ => throw new java.lang.Error("Unexpected primitive type name: " + name)
    }).setPos(tk)
  } ~ opt("(" ~ literal ~ ")")).map { 
    case (prim@TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
    case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) => 
      throw new AmycFatalError("Int type can only be used with a width of 32 bits, found : " + width)
    case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
      throw new AmycFatalError("Int type should have an integer width (only 32 bits is supported)")
    case TypeTree(IntType) ~ None => 
      throw new AmycFatalError("Int type should have a specific width (only 32 bits is supported)")
    case prim ~ Some(_) => 
      throw new AmycFatalError("Only Int type can have a specific width")
    case prim ~ None => prim
  }

  // A user-defined type (such as `List`).
  lazy val identifierType: Syntax[TypeTree] = (identifier ~ opt(qnamecont)).map {
    case id ~ None => TypeTree(ClassType(QualifiedName(None,id)))
    case id ~ Some(qcon) => TypeTree(ClassType(QualifiedName(Some(id),qcon))) 
  }

  lazy val qnamecont: Syntax[String] = (delimiter(".") ~ identifier).map {
    case _ ~ id => id
  }


  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence
  lazy val expr: Syntax[Expr] = recursive { 
     oneOf(
      exprPri1
     )
  }

  val times: Syntax[String] = op("*").map {
    case _ => "*"
  }
  val plus: Syntax[String] = op("+").map {
    case _ => "+"
  }
  val div: Syntax[String] = op("/").map {
    case _ => "/"
  }
  val minus: Syntax[String] = op("-").map {
    case _ => "-"
  }
  val mod: Syntax[String] = op("%").map {
    case _ => "%"
  }
  val lessThan: Syntax[String] = op("<").map {
    case _ => "<"
  } 
  val lessEquals: Syntax[String] = op("<=").map {
    case _ => "<="
  }
  val and: Syntax[String] = op("&&").map {
    case _ => "&&"
  }
  val or: Syntax[String] = op("||").map {
    case _ => "||"
  }
  val equals: Syntax[String] = op("==").map {
    case _ => "=="
  }
  val concat: Syntax[String] = op("++").map {
    case _ => "++"
  }


  lazy val factor: Syntax[Expr] = exprPri9 //| exprIf //| delimiter("(").skip ~ exprPri9 ~ delimiter(")").skip

  lazy val exprPri3: Syntax[Expr] = recursive {
    operators(factor)(
      // neg | not is LeftAssociative,
      times | div | mod is LeftAssociative,
      plus | minus | concat is LeftAssociative,
      lessThan | lessEquals is LeftAssociative,
      equals is LeftAssociative,
      and is LeftAssociative,
      or is LeftAssociative
    ){
      // case ("-", r) => Neg(r).setPos("-")
      // case ("!", r) => Not(r).setPos("!")
      case (l, "*", r) => Times(l, r).setPos(l)
      case (l, "/", r) => Div(l, r).setPos(l)
      case (l, "%", r) => Mod(l, r).setPos(l)
      case (l, "+", r) => Plus(l, r).setPos(l)
      case (l, "-", r) => Minus(l, r).setPos(l)
      case (l, "++", r) => Concat(l, r).setPos(l)
      case (l, "<", r) => LessThan(l, r).setPos(l)
      case (l, "<=", r) => LessEquals(l, r).setPos(l)
      case (l, "==", r) => Equals(l, r).setPos(l)
      case (l, "&&", r) => And(l, r).setPos(l)
      case (l, "||", r) => Or(l, r).setPos(l)

    }
  }
  //expr priority until operator
  lazy val exprPri1: Syntax[Expr] = recursive{
    oneOf(
      let,
      seq
    )
  }
  lazy val let: Syntax[Expr] = (kw("val") ~ parameter ~ delimiter("=") ~ exprPri2 ~ delimiter(";") ~ exprPri1).map {
        case _ ~ df ~ _ ~ value ~ _ ~ body => Let(df, value, body)
      }
  lazy val seq: Syntax[Expr] = (exprPri2 ~ opt(delimiter(";") ~ exprPri1)).map{
    case ep2 ~ None => ep2
    case ep2 ~ Some(_ ~ ep1) => Sequence(ep2, ep1)
  }

  lazy val exprPri2: Syntax[Expr] = recursive{
    oneOf(
      postfixed
    )
  }

  val exprmtch : Syntax[List[MatchCase]] = (kw("match") ~ delimiter("{") ~ many(matchcase) ~ delimiter("}")).map{
    case _ ~ _ ~ cases ~ _ => cases.toList
  }

  lazy val exprmtch_comb: Syntax[List[MatchCase]] = (kw("match") ~ exprcontmtch).map{
    case _ ~ cases => cases.toList
  }
  lazy val exprcontmtch: Syntax[Seq[MatchCase]] = exprmtch_old | exprmtch_new
  lazy val exprmtch_old: Syntax[Seq[MatchCase]] = (delimiter("{") ~ many(matchcase) ~ delimiter("}")).map{
        case _ ~ cases ~ _ => cases
      }
  lazy val exprmtch_new: Syntax[Seq[MatchCase]] = (many(matchcase) ~ kw("end") ~ kw("match")).map{
        case cases ~ _ ~ _ => cases
      }

  lazy val exprtemp: Syntax[Expr] = exprPri3 | exprIf_comb


  lazy val postfixed : Syntax[Expr] = postfixes(exprtemp,exprmtch_comb){
    case (e,m) => Match(e,m)
  }
  

  
  //after operatpr
  lazy val exprPri9: Syntax[Expr] = recursive{
    oneOf(
      simpleExpr,
      neg,
      not
    )
  }
  
  lazy val neg: Syntax[Expr] = (op("-") ~ simpleExpr).map{
      case _ ~ ep => Neg(ep)
    }
  lazy val not: Syntax[Expr] = (op("!") ~ simpleExpr).map{
      case _ ~ ep => Not(ep)
    }


  /*
  lazy val exprSeq: Syntax[Expr] = (expr2 ~ delimiter(";") ~ expr2).map {
        case e1 ~ _ ~ e2 => Sequence(e1, e2)
      }

  lazy val expr1: Syntax[Expr] =  recursive {(kw("val") ~ parameter ~ delimiter("=") ~ expr2 ~ delimiter(";") ~ expr2).map {
        case _ ~ df ~ _ ~ value ~ _ ~ body => Let(df, value, body)
      }
  }

  lazy val expr2: Syntax[Expr] = exprMatch | exprIf
  lazy val exprIf: Syntax[Expr] =  (kw("if") ~ delimiter("(") ~ value ~ delimiter(")") ~ delimiter("{") ~ value ~ delimiter("}") ~ kw("else") ~ delimiter("{") ~ value ~ delimiter("}")).map{
        case _ ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ _ ~ _ ~ elze ~ _ => Ite(cond,thenn,elze)
      }
  lazy val exprMatch: Syntax[Expr] =  (value ~ kw("match") ~ delimiter("{") ~ many(matchcase) ~ delimiter(")")).map {
        case scrut ~ _ ~ op ~ cases ~ cls => Match(scrut,cases.toList)
      }
  */

  lazy val matchcase: Syntax[MatchCase] = (kw("case") ~ pattern ~ delimiter("=>") ~ exprPri1).map {
    case kw ~ pat ~ delim ~ ep1 => MatchCase(pat, ep1)
  }

  lazy val args: Syntax[List[Expr]] = repsep(expr, ",").map(_.toList)

    
  // A literal expression.
  lazy val literal : Syntax[Literal[_]] = litdef | litunit 

  lazy val litdef: Syntax[Literal[_]] =  (accept(LiteralKind)) {
    case token@IntLitToken(value) =>
      IntLiteral(value).setPos(token)
    case token@StringLitToken(value) =>
      StringLiteral(value).setPos(token)
    case token@BoolLitToken(value) =>
      BooleanLiteral(value).setPos(token)
  }
  lazy val litunit : Syntax[Literal[_]] = (delimiter("(") ~ delimiter(")")).map{
    case _ ~ _ => UnitLiteral()
  }
        
  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList)


  lazy val pattern: Syntax[Pattern] = recursive { 
      oneOf(
        literalPattern,
        caseClassPattern,
        wildPattern)
    }

  lazy val literalPattern: Syntax[Pattern] = (literal).map{
    case token => LiteralPattern(token)
  }
    
  lazy val wildPattern: Syntax[Pattern] = kw("_").map{ 
    case _ => WildcardPattern()
  }

  lazy val caseClassPattern: Syntax[Pattern] = (identifier ~ opt(opt(qnamecont) ~ delimiter("(") ~ patterns ~ delimiter(")"))).map{
      case id ~ None => IdPattern(id)
      case id ~ Some(None ~ _ ~ args ~ _ ) =>
        CaseClassPattern(QualifiedName(None, id), args.toList)
      case id ~ Some(Some(qcon) ~ _ ~ args ~ _ ) =>
        CaseClassPattern(QualifiedName(Some(id), qcon), args.toList)
  }
        

  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] = 
        litdef.up[Expr] | variableOrCall | exprError | exprParen //| exprIf
  
  lazy val variableOrCall: Syntax[Expr] = (identifier ~ opt(opt(qnamecont) ~ delimiter("(") ~ args ~ delimiter(")"))).map{
        case id ~ None => Variable(id)
        case id ~ Some(None ~ _ ~ args ~ _) => Call(QualifiedName(None,id),args)
        case id ~ Some(Some(qcon) ~ _ ~ args ~ _) => Call(QualifiedName(Some(id),qcon),args)
      }
  lazy val exprError: Syntax[Expr] =  (kw("error") ~ delimiter("(") ~ exprPri1 ~ delimiter(")")).map {
        case _ ~ op ~ msg ~ cls => Error(msg)
      }
  lazy val exprParen: Syntax[Expr] =  (delimiter("(") ~ opt(exprPri1) ~ delimiter(")")).map {
        case _ ~ None ~ _  => UnitLiteral()
        case _ ~ Some(ep1) ~ _  => ep1
      }

  // lazy val exprIf: Syntax[Expr] =  (kw("if") ~ delimiter("(") ~ exprPri1 ~ delimiter(")") ~ delimiter("{") ~ exprPri1 ~ delimiter("}") ~ kw("else") ~ delimiter("{") ~ exprPri1 ~ delimiter("}")).map{
  //       case _ ~ _ ~ cond ~ _ ~ _ ~ thenn ~ _ ~ _ ~ _ ~ elze ~ _ => Ite(cond,thenn,elze)
  //     }
  // lazy val exprIf_l6: Syntax[Expr] =  (kw("if") ~ exprPri1 ~ kw("then") ~ exprPri1 ~ kw("else") ~ exprPri1 ~ kw("end if")).map{
  //       case _ ~ cond ~ _ ~ thenn ~ _ ~ elze ~ _ => Ite(cond,thenn,elze)
  //     }

  lazy val exprIf_comb: Syntax[Expr] = (kw("if") ~ delimiter("(")~ exprPri1 ~ delimiter(")") ~ exprcontIf).map{
    case _ ~ _ ~ cond ~ _ ~ ifep => Ite(cond,ifep(0),ifep(1))
  }
  lazy val exprcontIf: Syntax[List[Expr]] = exprIf_old | exprIf_new
  lazy val exprIf_old: Syntax[List[Expr]] = (delimiter("{") ~ exprPri1 ~ delimiter("}") ~ kw("else") ~ delimiter("{") ~ exprPri1 ~ delimiter("}")).map{
        case _ ~ thenn ~ _ ~ _ ~ _ ~ elze ~ _ => List(thenn, elze)
      }
  lazy val exprIf_new: Syntax[List[Expr]] = (exprPri1 ~ kw("else") ~ exprPri1 ~ kw("end") ~ kw("if")).map{
        case thenn ~ _ ~ elze ~ _ ~ _ => List(thenn, elze)
      }


  // val exprmtch : Syntax[List[MatchCase]] = (kw("match") ~ delimiter("{") ~ many(matchcase) ~ delimiter("}")).map{
  //   case _ ~ _ ~ cases ~ _ => cases.toList
  // }
  // lazy val postfixed : Syntax[Expr] = postfixes(exprtemp,exprmtch){
  //   case (e,m) => Match(e,m)
  //}

  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.


  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("Program grammar is not LL1!")
    }

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest) => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) => fatal("Unexpected token: " + token + ", possible kinds: " + rest.first.map(_.toString).mkString(", "))
    }
  }
}