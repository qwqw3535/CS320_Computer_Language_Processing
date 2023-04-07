package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        case BooleanLiteral(_) =>
          topLevelConstraint(BooleanType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Variable(name) =>
         topLevelConstraint(env(name))

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val typeV = TypeVariable.fresh()
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, typeV) ++ genConstraints(rhs, typeV)
        case Times(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Plus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Div(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Minus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case Mod(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessThan(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case LessEquals(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, IntType) ++ genConstraints(rhs, IntType)
        case And(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case Or(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(lhs, BooleanType) ++ genConstraints(rhs, BooleanType)
        case Concat(lhs, rhs) =>
          topLevelConstraint(StringType) ++ genConstraints(lhs, StringType) ++ genConstraints(rhs, StringType)
        case Not(e) =>
          topLevelConstraint(BooleanType) ++ genConstraints(e, BooleanType)
        case Neg(e) =>
          topLevelConstraint(IntType) ++ genConstraints(e, IntType)



         case Let(ParamDef(name, typeTree), value, body) =>
          val newType = TypeVariable.fresh()
          topLevelConstraint(newType) ++ genConstraints(value, typeTree.tpe) ++ genConstraints(body, newType)(env + (name -> typeTree.tpe))
        case Ite(cond, ifExpr, elseExpr) =>
          genConstraints(cond, BooleanType) ++ genConstraints(ifExpr, expected) ++ genConstraints(elseExpr, expected)
        
        case Sequence(e1, e2) =>
          val typeOf2 = TypeVariable.fresh()
          topLevelConstraint(typeOf2) ++ genConstraints(e1, TypeVariable.fresh()) ++ genConstraints(e2, typeOf2)

        case Call(qname, args) =>
          val (fun, cons) = (table.getFunction(qname), table.getConstructor(qname)) match {
            case (None, Some(fun)) => (fun, Constraint(ClassType(fun.parent), expected, e.position))
            case (Some(fun), None) => (fun, Constraint(fun.retType, expected, e.position))
          }
          cons :: (args zip fun.argTypes).flatMap(pair => genConstraints(pair._1, pair._2))


        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
           pat match{
            case LiteralPattern(lit) =>
                lit match {
                  case IntLiteral(_) => (List(Constraint(IntType, scrutExpected, pat.position)), Map())
                  case StringLiteral(_) => (List(Constraint(StringType, scrutExpected, pat.position)), Map())
                  case BooleanLiteral(_) => (List(Constraint(BooleanType, scrutExpected, pat.position)), Map())
                  case UnitLiteral() => (List(Constraint(UnitType, scrutExpected, pat.position)), Map())
                }
            case IdPattern(name) =>
                val fresh = TypeVariable.fresh();
                (List(Constraint(fresh, scrutExpected, pat.position)), Map(name -> fresh))
            case CaseClassPattern(cons, args) =>
                val signature = table.getConstructor(cons).get
                val parent = ClassType(signature.parent)
                val exp = signature.argTypes
                val realPattern = args zip exp map(p => handlePattern(p._1, p._2))
                (Constraint(parent, scrutExpected, pat.position) :: realPattern.unzip._1.flatten, realPattern.unzip._2.flatMap(_.toList).toMap)
            case WildcardPattern() => 
                (List(),Map()) 
            }
           
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            patConstraints ++ genConstraints(cse.expr, expected)(env ++ moreEnv)
          
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))
        

        case Error(e) => topLevelConstraint(expected) ++ genConstraints(e, StringType);
        
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found,expected) match {
            case (TypeVariable(id), type2: TypeVariable) =>
              if(id == type2.id) {
                solveConstraints(more)
              } else {
                solveConstraints(subst_*(constraints, id, type2))
              }
            case (temp, TypeVariable(id)) =>
              solveConstraints(subst_*(constraints,id, temp))
            case (TypeVariable(_), _) =>
              solveConstraints(Constraint(expected, found, pos) :: more)
            case (type1, type2) =>
              if (type1 == type2){
                solveConstraints(more)
              } else {
                error("Error1: expected: " ++ expected.toString  ++ ", but found: " ++ found.toString, pos)
                 solveConstraints(more)
              }
            case _ =>
               error("Error2: expected: " ++ expected.toString  ++ ", but found: " ++ found.toString, pos)
              solveConstraints(more)
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
