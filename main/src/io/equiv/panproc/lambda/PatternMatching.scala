package io.equiv.panproc.lambda

import io.equiv.panproc.lambda.Syntax._
import io.equiv.panproc.lambda.CallByValueBigStepSemantics.Bind

object PatternMatching:

  def patternCanMatch(pattern: Pattern, expression: Expression): Boolean =
    pattern match
      case Variable(name) =>
        expression match
          case Variable(rightName) =>
            name == rightName
          case Bind(env, Variable(rightName)) =>
            //TODO: Likely should check boundedness
            name == rightName
          case _ =>
            true
      case valuePattern: Literal =>
        valuePattern == expression
      case Constructor(left, right) =>
        expression match
          case Application(function, argument) =>
            patternCanMatch(left, function) &&
              patternCanMatch(right, argument)
          case Bind(env, term) =>
            patternCanMatch(pattern, term)
          case _ =>
            false

  def matchPattern(pattern: Pattern, expression: Expression): List[(String, Expression)] =
    pattern match
      case Variable(name) =>
        List(name -> (
          expression match
            case Variable(rightName) if name != rightName =>
              throw Exception(s"Constructors $pattern and $expression do not match.")
            case Bind(env, Variable(rightName)) if name != rightName =>
              //TODO: Likely should check boundedness
              throw Exception(s"Constructors $pattern and $rightName do not match.")
            case _ =>
              expression
        ))
      case valuePattern: Literal =>
        if valuePattern == expression then
          List()
        else
          throw Exception(s"$pattern and $expression cannot match.")
      case Constructor(left, right) =>
        expression match
          case Application(function, argument) =>
            matchPattern(left, function) ++
              matchPattern(right, argument)
          case Bind(env, term) =>
            matchPattern(pattern, term)
          case _ =>
            throw Exception(s"$pattern and $expression cannot match.")
