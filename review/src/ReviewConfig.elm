module ReviewConfig exposing (config)

import CognitiveComplexity
import NoConfusingPrefixOperator
import NoDebug.Log
import NoDebug.TodoOrToString
import NoImportingEverything
import NoMissingTypeAnnotation
import NoPrematureLetComputation
import NoRecursiveUpdate
import NoSimpleLetBody
import NoUnnecessaryTrailingUnderscore
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoUnnecessaryTrailingUnderscore.rule
    , NoSimpleLetBody.rule

    -- Experimental rule that I would not probably enforce in real-life project but wanted to try for this challenge.
    , CognitiveComplexity.rule 10

    -- I can imagine that there are scenarios when you can be tempted to import all (against all the best practices...), but I want to inforce it for this project, because it is not needed here.
    , NoImportingEverything.rule []
    , NoConfusingPrefixOperator.rule
    , NoMissingTypeAnnotation.rule
    , NoPrematureLetComputation.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoUnoptimizedRecursion.rule (NoUnoptimizedRecursion.optInWithComment "ENSURE TCO")
    , Simplify.rule Simplify.defaults
    , NoRecursiveUpdate.rule
    ]
