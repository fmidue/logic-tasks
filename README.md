# logic-tasks [![Haskell CI](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml/badge.svg)](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml)

## Mapping from Autotool to relevant modules in this repository

| in Autotool inventory (on <https://autotool.fmi.uni-due.de>)        | Forms | Direct | Quiz | Supports fractional points? | Autotool module (in [`collection/src`](https://git.uni-due.de/fmi/autotool-dev/-/tree/HEAD/collection/src)) | `logic-tasks` module(s)                                                                                                                                    |
| :------------------------------------------------------------------ | :---: | :----: | :--: | :-----------------------: | :---------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Aussagenlogik/Syntax/LogicComposeFormula                            |       |        |  x   | x                         | `Logic.Syntax.ComposeFormula`                                                                               | [`LogicTasks.Syntax.ComposeFormula`](src/LogicTasks/Syntax/ComposeFormula.hs), [`Tasks.ComposeFormula.Quiz`](src/Tasks/ComposeFormula/Quiz.hs)             |
| Aussagenlogik/Syntax/LogicDecomposeFormula                          |       |        |  x   |                           | `Logic.Syntax.DecomposeFormula`                                                                             | [`LogicTasks.Syntax.DecomposeFormula`](src/LogicTasks/Syntax/DecomposeFormula.hs), [`Tasks.DecomposeFormula.Quiz`](src/Tasks/DecomposeFormula/Quiz.hs)     |
| Aussagenlogik/Syntax/LogicInvalidCnfs                               |   x   |        |  x   | x                         | `Logic.Syntax.LegalCnf`                                                                                     | [`LogicTasks.Syntax.IllegalCnfs`](src/LogicTasks/Syntax/IllegalCnfs.hs), [`Tasks.LegalNormalForm.Quiz`](src/Tasks/LegalNormalForm/Quiz.hs)                 |
| Aussagenlogik/Syntax/LogicInvalidDnfs                               |   x   |        |  x   | x                         | `Logic.Syntax.LegalDnf`                                                                                     | [`LogicTasks.Syntax.IllegalDnfs`](src/LogicTasks/Syntax/IllegalDnfs.hs), [`Tasks.LegalNormalForm.Quiz`](src/Tasks/LegalNormalForm/Quiz.hs)                 |
| Aussagenlogik/Syntax/LogicInvalidFormulas                           |   x   |        |  x   | x                         | `Logic.Syntax.LegalFormula`                                                                                 | [`LogicTasks.Syntax.IllegalFormulas`](src/LogicTasks/Syntax/IllegalFormulas.hs), [`Tasks.LegalProposition.Quiz`](src/Tasks/LegalProposition/Quiz.hs)       |
| Aussagenlogik/Syntax/LogicRemoveBrackets                            |       |        |  x   |                           | `Logic.Syntax.SimplestFormula`                                                                              | [`LogicTasks.Syntax.SimplestFormula`](src/LogicTasks/Syntax/SimplestFormula.hs), [`Tasks.SuperfluousBrackets.Quiz`](src/Tasks/SuperfluousBrackets/Quiz.hs) |
| Aussagenlogik/Syntax/LogicSubformulas                               |   x   |        |  x   | x                         | `Logic.Syntax.SubFormula`                                                                                   | [`LogicTasks.Syntax.SubTreeSet`](src/LogicTasks/Syntax/SubTreeSet.hs), [`Tasks.SubTree.Quiz`](src/Tasks/SubTree/Quiz.hs)                                   |
| Aussagenlogik/Syntax/LogicTreeToFormula                             |       |        |  x   |                           | `Logic.Syntax.TreeToFormula`                                                                                | [`LogicTasks.Syntax.TreeToFormula`](src/LogicTasks/Syntax/TreeToFormula.hs), [`Tasks.TreeToFormula.Quiz`](src/Tasks/TreeToFormula/Quiz.hs)                 |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFillGaps         |   x   |   x    |  x   | x                         | `Logic.Semantics.FillGaps`                                                                                  | [`LogicTasks.Semantics.Fill`](src/LogicTasks/Semantics/Fill.hs)                                                                                            |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableChooseForFormula |   x   |   x    |  x   |                           | `Logic.Semantics.ChooseTable`                                                                               | [`LogicTasks.Semantics.Pick`](src/LogicTasks/Semantics/Pick.hs)                                                                                            |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFindMistakes     |   x   |   x    |  x   | x                         | `Logic.Semantics.FindMistakes`                                                                              | [`LogicTasks.Semantics.Decide`](src/LogicTasks/Semantics/Decide.hs)                                                                                        |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMaxterm          |       |   x    |  x   |                           | `Logic.Semantics.MaxTerm`                                                                                   | [`LogicTasks.Semantics.Max`](src/LogicTasks/Semantics/Max.hs)                                                                                              |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMinTerm          |       |   x    |  x   |                           | `Logic.Semantics.MinTerm`                                                                                   | [`LogicTasks.Semantics.Min`](src/LogicTasks/Semantics/Min.hs)                                                                                              |
| Aussagenlogik/Semantik/Resolution/LogicResolutionStep               |   x   |   x    |  x   |                           | `Logic.Semantics.ResolutionStep`                                                                            | [`LogicTasks.Semantics.Step`](src/LogicTasks/Semantics/Step.hs)                                                                                            |
| Aussagenlogik/Semantik/Resolution/LogicResolutionComplete           |   x   |   x    |  x   |                           | `Logic.Semantics.ResolutionFull`                                                                            | [`LogicTasks.Semantics.Resolve`](src/LogicTasks/Semantics/Resolve.hs)                                                                                      |
| Aussagenlogik/Semantik/Resolution/PrologResolutionStep              |       |   x    |  x   |                           | `Logic.Semantics.ResolutionStepProlog`                                                                      | [`LogicTasks.Semantics.Prolog`](src/LogicTasks/Semantics/Prolog.hs)                                                                                        |

## Testing a module

You can use the `testModule` function in order to test a module. A sample call looks like this:

```text
$ stack repl
ghci> testModule (Just AutoLeijen) German (genFillInst dFillConf) LogicTasks.Semantics.Fill.description LogicTasks.Semantics.Fill.partialGrade LogicTasks.Semantics.Fill.completeGrade parser
```

This specific call tests the `Fill` module (found in `src/LogicTasks/Semantics/Fill.hs`). The output looks like this:

```text
Betrachten Sie die folgende Formel:>>>> <F = (¬A ∨ ¬B) ∧ (A ∨ B) ∧ (B ∨ ¬C) ∧ (A ∨ B ∨ D)> <<<<

Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (Wahr oder Falsch).>>>> <A | B | C | D | F
--|---|---|---|--
0 | 0 | 0 | 0 | 0
0 | 0 | 0 | 1 | -
0 | 0 | 1 | 0 | 0
0 | 0 | 1 | 1 | 0
0 | 1 | 0 | 0 | 1
0 | 1 | 0 | 1 | -
0 | 1 | 1 | 0 | 1
0 | 1 | 1 | 1 | -
1 | 0 | 0 | 0 | -
1 | 0 | 0 | 1 | 1
1 | 0 | 1 | 0 | 0
1 | 0 | 1 | 1 | -
1 | 1 | 0 | 0 | 0
1 | 1 | 0 | 1 | 0
1 | 1 | 1 | 0 | -
1 | 1 | 1 | 1 | 0

> <<<<

Geben Sie als Lösung eine Liste der fehlenden Wahrheitswerte an, wobei das erste Element der Liste der ersten Lücke von oben entspricht, das zweite Element der zweiten Lücke, etc.
Die Eingabe der Werte kann binär (0 = falsch, 1 = wahr), ausgeschrieben (falsch, wahr) oder als Kurzform (f, w) erfolgen.
>>>>Ein Lösungsversuch im Fall von vier Lücken könnte beispielsweise so aussehen: <[0,1,1,1]> <<<<

Just ()
[0,1,1,1,0,1]
---- Input ----
[TruthValue {truth = False},TruthValue {truth = True},TruthValue {truth = True},TruthValue {truth = True},TruthValue {truth = False},TruthValue {truth = True}]
---- Prettified Input ----
[False
,True
,True
,True
,False
,True]
---- Partial ----
Lösung hat korrekte Länge?
>>>> <Ja.> <<<<

Just ()
---- Complete ----
Lösung ist korrekt?
>>>> <Nein.> <<<<

>>>>Die Lösung beinhaltet 1 Fehler.<<<<
Nothing
```

In more detail:

- We passed `Just AutoLeijen` to format the input with the specified pretty printer. Other options are: `Nothing`, `Just AutoHughesPJ` or `Manual f` where f is of type `a -> String`. Note that only `Nothing` makes sense for tasks using `Delayed`.
- We passed `German` to print the german version of the task. The other option would be `English`.
- We then passed the generator for creating an instance of the specified module. Must be of type `Gen a`.
- Furthermore, we pass the function that prints the task description. This is usually `SomeModulePath.description`.
- Next, we pass the function that checks the input for syntax errors. This is usually `SomeModulePath.partialGrade`.
- Then, we pass the function that checks the input for semantic errors. This is usually `SomeModulePath.completeGrade`.
- Lastly, we pass a parser that allows us to parse the users input. This is usually just `parser`. Must be of type `Parser b`, if you define one yourself.
