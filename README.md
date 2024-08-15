# logic-tasks [![Haskell CI](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml/badge.svg)](https://github.com/fmidue/logic-tasks/actions/workflows/haskell.yml)

## Mapping from Autotool to relevant modules in this repository

| in Autotool inventory (on <https://autotool.fmi.uni-due.de>) | Direct | Quiz | Autotool module (in [`collection/src`](https://git.uni-due.de/fmi/autotool-dev/-/tree/HEAD/collection/src)) | `logic-tasks` module(s) |
| :-- | :-: | :-: | :-- | :-- |
| Aussagenlogik/Syntax/LogicComposeFormula | | x | `Logic.Syntax.ComposeFormula` | [`LogicTasks.Syntax.ComposeFormula`](src/LogicTasks/Syntax/ComposeFormula.hs), [`Tasks.ComposeFormula.Quiz`](src/Tasks/ComposeFormula/Quiz.hs) |
| Aussagenlogik/Syntax/LogicDecomposeFormula | | x | `Logic.Syntax.DecomposeFormula` | [`LogicTasks.Syntax.DecomposeFormula`](src/LogicTasks/Syntax/DecomposeFormula.hs), [`Tasks.DecomposeFormula.Quiz`](src/Tasks/DecomposeFormula/Quiz.hs) |
| Aussagenlogik/Syntax/LogicInvalidCnfs | | x | `Logic.Syntax.LegalCnf` | [`LogicTasks.Syntax.IllegalCnfs`](src/LogicTasks/Syntax/IllegalCnfs.hs), [`Tasks.LegalCNF.Quiz`](src/Tasks/LegalCNF/Quiz.hs) |
| Aussagenlogik/Syntax/LogicInvalidFormulas | | x | `Logic.Syntax.LegalFormula` | [`LogicTasks.Syntax.IllegalFormulas`](src/LogicTasks/Syntax/IllegalFormulas.hs), [`Tasks.LegalProposition.Quiz`](src/Tasks/LegalProposition/Quiz.hs) |
| Aussagenlogik/Syntax/LogicRemoveBrackets | | x | `Logic.Syntax.SimplestFormula` | [`LogicTasks.Syntax.SimplestFormula`](src/LogicTasks/Syntax/SimplestFormula.hs), [`Tasks.SuperfluousBrackets.Quiz`](src/Tasks/SuperfluousBrackets/Quiz.hs) |
| Aussagenlogik/Syntax/LogicSubformulas | | x | `Logic.Syntax.SubFormula` | [`LogicTasks.Syntax.SubTreeSet`](src/LogicTasks/Syntax/SubTreeSet.hs), [`Tasks.SubTree.Quiz`](src/Tasks/SubTree/Quiz.hs) |
| Aussagenlogik/Syntax/LogicTreeToFormula | | x | `Logic.Syntax.TreeToFormula` | [`LogicTasks.Syntax.TreeToFormula`](src/LogicTasks/Syntax/TreeToFormula.hs), [`Tasks.TreeToFormula.Quiz`](src/Tasks/TreeToFormula/Quiz.hs) |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFillGaps | x | x | `Logic.Semantics.FillGaps` | [`LogicTasks.Semantics.Fill`](src/LogicTasks/Semantics/Fill.hs) |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableChooseForFormula | x | x | `Logic.Semantics.ChooseTable` | [`LogicTasks.Semantics.Pick`](src/LogicTasks/Semantics/Pick.hs) |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableFindMistakes | x | x | `Logic.Semantics.FindMistakes` | [`LogicTasks.Semantics.Decide`](src/LogicTasks/Semantics/Decide.hs) |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMaxterm | x | x | `Logic.Semantics.MaxTerm` | [`LogicTasks.Semantics.Max`](src/LogicTasks/Semantics/Max.hs) |
| Aussagenlogik/Semantik/Wahrheitstabellen/TruthTableMinTerm | x | x | `Logic.Semantics.MinTerm` | [`LogicTasks.Semantics.Min`](src/LogicTasks/Semantics/Min.hs) |
| Aussagenlogik/Semantik/Resolution/LogicResolutionStep | x | x | `Logic.Semantics.ResolutionStep` | [`LogicTasks.Semantics.Step`](src/LogicTasks/Semantics/Step.hs) |
| Aussagenlogik/Semantik/Resolution/LogicResolutionComplete | x | x | `Logic.Semantics.ResolutionFull` | [`LogicTasks.Semantics.Resolve`](src/LogicTasks/Semantics/Resolve.hs) |
| Aussagenlogik/Semantik/Resolution/PrologResolutionStep | x | x | `Logic.Semantics.ResolutionStepProlog` | [`LogicTasks.Semantics.Prolog`](src/LogicTasks/Semantics/Prolog.hs) |

## Testing a module

You can use the `testModule` function in order to test a module. A sample call looks like this:

```text
$ stack repl
ghci> testModule (Just AutoLeijen) German (genResInst dResConf) LogicTasks.Semantics.Resolve.description LogicTasks.Semantics.Resolve.partialGrade LogicTasks.Semantics.Resolve.completeGrade parser
```

This specific call tests the `Resolve` module (found in `src/LogicTasks/Semantics/Resolve.hs`). The output looks like this:

```text
Betrachten Sie die folgende Formel in KNF:>>>> <¬D ∧ (¬A ∨ D) ∧ (A ∨ D)> <<<<

Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten.
Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind: (Erste Klausel, Zweite Klausel, Resolvente)
Beachten Sie dabei die folgenden möglichen Schreibweisen:
>>>>Negation: <-, ~, nicht> <<<<

>>>>Nicht-leere Klausel: <{ ... }> <<<<

>>>>Leere Klausel: <{ }> <<<<

Optional können Sie Klauseln auch durch Nummern ersetzen.
Klauseln aus der Formel sind bereits ihrer Reihenfolge nach nummeriert. (erste Klausel = 1, zweite Klausel = 2, ...).
Neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen.
>>>>Ein Lösungsversuch könnte beispielsweise so aussehen:  <[(1, 2, {A, nicht B} = 5), (4, 5, { })]> <<<<

Just ()
[({-D},{-A,D},{-A})]
---- Input ----
[({-D},{-A,D},{-A})]
---- Prettified Input ----
[({-D},{-A,D},{-A})]
---- Partial ----
1. Schritt verwendet nur existierende Indizes?
>>>> <Ja.> <<<<

1. Schritt vergibt keinen Index wiederholt?
>>>> <Ja.> <<<<

Genutzte Literale kommen in Formel vor?
>>>> <Ja.> <<<<

Alle Schritte sind gültig?
>>>> <Ja.> <<<<

Letzter Schritt leitet die leere Klausel ab?
>>>> <Nein.> <<<<

Nothing
!!! The following would not be printed in Autotool !!!
---- Complete ----
Lösung ist korrekt?
>>>> <Nein.> <<<<

Nothing
```

In more detail:

- We passed `Just (AutoLeijen)` to format the input with the specified pretty printer. Other options are: `Nothing`, `Just (AutoHughesPJ)` or `Just (Manual f)` where f is of type `a -> String`.
- We passed `German` to print the german version of the task. The other option would be `English`.
- We then passed the generator for creating an instance of the specified module. Must be of type `Gen a`.
- Furthermore, we pass the function that prints the task description. This is usually `SomeModulePath.description`.
- Next, we pass the function that checks the input for syntax errors. This is usually `SomeModulePath.partialGrade`.
- Then, we pass the function that checks the input for semantic errors. This is usually `SomeModulePath.completeGrade`.
- Lastly, we pass a parser that allows us to parse the users input. This is usually just `parser`. Must be of type `Parser b`, if you define one yourself.
