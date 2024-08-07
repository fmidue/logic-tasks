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
