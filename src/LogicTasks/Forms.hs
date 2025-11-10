{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module LogicTasks.Forms (
  tableForm,
  fullResolutionForm
  ) where


import Control.Monad.Reader             (reader)
import Data.List                        (transpose, sort)
import Data.Maybe                       (isNothing)
import Data.Text                        (Text, pack)
import FlexTask.Generic.Form (
  formifyComponentsFlat,
  single,
  Hidden(..)
  )
import FlexTask.FormUtil (
  addAttribute,
  addCss,
  addCssClass,
  addNameAndCssClass,
  readOnly,
  )
import FlexTask.YesodConfig (
  FlexForm,
  Rendered,
  Widget,
  pattern Singular,
  )
import Yesod (
  RenderMessage(..),
  cassius,
  fieldSettingsLabel,
  fvInput,
  mopt,
  textField,
  whamlet,
  )

import Formula.Types (Clause)



tableForm :: Int -> Int -> [Text] -> [Text] -> Rendered Widget
tableForm emptyColumns rows staticStart staticEnd =
  addCss css $ reader $ \extra -> do
    let headerList = replicate emptyColumns headerName
        cellList = replicate rows inputName
        totalColumns = emptyColumns + length staticStart + length staticEnd
    headersRes <- traverse (field 1 headerClass) headerList
    columnsRes <- traverse
      (\num -> traverse (field num inputClass) cellList)
      [2..totalColumns+1]
    let tableHeaders = map snd headersRes
        tableRows = transpose $ map (map snd) columnsRes
    pure ( Singular headerName ++ Singular inputName
         , [whamlet|
              #{extra}
              <div .#{containerClass}>
                <table>
                  <tr>
                    $forall startHeader <- staticStart
                      <th>#{startHeader}
                    $forall inputHeader <- tableHeaders
                      <th>^{fvInput inputHeader}
                    $forall endHeader <- staticEnd
                      <th>#{endHeader}
                  $forall row <- tableRows
                    <tr>
                      $forall input <- row
                        <td>^{fvInput input}|]
         )
  where
    tabIndex i = addAttribute ("tabindex", pack $ show @Int i)
    field i name cl = mopt textField
      (tabIndex i $ addNameAndCssClass cl name)
      Nothing

    containerClass :: String
    containerClass = "truth-table"
    headerClass = "header"
    inputClass = "tableInput"
    headerName = "headers"
    inputName = "cells"

    css = [cassius|
      .#{containerClass}
        .#{headerClass}
          width: 100%
          text-align: center
          padding-top: 10px
          padding-bottom: 10px

        .#{inputClass}
          width: 100%
          text-align: center

        th, td
          border: 1px solid black
          border-collapse: collapse
          text-align: center

        table tr th:nth-child(-n+4)
          width: 2.5%

        table tr td:nth-child(-n+4)
          height: 2%

        table tr th:nth-child(n+5)
          width: 3%

        table tr th:nth-child(n+9)
          width: 6.5%

        table tr th:nth-child(n+14)
          width: 10%
    |]


data Label = Step | First | Second | Resolvent


instance RenderMessage FlexForm Label where
  renderMessage _   ("en":_) Step      = "Step"
  renderMessage _   _        Step      = "Schritt"
  renderMessage _   ("en":_) First     = "First Clause"
  renderMessage _   _        First     = "Erste Klausel"
  renderMessage _   ("en":_) Second    = "Second Clause"
  renderMessage _   _        Second    = "Zweite Klausel"
  renderMessage _   _        Resolvent = "Resolvent"



{- |
Form for full resolution with multiple input fields.
Allows for prefilling rows by supplying a list of values.
The list may be shorter than the amount of steps.
In that case, everything afterwards is left empty. (Filled up with triples of Nothing)
-}
fullResolutionForm
  :: Int -- ^ amount of input rows
  -> [Clause] -- ^ pool of clauses for resolution
  -> (Clause -> String) -- ^ how to display the clauses
  -> [(Maybe String, Maybe String, Maybe String)] -- ^ list of values to prefill rows with
  -> Rendered Widget
fullResolutionForm steps clauses howToShow prefilledFields = addCss css $ do
  forms <- traverse
            (\(x,(val1,val2,val3)) -> formifyComponentsFlat
              (Just (val1 ,val2, val3, Hidden x))
              [ fSettings val1 First
              , fSettings val2 Second
              , fSettings val3 Resolvent
              , single $ fieldSettingsLabel $ "= " <> pack (show x)
              ]
            )
            $ zip rowIndices rowDefaults
  reader $ \extra -> do
    (fields,formRows) <- unzip <$> sequence forms
    pure (concat fields, html extra formRows)
  where
    indexZip xs = zip xs [1 :: Int ..]
    clauseStrings = map howToShow $ sort clauses
    firstFreeIndex = length clauseStrings +1
    rowIndices = [firstFreeIndex .. firstFreeIndex + steps]
    rowDefaults = prefilledFields ++ replicate
      (length rowIndices - length prefilledFields)
      (Nothing,Nothing,Nothing)

    containerClass  :: String
    containerClass = "full-resolution-form"
    inputClass = "clause-input"
    fSettings x = single . addCssClass inputClass .
      (if isNothing x then id else readOnly) . fieldSettingsLabel

    html token widgets = [whamlet|
      #{token}
      <div .#{containerClass}>
        $forall (clause, givenIndex) <- indexZip clauseStrings
          <span .flex-form-span .disabled-clauses>
            <input type=text value="#{clause}" .#{inputClass} disabled>
          <span .flex-form-span>
            <p .static-num>
              = #{givenIndex}
        $forall (inputs,step) <- indexZip widgets
          <p .step-counter>
            _{Step} #{step}:
          $forall widget <- inputs
            ^{widget}
    |]

    css = [cassius|
      .#{containerClass}
          display:grid
          grid-template-columns: 0.6fr 1.4fr 1.4fr 1.4fr 0.2fr
          justify-items:center
          align-items:end
          column-gap: 0.5em
          row-gap: 2em

        .#{inputClass}
          width:70%
          margin-left:1em

        p, label, .static-num
          font-weight:bold

        .flex-form-span
          width:75%
          label
            display:block

        .disabled-clauses
          grid-column-start: 4
          align-self: start

        .flex-form-span:has(input[type="hidden"])
          display:flex
          align-items: center
    |]
