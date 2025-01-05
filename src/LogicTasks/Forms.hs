{-# language FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language RecordWildCards #-}
{-# language TypeApplications #-}

module LogicTasks.Forms where


import Control.Monad.Reader             (reader)
import Data.List                        (transpose)
import Data.Text                        (Text, pack)
import FlexTask.FormUtil (
  addAttribute,
  addCss,
  addNameAndCssClass,
  )
import FlexTask.YesodConfig             (Rendered)
import Yesod (
  cassius,
  fvInput,
  mopt,
  textField,
  whamlet,
  )



tableForm :: Int -> Int -> [Text] -> [Text] -> Rendered
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
    pure ( [headerName, inputName]
         , [whamlet|
              #{extra}
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

    headerClass = "header"
    inputClass = "tableInput"
    headerName = "headers"
    inputName = "cells"

    css = [cassius|
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
