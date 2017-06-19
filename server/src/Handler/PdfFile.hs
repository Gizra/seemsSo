{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.PdfFile where

import Import
import Utils.Form (renderSematnicUiDivs)

uploadDirectory :: FilePath
uploadDirectory = "static/item-pdf"

uploadForm :: Form (FileInfo, UTCTime)
uploadForm =
    renderSematnicUiDivs $
    (,) <$> fileAFormReq "PDF file" <*> lift (liftIO getCurrentTime)

writeToServer :: FileInfo -> Handler FilePath
writeToServer file = do
    let filename = unpack $ fileName file
    let path = pdfFilePath filename
    liftIO $ fileMove file path
    return filename

pdfFilePath :: String -> FilePath
pdfFilePath f = uploadDirectory </> f

getPdfFileR :: Handler Html
getPdfFileR = do
    ((_, widget), enctype) <- runFormPost uploadForm
    pdfs <- runDB $ selectList [PdfFileFilename !=. ""] [Desc PdfFileDate]
    mmsg <- getMessage
    defaultLayout $ do
        [whamlet|$newline never
$maybe msg <- mmsg
   <div .message>
       <div .container>
           #{msg}
<div .container>
   <div .row>
       <h2>
           Upload new pdf
       <div .form-actions>
           <form method=post enctype=#{enctype}>
               ^{widget}
               <input .btn type=submit value="Upload">
       $if not $ null pdfs
           <table .table>
               <tr>
                   <th>
                       PDF
                   <th>
                       Decription
                   <th>
                       Uploaded
                   <th>
                       Action
               $forall Entity pdfId pdf <- pdfs
                   <tr>
                       <td>
                           <a href=#{pdfFilePath $ pdfFileFilename pdf}>
                               #{pdfFileFilename pdf}
                       <td>
                           #{show $ pdfFileDate pdf}
                       <td>
                           <a href=# .delete data-img-url=@{PdfFileR}>
                               delete

|]

postPdfFileR :: Handler Html
postPdfFileR = do
    ((result, widget), enctype) <- runFormPost uploadForm
    case result of
        FormSuccess (file, date)
           -- TODO: check if pdf already exists
           -- save to pdf directory
         -> do
            filename <- writeToServer file
            _ <- runDB $ insert $ PdfFile filename date
            setMessage "PDF saved"
            redirect PdfFileR
        _ -> do
            setMessage "Something went wrong"
            redirect PdfFileR
