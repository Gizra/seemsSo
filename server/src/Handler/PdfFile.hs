{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

getPdfFileR :: PdfFileId -> Handler Html
getPdfFileR pdfId = do
    pdf <- runDB $ get404 pdfId
    defaultLayout $(widgetFile "pdf-download")

postPdfFileCreateR :: Handler Html
postPdfFileCreateR = do
    ((result, widget), enctype) <- runFormPost uploadForm
    case result of
        FormSuccess (file, created)
           -- TODO: check if pdf already exists
           -- save to pdf directory
         -> do
            filename <- writeToServer file
            pdfId <- runDB $ insert $ PdfFile filename created
            setMessage "PDF saved"
            redirect $ PdfFileR pdfId
        _ -> do
            setMessage "Something went wrong"
            redirect PdfFileCreateR
