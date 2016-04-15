{-# LANGUAGE OverloadedStrings #-}
module OpenDoc.ODS where

import qualified Data.ByteString.Lazy as BL
import Codec.Archive.Zip
import Text.XML (parseLBS, def)
import Text.XML.Cursor (fromDocument, element, child, (>=>), Cursor, content, attribute)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T



data ODS = ODS { sheets :: [Sheet] }
data Sheet = Sheet { cells :: [[Cell]] }
data CellContent = TextCell String
data Cell = Cell { cellText :: String }


odsFromFile :: FilePath -> IO ODS
odsFromFile file = do
  bytes <- BL.readFile file
  return $ odsFromBytestring bytes

unsafePrint :: String -> a -> a
unsafePrint str val =
    unsafePerformIO (do
                      putStrLn str
                      return val)


-- nodeShortShow (NodeContent txt) = "NodeContent ("++show txt++")"
-- nodeShortShow (NodeComment txt) = "NodeComment ("++show txt++")"
-- nodeShortShow (NodeInstruction _) = "NodeInstruction"
-- nodeShortShow (NodeElement (Element {elementName=name})) = "<"++show name++">"

-- showXMLNode :: Node -> String
-- showXMLNode (NodeElement elemt) = showXMLElement elemt
-- showXMLNode (NodeComment txt) = "NodeComment ("++show txt++")"
-- showXMLNode (NodeContent txt) = "NodeContent ("++show txt++")"
-- showXMLNode (NodeInstruction _) = "NodeInstruction"

-- showXMLElement :: Element -> String
-- showXMLElement xml = TL.unpack text
--     where text = renderText def $ Document { documentPrologue=emptyPrologue,
--                                              documentRoot=xml,
--                                              documentEpilogue=[] }
--           emptyPrologue = Prologue { prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[] }


-- cursorShortShow cursor = nodeShortShow $ node cursor

parseCell :: Cursor -> [Cell]
parseCell cursor =
    let texts = (child >=> element "{urn:oasis:names:tc:opendocument:xmlns:text:1.0}p"
                           >=> child >=> content) cursor in
    let text = case texts of [t] -> t; [] -> ""; _ -> error "more than one text element in cell" in
    let repetitions = case attribute "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}number-columns-repeated" cursor of
                        [n] -> read (T.unpack n)
                        [] -> 1
                        _ -> error "more than one number-columns-repeated argument" in
    let cell = Cell { cellText=T.unpack text } in
    replicate repetitions cell

parseRow :: Cursor -> [Cell]
parseRow cursor =
    let cells' = (child >=> element "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table-cell") cursor in
    concatMap parseCell cells'

parseSheet :: Cursor -> Sheet
parseSheet cursor =
    let rows = (child >=> element "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table-row") cursor in
    Sheet {cells = map parseRow rows}



odsFromBytestring :: BL.ByteString -> ODS
odsFromBytestring bytes =
    let archive = toArchive bytes in
    let Just entry = findEntryByPath "content.xml" archive in
    let content' = fromEntry entry in
    let Right xml = parseLBS def content' in
    let cursor = fromDocument xml in
    let sheet_cursor = (element "{urn:oasis:names:tc:opendocument:xmlns:office:1.0}document-content"
                        >=> child >=> element "{urn:oasis:names:tc:opendocument:xmlns:office:1.0}body"
                        >=> child >=> element "{urn:oasis:names:tc:opendocument:xmlns:office:1.0}spreadsheet"
                        >=> child >=> element "{urn:oasis:names:tc:opendocument:xmlns:table:1.0}table") cursor in
    let sheets' = map parseSheet sheet_cursor in
    ODS { sheets = sheets' }


