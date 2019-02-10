{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Performer (
  perform
) where

import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy.Char8      (unpack)
import           Data.Conduit                    (runConduit, (.|))
import           Data.Conduit.Combinators
import           Data.String
import qualified Data.Text                       as TextS
import qualified Data.Text.Lazy                  as TextL
import           Data.Time.Format
import           Network.HTTP.Simple
import           Network.Mail.Client.Gmail
import           Network.Mail.Mime
import           NewsletterConfig
import           Task
import           Text.Blaze
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet                     (shamlet)
import           Text.RSS.Conduit.Parse.Simple
import           Text.RSS.Types
import           Text.XML.Stream.Parse
import           URI.ByteString

rssToHtml :: RssDocument a -> Task -> (TextS.Text, TextL.Text)
rssToHtml doc task = (getSubject doc, getBody doc)
  where
    getSubject doc = channelTitle doc
    getBody doc = fromString $ renderHtml $ emailTemplate doc task

performTask :: Task -> NewsletterConfig -> IO (Maybe String)
performTask task config = do
  rssDocM <- runResourceT $ runConduit $
    httpSource (fromString $ sourceRss task) getResponseBody .| (parseBytes def) .| rssDocument
  case rssDocM of
    Nothing -> return $ Just "Unable to parse rss document"
    Just rssDoc -> do
      let (subject, text) = rssToHtml rssDoc task
      sendGmail
        (username config)
        (password config)
        (fromString $ show $ addressEmail $ from config)
        [(targetEmail task)]
        []
        []
        subject
        text
        []
        5000000
      return Nothing

perform :: [Task] -> NewsletterConfig -> IO [Maybe String]
perform [] config = return []
perform (task : tasks) config = do
  result <- (performTask task config)
  results <- perform tasks config
  return (result : results)

filterItemsByTask :: Task -> RssItem a -> Bool
filterItemsByTask (Task startDate endDate _ _) item  =
  case itemPubDate item of
    Nothing      -> False
    Just pubDate -> pubDate >= startDate && pubDate <= endDate

emailTemplate doc task = [shamlet|
  <h1>
    <a href=#{link}>#{title}
  <section>
    $forall item <- items
      <article>
        <h2>
          <a href=#{showItemLink item}>#{itemTitle item}
        <p>^{preEscapedToMarkup $ itemDescription item}
        <footer>
          <span style="color: SteelBlue">
            <time datetime=#{articleTime "%Y-%m-%d %H:%M:%S" $ itemPubDate item}>#{articleTime  "%e %B, %y" $ itemPubDate item}
          <p>Author: #{itemAuthor item}
|]
  where
    title = channelTitle doc
    link = withRssURI (\uri -> unpack $ toLazyByteString $ serializeURIRef uri) (channelLink doc)
    articleTime fmt Nothing        = ""
    articleTime fmt (Just pubDate) = formatTime defaultTimeLocale fmt pubDate
    showItemLink item = case (itemLink item) of
      Nothing -> "#"
      Just link -> withRssURI (\uri -> unpack $ toLazyByteString $ serializeURIRef uri) link
    items = Prelude.filter (filterItemsByTask task) (channelItems doc)
