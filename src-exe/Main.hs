{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Control.Lens
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types (emptyObject)
import qualified Data.Aeson as A
import Data.Aeson.Lens
import Development.Shake
import Development.Shake.Forward (forwardOptions, shakeArgsForward)
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath ((</>), dropExtension, dropDirectory1)
import Slick
import GHC.Generics (Generic)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar
import qualified Text.Sass as SASS

currentDate :: IO (Integer, Int, Int) -- Year, Month, day
currentDate = Calendar.toGregorian . Clock.utctDay <$> Clock.getCurrentTime

year :: Getter UTCTime Integer
year = to $ (^. _1) . Calendar.toGregorian . Clock.utctDay

outputFolder :: FilePath
outputFolder = "build/"

outputIndexFile :: FilePath
outputIndexFile = outputFolder </> "index.html"

indexTemplate :: FilePath
indexTemplate = "site/templates/index.mustache"

baseTemplate :: FilePath
baseTemplate = "site/templates/base.mustache"

postTemplate :: FilePath
postTemplate = "site/templates/post.mustache"

aboutPage :: FilePath
aboutPage = "site/about.html"

newtype IndexInfo = IndexInfo
	{ posts :: [Post]
	} deriving (Generic, Show, FromJSON, ToJSON)

data Post = Post
	{ title :: T.Text
	, content :: T.Text
	, url :: T.Text
	} deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Given a list of posts build the index
buildIndex :: UTCTime -> [Post] -> Action ()
buildIndex utcTime posts' = do
	indexT <- compileTemplate' indexTemplate
	let indexInfo = IndexInfo {posts = posts'}

	let	indexHTML = T.unpack $ substitute indexT (A.toJSON indexInfo)

	baseT <- compileTemplate' baseTemplate

	let withContent = _Object . at "content" ?~ A.String (T.pack indexHTML)
	let withYear = _Object . at "year" ?~ A.String (T.pack (show (utcTime ^. year)))

	writeFile' outputIndexFile (T.unpack $ substitute baseT (withYear $ withContent emptyObject))

-- | Find and build all posts
buildPosts :: UTCTime -> Action [Post]
buildPosts utcTime = do
	pPaths <- getDirectoryFiles "." ["site/posts//*.md"]
	forP
		(filter (not . ("site/posts//CHANGELOG.md" ?==)) pPaths)
		(buildPost utcTime)

-- | Load a post, process metadata, write it to output, then return the post
-- object.
-- Detects changes to either post content or template.
buildPost :: UTCTime -> FilePath -> Action Post
buildPost utcTime srcPath = do
	liftIO . putStrLn $ "Building: " <> srcPath
	postContent <- readFile' srcPath

	-- load post content and metadata as JSON blob
	postData <- markdownToHTML . T.pack $ postContent
	let postUrl = (<> ".html") . T.pack . dropDirectory1 $ dropExtension srcPath
	let withPostUrl = _Object . at "url" ?~ A.String postUrl
	let withYear = _Object . at "year" ?~ A.String (T.pack (show (utcTime ^. year)))


	-- Add additional metadata we'be been abale to compute
	let fullPostData = (withYear . withPostUrl) postData

	-- The post template
	postT <- compileTemplate' postTemplate
	let postHTML = substitute postT fullPostData

	let withPostContent = _Object . at "content" ?~ A.String postHTML

	let finalTitle = case postData ^? (_Object . at "title" ) of
		Just (Just (A.String x)) -> x
		_ -> error $ "Missing title at file: " ++ srcPath

	let withTitle = _Object . at "title" ?~ A.String finalTitle

	template <- compileTemplate' baseTemplate
	writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template
		(withPostContent $ withTitle emptyObject)
	--Convert the metadata into a Post object
	convert fullPostData

-- | Builds the about page
buildAbout :: UTCTime -> Action ()
buildAbout utcTime = do
	aboutContent <- readFile' aboutPage

	let aboutUrl = "about.html"
	let withAboutUrl = _Object . at "url" ?~ A.String aboutUrl
	let withAboutContent = _Object . at "content" ?~ A.String (T.pack aboutContent)
	let withYear = _Object . at "year" ?~ A.String (T.pack (show (utcTime ^. year)))

	let fullAboutData = (withYear . withAboutContent . withAboutUrl) emptyObject

	template <- compileTemplate' baseTemplate
	writeFile' (outputFolder </> T.unpack aboutUrl) . T.unpack $ substitute template fullAboutData
	return ()

buildStyles :: Action ()
buildStyles = do
	styleFiles <- getDirectoryFiles "./site/css" ["*.scss"]

	void $ forP styleFiles $ \scssFilePath -> do
		liftIO . putStrLn $ "[SCSS] Processing: " <> scssFilePath
		result <- liftIO ( SASS.compileFile ("site/css" </> scssFilePath) SASS.def :: SASS.ExtendedResult )
		case result of
			Left sassError -> liftIO (SASS.errorText sassError) >>= liftIO . print
			Right extendedResult -> writeFile'
				(outputFolder </> "css" </> (dropExtension scssFilePath) <> ".css")
				(SASS.resultString extendedResult)


-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
	filePaths <- getDirectoryFiles "./site/" ["images//*"]
	void $ forP filePaths $ \filePath ->
		copyFileChanged ("site" </> filePath) (outputFolder </> filePath)
	
	-- copy favicon
	void $ copyFileChanged "favicon/favicon.svg" (outputFolder </> "favicon.svg")

-- | Specific build rules for the Shake system
-- defines workflow to build the website
buildRules :: Action ()
buildRules = do
	currentTime <- liftIO Clock.getCurrentTime
	allPosts <- buildPosts currentTime
	buildIndex currentTime allPosts
	buildStyles
	copyStaticFiles
	buildAbout currentTime

main :: IO ()
main = do
	let commandLineOptions = forwardOptions $ shakeOptions
		{ shakeVerbosity = Chatty 
		, shakeLintInside = ["site"]}
	shakeArgsForward commandLineOptions buildRules
