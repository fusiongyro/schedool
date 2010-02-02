{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Render module abstracts a certain flavor of HTML templating. In my
--   own PHP code I often found that I needed a way to add JavaScript, CSS
--   or other stuff to the header or to modify the title when I was deep
--   inside some other script mucking around with some code for display. It
--   occurred to me that this kind of thing would be more than adequately
--   handled by using a Writer monad; I don't need to see what's in the
--   header, I just need to ensure that the body can add stuff to it.
--
--   Conveniently, the Text.XHtml module defines Html as a monoid, so this was
--   trivial work.
--
--   To use, make your HTML combinators as usual. They can then be injected
--   into this monad using 'addHtml'. You can also call the 'addCSS' and
--   'addJavaScript' functions to store some references to CSS and JavaScript
--   you use. I may at some point make these more intelligent, either by
--   minifying them or removing duplicates. For the time being, avoid
--   duplicates manually.
--
--   Your toplevel function can use the 'layout' function as the basic
--   scaffold for your page. Feel free to make new combinators based on its
--   example or purely with Text.XHtml. You can then use the 'renderM'
--   function to convert layouts to actual HTML text.
module WebSched.Render (CSS
              ,JavaScript
              ,module Text.XHtml.Strict
              ,addCSS
              ,addJavaScript
              ,addHtml
              ,renderM
              ,layout)
    where

import Control.Monad.Writer
import Data.Monoid
import Text.XHtml.Strict

-- | An alias to elucidate the RenderState type below.
type CSS         = Html
-- | Another alias.
type JavaScript  = Html

-- | This is the type I'll be using inside the Writer monad below. The
--   thinking is that this type will represent sort of a rendering context, so
--   that user combinators can either be "pure" and just output HTML for the
--   body, or they can do a mixture of creating HTML and recording CSS and
--   JavaScript links to be sent along with everything else.
data RenderState = RS (CSS, JavaScript, Html)
                   deriving (Show)

-- | By making RenderState a monoid, I can avoid doing the actual book keeping
--   for the Writer monad, which will just rely on these two methods here to
--   handle my state for me.
instance Monoid RenderState where
    mempty              = RS (mempty, mempty, mempty)
    RS a `mappend` RS b = RS $ a `mappend` b

-- | This is the monad which encapsulates the Writer RenderState monad, to make our
--   public interface a little cleaner.
newtype RenderM a = RenderM { runRM :: Writer RenderState a }
    deriving (Monad)

-- | This is a basic combinator which extracts some commonality between links
--   and script tags.
linkHelper top attrs = top ! attrs << ""

-- | Sticks another CSS link at the top of the document.
addCSS :: String -> RenderM ()
addCSS css       = RenderM $ tell $ RS (csslink, mempty, mempty)
    where
      csslink    = linkHelper thelink [rel "stylesheet", href css, thetype "text/css"]

-- | Sticks another JavaScript link at the top of the document.
addJavaScript :: String -> RenderM ()
addJavaScript js = RenderM $ tell $ RS (mempty, jslink, mempty)
    where
      jslink     = linkHelper script [lang "javascript", src js, thetype mimetype ]
      mimetype   = "text/javascript"

-- | Concatenate some HTML to the bottom of the body of this document.
addHtml :: Html -> RenderM ()
addHtml html  = RenderM $ tell $ RS (mempty, mempty, html)

-- | Internally, this function handles the Text.XHtml interface and produces
--   text for output.
runRender :: RenderState -> String
runRender (RS (css, js, body)) = prettyHtml $ header (css +++ js) +++ body

-- | Monad entry point. Use to cause your calls to layout and add* to produce
--   a string you can send someone.
renderM :: RenderM a -> String
renderM = runRender . execWriter . runRM

-- | The basic page layout for this application.
layout :: RenderM ()
layout = do
  addHtml $ p << "Hello, world!"
  addJavaScript "jQuery.js"
  addHtml $ p << "This is my first monad."
  addCSS "main.css"
  addHtml $ p << "So far, I think it's working out OK."
