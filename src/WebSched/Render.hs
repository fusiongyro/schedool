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
    RS a `mappend` RS b = RS $ a `mappend` b
    mempty = RS (mempty, mempty, mempty)

-- | This is the basic extra-monadic function, which adds a link to this CSS
--   file to the HTML output.
addCSS css       = tell $ RS (csslink, mempty, mempty)
    where
      csslink    = thelink ! [ rel "stylesheet"
                             , href css
                             , thetype "text/css" ] << ""
addJavaScript js = tell $ RS (mempty, jslink, mempty)
    where
      jslink     = script ! [ lang "javascript"
                            , src  js
                            , thetype "text/javascript" ] << ""
addHtml html  = tell $ RS (mempty, mempty, html)

runRender :: RenderState -> String
runRender (RS (css, js, body)) = prettyHtml $ header (css +++ js) +++ body

renderRS = runRender . execWriter

layout = do
  addCSS "main.css"
  addJavaScript "jQuery.js"
  addHtml $ p << "Hello, world!"
  addHtml $ p << "This is my first monad."
  addHtml $ p << "So far, I think it's working out OK."