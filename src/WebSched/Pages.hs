import Text.XHtml.Strict

layout :: (HTML a, HTML b) => a -> b -> Html
layout title' body' = heading +++ body << body'
    where
      heading = header << [ thetitle << title', js "jQuery.js" ]

-- | Helper method for declaring JavaScript includes.
js :: String -> Html
js source = script ! [src source] << ""


-- Right now I'm wondering if it would be useful to have an HTML monad,
-- something like a Writer monad that lets you record JS and CSS includes even
-- while rendering regular text...
