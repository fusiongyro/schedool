import Text.XHtml.Strict

layout :: (HTML a, HTML b) => a -> b -> Html
layout title' body' = heading +++ body << body'
    where
      heading = header << [ thetitle << title', js "jQuery.js" ]

js :: String -> Html
js source = script ! [src source] << ""

