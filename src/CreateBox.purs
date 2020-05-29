module CreateBox where

import Prelude

import Effect (Effect)
import Web.DOM.Element as DOM
import Web.DOM.Document as DOM

foreign import setStyleProp :: String -> String -> DOM.Element -> Effect Boolean
createBoxElement :: String -> DOM.Document -> Effect DOM.Element
createBoxElement id document = do
    boxEl <- DOM.createElement "div" document
    DOM.setId id boxEl
    DOM.setClassName "box" boxEl
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl
