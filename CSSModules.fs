﻿module CSSModules

let inline internal (-) m p = m -- s p

module Background =
  let inline internal (-) p = (-) background p
  let noRepeat  = (-) Background.NoRepeat
  let center    = (-) Background.Center

module Border =
  let inline internal (-) p = (-) border p
  let none       = (-) Border.None
  let solid      = (-) Border.Solid
  let set  w t c =
    border -/ [w; s t; c]

module Clear =
  let inline internal (-) p = (-) clear p
  let both = (-) Clear.Both

module Color =
  let inline internal (-) p = (-) color p
  let white   = (-) Color.White
  let black   = (-) Color.Black
  let red     = (-) Color.Red

module Cursor =
  let inline internal (-) p = (-) cursor p
  let pointer = (-) Cursor.Pointer

module Display =
  let inline internal (-) p = (-) display p
  let block     = (-) Display.Block
  let inline_   = (-) Display.Inline

module Float =
  let inline internal (-) p = (-) "float" p
  let left  = (-) Float.Left
  let right = (-) Float.Right

module FontVariant =
  let inline internal (-) p = (-) fontVariant p
  let smallCaps = (-) FontVariant.SmallCaps

module FontWeight =
  let inline internal (-) p = fontWeight -- s p
  let bold = (-) FontWeight.Bold

module LineHeight =
  let inline internal (-) p = lineHeight -- s p
  let normal = (-) LineHeight.Normal

module Overflow =
  let inline internal (-) p = (-) overflow p
  let auto      = (-) Overflow.Auto
  let hidden    = (-) Overflow.Hidden

module Position =
  let inline internal (-) p = (-) position p
  let absolute = (-) Position.Absolute
  let relative = (-) Position.Relative

module TextAlign =
  let inline internal (-) p = (-) textAlign p
  let center    = (-) TextAlign.Center
  let left      = (-) TextAlign.Left
  let right     = (-)TextAlign.Right

module TextDecoration =
  let inline internal (-) p = (-) textDecoration p
  let none = (-) TextDecoration.None

module WhiteSpace =
  let inline internal (-) p = (-) whiteSpace p
  let nowrap = (-) WhiteSpace.NoWrap
